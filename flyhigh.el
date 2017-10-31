;;; flyhigh.el --- idk  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Yuta Yamada

;; Author: Yuta Yamada <cokesboy<at>gmail.com>
;; Package-Requires: ((emacs "26.1") (dash "20170810"))
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Flyhigh is a minor Emacs mode performing on-the-fly syntax highlight.
;;
;; The docstrings of below variable is relevant to understanding how
;; Flyhigh works for both the user and the backend programmer:
;;
;; * `flyhigh-diagnostic-functions'


;; Large portion of this package was stolen from flymake.el from Emacs
;; 26 to make on the fly syntax highlight package.  Essentially what it
;; does is similar and flymake.el's design was really nice.


;; (un)Implementation/idea note:
;;
;; * Only work if Emacs is not active (e.g, idle)
;; * Only apply specific range/lines in buffer.  This is important when your
;;   file is too too big.
;; * Discard highlight data if user started editing current query before apply
;; * The highlight should be executed partially in the idle time.
;; * Use `defmethod', so other people might use this library same way.
;; * What is the best way to apply highlight? Overlay?
;; * cache the un-finished highlight and doesn't make a query if you
;;   already have the highlight when user doesn't changed anything,
;;   but cursor moved.

;;; Code:

(require 'cl-lib)
(require 'thingatpt) ; end-of-thing
(require 'warnings) ; warning-numeric-level, display-warning
(require 'compile) ; for some faces
;; when-let*, if-let*, hash-table-keys, hash-table-values:
(eval-when-compile (require 'subr-x))
(require 'dash)
(require 'deferred)

(defgroup flyhigh nil
  "Universal on-the-fly syntax highlighter."
  :group 'tools)

;; Probably we don't need this
(defcustom flyhigh-start-syntax-highlight-on-newline nil
  "Start syntax check if newline char was added/removed from the buffer."
  :type 'boolean)

(defcustom flyhigh-no-changes-timeout 10 ; sec
  "Time to wait after last change before automatically checking buffer.
If nil, never start checking buffer automatically like this."
  :type 'number)

(defcustom flyhigh-invisible-hl-timeout 1000
  "Time to idle wait for invisible highlight by msec."
  :type 'number)

(defcustom flyhigh-highlight-interval 10
  "Time to idle wait for group of highlight by msec."
  :type 'number)

(defcustom flyhigh-offset 0
  "Line offset; increase offset more highlight more display lock..."
  :type 'number)

(defcustom flyhigh-division 5
  "wip"
  :type 'number)

(defcustom flyhigh-start-on-flyhigh-mode t
  "Start syntax check when `flyhigh-mode' is enabled.
Specifically, start it when the buffer is actually displayed."
  :type 'boolean)

(defcustom flyhigh-wrap-around t
  "If non-nil, moving to errors wraps around buffer boundaries."
  :type 'boolean)

(defvar-local flyhigh-timer nil
  "Timer for starting syntax highlight")

(defvar-local flyhigh-check-start-time nil
  "Time at which syntax highlight was started.")

(defvar-local flyhigh-window-bounds nil)

(defun flyhigh-window-bounds-with-offset ()
  "Return cons of (beg . end)."
  (flyhigh--offset (flyhigh--line (window-start))
                   (flyhigh--line (window-end))))

(defun flyhigh--offset (start-line end-line)
  "Calculate START-LINE and END-LINE of offset."
  (let* ((max-line (line-number-at-pos (point-max) 'absolute))
         (m (- start-line flyhigh-offset))
         (p (+ end-line flyhigh-offset)))
    (cons (min (max m 1) max-line) (min (max p 1) max-line))))

(defun flyhigh--log-1 (level sublog msg &rest args)
  "Do actual work for `flyhigh-log'."
  (let (;; never popup the log buffer
        (warning-minimum-level :emergency)
        (warning-type-format
         (format " [%s %s]"
                 (or sublog 'flyhigh)
                 (current-buffer))))
    (display-warning (list 'flyhigh sublog)
                     (apply #'format-message msg args)
                     (if (numberp level)
                         (or (nth level
                                  '(:emergency :error :warning :debug :debug) )
                             :error)
                       level)
                     "*Flyhigh log*")))

(defun flyhigh-switch-to-log-buffer ()
  "Go to the *Flyhigh log* buffer."
  (interactive)
  (switch-to-buffer "*Flyhigh log*"))

;;;###autoload
(defmacro flyhigh-log (level msg &rest args)
  "Log, at level LEVEL, the message MSG formatted with ARGS.
LEVEL is passed to `display-warning', which is used to display
the warning.  If this form is included in a byte-compiled file,
the generated warning contains an indication of the file that
generated it."
  (let* ((compile-file (and (boundp 'byte-compile-current-file)
                            (symbol-value 'byte-compile-current-file)))
         (sublog (if (and
                      compile-file
                      (not load-file-name))
                     (intern
                      (file-name-nondirectory
                       (file-name-sans-extension compile-file))))))
    `(flyhigh--log-1 ,level ',sublog ,msg ,@args)))

(defun flyhigh-error (text &rest args)
  "Format TEXT with ARGS and signal an error for Flyhigh."
  (let ((msg (apply #'format-message text args)))
    (flyhigh-log :error msg)
    (error (concat "[Flyhigh] " msg))))

(cl-defstruct (flyhigh--diag
               (:constructor flyhigh--diag-make))
  buffer beg end face backend)

;;;###autoload
(defun flyhigh-make-diagnostic (buffer
                                beg
                                end
                                face)
  "Make a Flyhigh diagnostic for BUFFER's region from BEG to END.
FACE is the face of the region."
  (flyhigh--diag-make :buffer buffer :beg beg :end end :face face))

;;;###autoload
(defun flyhigh-diagnostics (&optional beg end)
  "Get Flyhigh diagnostics in region determined by BEG and END.

If neither BEG or END is supplied, use the whole buffer,
otherwise if BEG is non-nil and END is nil, consider only
diagnostics at BEG."
  (mapcar (lambda (ov) (overlay-get ov 'flyhigh-diagnostic))
          (flyhigh--overlays :beg beg :end end)))

(defmacro flyhigh--diag-accessor (public internal thing)
  "Make PUBLIC an alias for INTERNAL, add doc using THING."
  `(defsubst ,public (diag)
     ,(format "Get Flyhigh diagnostic DIAG's %s." (symbol-name thing))
     (,internal diag)))

(flyhigh--diag-accessor flyhigh-diagnostic-buffer flyhigh--diag-buffer buffer)
(flyhigh--diag-accessor flyhigh-diagnostic-beg flyhigh--diag-beg beg)
(flyhigh--diag-accessor flyhigh-diagnostic-end flyhigh--diag-end end)
(flyhigh--diag-accessor flyhigh-diagnostic-backend flyhigh--diag-backend backend)
(flyhigh--diag-accessor flyhigh-diagnostic-face flyhigh--diag-face face)

(cl-defun flyhigh--overlays (&key beg end filter compare key)
  "Get flyhigh-related overlays.
If BEG is non-nil and END is nil, consider only `overlays-at'
BEG. Otherwise consider `overlays-in' the region comprised by BEG
and END, defaulting to the whole buffer.  Remove all that do not
verify FILTER, a function, and sort them by COMPARE (using KEY)."
  (save-restriction
    (widen)
    (let ((ovs (cl-remove-if-not
                (lambda (ov)
                  (and (overlay-get ov 'flyhigh-diagnostic)
                       (or (not filter)
                           (funcall filter ov))))
                (if (and beg (null end))
                    (overlays-at beg t)
                  (overlays-in (or beg (point-min))
                               (or end (point-max)))))))
      (if compare
          (cl-sort ovs compare :key (or key
                                        #'identity))
        ovs))))

(defun flyhigh-delete-own-overlays (&optional filter)
  "Delete all Flyhigh overlays in BUFFER."
  (mapc #'delete-overlay (flyhigh--overlays :filter filter)))

;; (defface flyhigh-error
;;   '((((supports :underline (:style wave)))
;;      :underline (:style wave :color "Red1"))
;;     (t
;;      :inherit error))
;;   "Face used for marking error regions."
;;   :version "24.4")

;; (defface flyhigh-warning
;;   '((((supports :underline (:style wave)))
;;      :underline (:style wave :color "deep sky blue"))
;;     (t
;;      :inherit warning))
;;   "Face used for marking warning regions."
;;   :version "24.4")

;; (defface flyhigh-note
;;   '((((supports :underline (:style wave)))
;;      :underline (:style wave :color "yellow green"))
;;     (t
;;      :inherit warning))
;;   "Face used for marking note regions."
;;   :version "26.1")

(defvar flyhigh-diagnostic-functions nil
  "Special hook of Flyhigh backends that check a buffer.

The functions in this hook diagnose problems in a buffer's
contents and provide information to the Flyhigh user interface
about where and how to annotate problems diagnosed in a buffer.

Each backend function must be prepared to accept an arbitrary
number of arguments:

* the first argument is always REPORT-FN, a callback function
  detailed below;

* the remaining arguments are keyword-value pairs in the
  form (:KEY VALUE :KEY2 VALUE2...).  Currently, Flyhigh provides
  no such arguments, but backend functions must be prepared to
  accept and possibly ignore any number of them.

Whenever Flyhigh or the user decides to re-check the buffer,
backend functions are called as detailed above and are expected
to initiate this check, but aren't required to complete it before
exiting: if the computation involved is expensive, especially for
large buffers, that task can be scheduled for the future using
asynchronous processes or other asynchronous mechanisms.

In any case, backend functions are expected to return quickly or
signal an error, in which case the backend is disabled.  Flyhigh
will not try disabled backends again for any future checks of
this buffer.  Certain commands, like turning `flyhigh-mode' off
and on again, reset the list of disabled backends.

If the function returns, Flyhigh considers the backend to be
\"running\". If it has not done so already, the backend is
expected to call the function REPORT-FN with a single argument
REPORT-ACTION also followed by an optional list of keyword-value
pairs in the form (:REPORT-KEY VALUE :REPORT-KEY2 VALUE2...).

Currently accepted values for REPORT-ACTION are:

* A (possibly empty) list of diagnostic objects created with
  `flyhigh-make-diagnostic', causing Flyhigh to annotate the
  buffer with this information.

  A backend may call REPORT-FN repeatedly in this manner, but
  only until Flyhigh considers that the most recently requested
  buffer check is now obsolete because, say, buffer contents have
  changed in the meantime. The backend is only given notice of
  this via a renewed call to the backend function. Thus, to
  prevent making obsolete reports and wasting resources, backend
  functions should first cancel any ongoing processing from
  previous calls.

* The symbol `:panic', signaling that the backend has encountered
  an exceptional situation and should be disabled.

Currently accepted REPORT-KEY arguments are:

* `:explanation' value should give user-readable details of
  the situation encountered, if any.

* `:force': value should be a boolean suggesting that Flyhigh
  consider the report even if it was somehow unexpected.")



(defun flyhigh--highlight-line (diagnostic)
  "Highlight buffer with info in DIAGNOSTIC."
  (when-let* ((ov (make-overlay
                   (flyhigh--diag-beg diagnostic)
                   (flyhigh--diag-end diagnostic))))
    (cl-flet ((default-maybe
                (prop value)
                (unless (or (plist-member (overlay-properties ov) prop)
                            (let ((cat (overlay-get ov 'flyhigh-category)))
                              (and cat (plist-member (symbol-plist cat) prop))))
                  (overlay-put ov prop value))))
      (default-maybe 'face (flyhigh--diag-face diagnostic)))

    ;; Some properties can't be overridden.
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'flyhigh-diagnostic diagnostic)))

(defvar-local flyhigh--backend-state nil
  "Buffer-local hash table of a Flyhigh backend's state.
The keys to this hash table are functions as found in
`flyhigh-diagnostic-functions'. The values are structures
of the type `flyhigh--backend-state', with these slots:

`running', a symbol to keep track of a backend's replies via its
REPORT-FN argument. A backend is running if this key is
present. If nil, Flyhigh isn't expecting any replies from the
backend.

`diags', a (possibly empty) list of recent diagnostic objects
created by the backend with `flyhigh-make-diagnostic'.

`reported-p', a boolean indicating if the backend has replied
since it last was contacted.

`disabled', a string with the explanation for a previous
exceptional situation reported by the backend, nil if the
backend is operating normally.")

(cl-defstruct (flyhigh--backend-state
               (:constructor flyhigh--make-backend-state))
  running reported-p disabled diags)

(defmacro flyhigh--with-backend-state (backend state-var &rest body)
  "Bind BACKEND's STATE-VAR to its state, run BODY."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  (let ((b (make-symbol "b")))
    `(let* ((,b ,backend)
            (,state-var
             (or (gethash ,b flyhigh--backend-state)
                 (puthash ,b (flyhigh--make-backend-state)
                          flyhigh--backend-state))))
       ,@body)))

(defun flyhigh-is-running ()
  "Tell if Flyhigh has running backends in this buffer"
  (flyhigh-running-backends))

(cl-defun flyhigh--handle-report (backend token report-action
                                          &key explanation force
                                          &allow-other-keys)
  "Handle reports from BACKEND identified by TOKEN.
BACKEND, REPORT-ACTION and EXPLANATION, and FORCE conform to the calling
convention described in `flyhigh-diagnostic-functions' (which
see). Optional FORCE says to handle a report even if TOKEN was
not expected."
  (let* ((state (gethash backend flyhigh--backend-state))
         (first-report (not (flyhigh--backend-state-reported-p state))))
    (setf (flyhigh--backend-state-reported-p state) t)
    (let (expected-token)
      (cond
       ((null state)
        (flyhigh-error
         "Unexpected report from unknown backend %s" backend))
       ((flyhigh--backend-state-disabled state)
        (flyhigh-error
         "Unexpected report from disabled backend %s" backend))
       ((progn
          (setq expected-token (flyhigh--backend-state-running state))
          (null expected-token))
        ;; should never happen
        (flyhigh-error "Unexpected report from stopped backend %s" backend))
       ((and (not (eq expected-token token))
             (not force))
        (flyhigh-error "Obsolete report from backend %s with explanation %s"
                       backend explanation))
       ((eq :panic report-action)
        (flyhigh--disable-backend backend explanation))
       ((not (listp report-action))
        (flyhigh--disable-backend backend
                                  (format "Unknown action %S" report-action))
        (flyhigh-error "Expected report, but got unknown key %s" report-action))
       (t
        (if (and first-report
                 (buffer-modified-p (current-buffer)))
            (flyhigh--handle-report-1 report-action t backend state)
          (flyhigh--handle-report-1 report-action nil backend state t)))))))

(defun flyhigh--handle-report-1 (new-hl flush backend state &optional reuse-ov)
  ""
  ;; Trying to make pseudo async handler using `deferred' package.
  (deferred:$
    ;; TODO: remove some overlays from `new-hl' that already
    ;; highlighted.
    (deferred:next
      ;; TODO: document what it does
      (lambda ()
        (when flush
          (save-restriction
            (widen)
            (let ((ws (window-start))
                  (we (window-end)))
              (flyhigh-delete-own-overlays
               (lambda (ov)
                 (and
                  (<= ws (overlay-start ov) (overlay-end ov) we)
                  (eq backend
                      (flyhigh--diag-backend
                       (overlay-get ov 'flyhigh-diagnostic)))))))))))

    (deferred:nextc it
      (lambda (_)
        (if (not reuse-ov)
            new-hl
          (cl-remove-if
           (lambda (diag) (member diag (flyhigh--backend-state-diags state)))
           new-hl))))

    (deferred:nextc it
      (lambda (hl) (flyhigh--sort-by-visible hl)))

    (deferred:nextc it
      (lambda (hl)
        (flyhigh--split-by flyhigh-division hl)))

    (deferred:nextc it
      (deferred:lambda (hl)
        (flyhigh--apply-highlight-lines (car hl) backend state)
        (when hl
          (setf hl (cdr hl))
          (deferred:nextc
            (deferred:$
              (deferred:wait-idle flyhigh-highlight-interval)
              (deferred:nextc it (lambda (_) hl)))
            self))))

    (deferred:nextc it
      (lambda (_)
        (when flyhigh-check-start-time
          (flyhigh-log :debug "backend %s reported %d diagnostics in %.2f second(s)"
                       backend
                       (length new-hl)
                       (- (float-time) flyhigh-check-start-time)))
        (when (and (get-buffer (flyhigh--diagnostics-buffer-name))
                   (get-buffer-window (flyhigh--diagnostics-buffer-name))
                   (null (cl-set-difference (flyhigh-running-backends)
                                            (flyhigh-reporting-backends))))
          (flyhigh-show-diagnostics-buffer))))))

(defun flyhigh--sort-by-visible (diags)
  "Return curated DIAGS.
Visible is fast, invisible is later."
  (cl-loop with ws = (window-start)
           with we = (window-end)
           for diag in diags
           for beg = (flyhigh--diag-beg diag)
           for end = (flyhigh--diag-end diag)
           if (<= ws beg end we)
           collect diag into visible-hl
           else collect diag into invisible-hl
           finally return (append visible-hl invisible-hl)))

(defun flyhigh--split-by (num hl)
  "Split HL of highlight by NUM."
  (cl-loop for i from 0 to (length hl) by num
           for x = (-take num hl) then (-take num (nthcdr (1+ i) hl))
           if x collect x))

(defun flyhigh--apply-highlight-lines (diags backend state)
  ""
  (when diags
    (save-restriction
      (widen)
      (mapc (lambda (diag) (flyhigh--apply-highlight-line diag backend))
            diags))
    (setf (flyhigh--backend-state-diags state)
          (append diags (flyhigh--backend-state-diags state)))))

(defun flyhigh--apply-highlight-line (diag backend)
  ""
  (condition-case err
      (flyhigh--highlight-line diag)
    (error
     (message "flyhigh error: %s\n diag %s"
              (error-message-string err)
              diag)))
  (setf (flyhigh--diag-backend diag) backend))

(defun flyhigh-make-report-fn (backend &optional token)
  "Make a suitable anonymous report function for BACKEND.
BACKEND is used to help Flyhigh distinguish different diagnostic
sources.  If provided, TOKEN helps Flyhigh distinguish between
different runs of the same backend."
  (let ((buffer (current-buffer)))
    (lambda (&rest args)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (apply #'flyhigh--handle-report backend token args))))))

(defun flyhigh--collect (fn &optional message-prefix)
  "Collect Flyhigh backends matching FN.
If MESSAGE-PREFIX, echo a message using that prefix"
  (unless flyhigh--backend-state
    (user-error "Flyhigh is not initialized"))
  (let (retval)
    (maphash (lambda (backend state)
               (when (funcall fn state) (push backend retval)))
             flyhigh--backend-state)
    (when message-prefix
      (message "%s%s"
               message-prefix
               (mapconcat (lambda (s) (format "%s" s))
                          retval ", ")))
    retval))

(defun flyhigh-running-backends ()
  "Compute running Flyhigh backends in current buffer."
  (interactive)
  (flyhigh--collect #'flyhigh--backend-state-running
                    (and (called-interactively-p 'interactive)
                         "Running backends: ")))

(defun flyhigh-disabled-backends ()
  "Compute disabled Flyhigh backends in current buffer."
  (interactive)
  (flyhigh--collect #'flyhigh--backend-state-disabled
                    (and (called-interactively-p 'interactive)
                         "Disabled backends: ")))

(defun flyhigh-reporting-backends ()
  "Compute reporting Flyhigh backends in current buffer."
  (interactive)
  (flyhigh--collect #'flyhigh--backend-state-reported-p
                    (and (called-interactively-p 'interactive)
                         "Reporting backends: ")))

(defun flyhigh--disable-backend (backend &optional explanation)
  "Disable BACKEND because EXPLANATION.
If it is running also stop it."
  (flyhigh-log :warning "Disabling backend %s because %s" backend explanation)
  (flyhigh--with-backend-state backend state
    (setf (flyhigh--backend-state-running state) nil
          (flyhigh--backend-state-disabled state) explanation
          (flyhigh--backend-state-reported-p state) t)))

(defun flyhigh--run-backend (backend)
  "Run the backend BACKEND, reenabling if necessary."
  (flyhigh-log :debug "Running backend %s" backend)
  (let ((run-token (cl-gensym "backend-token")))
    (flyhigh--with-backend-state backend state
      (setf (flyhigh--backend-state-running state) run-token
            (flyhigh--backend-state-disabled state) nil
            (flyhigh--backend-state-diags state) nil
            (flyhigh--backend-state-reported-p state) nil))
    ;; FIXME: Should use `condition-case-unless-debug' here, but don't
    ;; for two reasons: (1) that won't let me catch errors from inside
    ;; `ert-deftest' where `debug-on-error' appears to be always
    ;; t. (2) In cases where the user is debugging elisp somewhere
    ;; else, and using flyhigh, the presence of a frequently
    ;; misbehaving backend in the global hook (most likely the legacy
    ;; backend) will trigger an annoying backtrace.
    ;;
    (condition-case err
        (funcall backend
                 (flyhigh-make-report-fn backend run-token))
      (error
       (flyhigh--disable-backend backend err)))))

(defun flyhigh-start (&optional deferred force)
  "Start a syntax check for the current buffer.
DEFERRED is a list of symbols designating conditions to wait for
before actually starting the check.  If it is nil (the list is
empty), start it immediately, else defer the check to when those
conditions are met.  Currently recognized conditions are
`post-command', for waiting until the current command is over,
`on-display', for waiting until the buffer is actually displayed
in a window.  If DEFERRED is t, wait for all known conditions.

With optional FORCE run even disabled backends.

Interactively, with a prefix arg, FORCE is t."
  (interactive (list nil current-prefix-arg))

  (setq-local flyhigh-window-bounds
              (flyhigh--offset (flyhigh--line (window-start))
                               (flyhigh--line (window-end))))

  (let ((deferred (if (eq t deferred)
                      '(post-command on-display)
                    deferred))
        (buffer (current-buffer)))
    (cl-labels
        ((start-post-command
          ()
          (remove-hook 'post-command-hook #'start-post-command
                       nil)
          (with-current-buffer buffer
            (flyhigh-start (remove 'post-command deferred) force)))
         (start-on-display
          ()
          (remove-hook 'window-configuration-change-hook #'start-on-display
                       'local)
          (flyhigh-start (remove 'on-display deferred) force)))
      (cond ((and (memq 'post-command deferred)
                  this-command)
             (add-hook 'post-command-hook
                       #'start-post-command
                       'append nil))
            ((and (memq 'on-display deferred)
                  (not (get-buffer-window (current-buffer))))
             (add-hook 'window-configuration-change-hook
                       #'start-on-display
                       'append 'local))
            (t
             (setq flyhigh-check-start-time (float-time))
             (run-hook-wrapped
              'flyhigh-diagnostic-functions
              (lambda (backend)
                (cond
                 ((and (not force)
                       (flyhigh--with-backend-state backend state
                         (flyhigh--backend-state-disabled state)))
                  (flyhigh-log :debug "Backend %s is disabled, not starting"
                               backend))
                 (t
                  (flyhigh--run-backend backend)))
                nil)))))))

(defvar flyhigh-mode-map
  (let ((map (make-sparse-keymap))) map)
  "Keymap for `flyhigh-mode'")

;;;###autoload
(define-minor-mode flyhigh-mode
  "Toggle Flyhigh mode on or off.
With a prefix argument ARG, enable Flyhigh mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

Flyhigh is an Emacs minor mode for on-the-fly syntax checking.
Flyhigh collects diagnostic information from multiple sources,
called backends, and visually annotates the buffer with the
results.

Flyhigh performs these checks while the user is editing.  The
customization variables `flyhigh-start-on-flyhigh-mode',
`flyhigh-no-changes-timeout' and
`flyhigh-start-syntax-highlight-on-newline' determine the exact
circumstances whereupon Flyhigh decides to initiate a check of
the buffer.

The commands `flyhigh-goto-next-error' and
`flyhigh-goto-prev-error' can be used to navigate among Flyhigh
diagnostics annotated in the buffer.

The visual appearance of each type of diagnostic can be changed
in the variable `flyhigh-diagnostic-types-alist'.

Activation or deactivation of backends used by Flyhigh in each
buffer happens via the special hook
`flyhigh-diagnostic-functions'.

Some backends may take longer than others to respond or complete,
and some may decide to disable themselves if they are not
suitable for the current buffer. The commands
`flyhigh-running-backends', `flyhigh-disabled-backends' and
`flyhigh-reporting-backends' summarize the situation, as does the
special *Flyhigh log* buffer."  :group 'flyhigh :lighter
  flyhigh--mode-line-format :keymap flyhigh-mode-map
  (cond
   ;; Turning the mode ON.
   (flyhigh-mode
    (add-hook 'after-change-functions 'flyhigh-after-change-function nil t)
    (add-hook 'after-save-hook 'flyhigh-after-save-hook nil t)
    (add-hook 'kill-buffer-hook 'flyhigh-kill-buffer-hook nil t)
    ;; I added
    (add-hook 'post-command-hook 'flyhigh-cursor-move-hook nil t)

    (setq flyhigh--backend-state (make-hash-table))

    (when flyhigh-start-on-flyhigh-mode (flyhigh-start t)))

   ;; Turning the mode OFF.
   (t
    (remove-hook 'after-change-functions 'flyhigh-after-change-function t)
    (remove-hook 'after-save-hook 'flyhigh-after-save-hook t)
    (remove-hook 'kill-buffer-hook 'flyhigh-kill-buffer-hook t)
    ;; I added
    (remove-hook 'post-command-hook 'flyhigh-cursor-move-hook t)

    ;;+(remove-hook 'find-file-hook (function flyhigh-find-file-hook) t)

    (flyhigh-delete-own-overlays)

    (when flyhigh-timer
      (cancel-timer flyhigh-timer)
      (setq flyhigh-timer nil)))))

(defun flyhigh--schedule-timer-maybe ()
  "(Re)schedule an idle timer for checking the buffer.
Do it only if `flyhigh-no-changes-timeout' is non-nil."
  (when flyhigh-timer (cancel-timer flyhigh-timer))
  (when flyhigh-no-changes-timeout
    (setq
     flyhigh-timer
     (run-with-idle-timer
      (seconds-to-time flyhigh-no-changes-timeout)
      nil
      (lambda (buffer)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (and flyhigh-mode
                       flyhigh-no-changes-timeout)
        (flyhigh-log
               :debug "starting syntax check after idle for %s seconds"
               flyhigh-no-changes-timeout)
        (flyhigh-start t))
            (setq flyhigh-timer nil))))
      (current-buffer)))))

;;;###autoload
(defun flyhigh-mode-on ()
  "Turn Flyhigh mode on."
  (flyhigh-mode 1))

;;;###autoload
(defun flyhigh-mode-off ()
  "Turn Flyhigh mode off."
  (flyhigh-mode 0))

(make-obsolete 'flyhigh-mode-on 'flyhigh-mode "26.1")
(make-obsolete 'flyhigh-mode-off 'flyhigh-mode "26.1")

(defun flyhigh-after-change-function (start stop _len)
  "Start syntax check for current buffer if it isn't already running."
  (let((new-text (buffer-substring start stop)))
    (when (and flyhigh-start-syntax-highlight-on-newline (equal new-text "\n"))
      (flyhigh-log :debug "starting syntax check as new-line has been seen")
      (flyhigh-start t))
    (flyhigh--schedule-timer-maybe)))

(defun flyhigh--line (pos)
  "Return line of POS."
  (save-excursion
    (goto-char pos)
    (line-number-at-pos pos 'absolute)))

(defun flyhigh-cursor-move-hook ()
  "Add a timer to re-dispaly for cursor move."
  (let ((sline (flyhigh--line (window-start)))
        (eline (flyhigh--line (window-end))))
    (when (not (and (<= (car flyhigh-window-bounds) sline)
                    (>= (cdr flyhigh-window-bounds) eline)))
      (flyhigh--schedule-timer-maybe))))

(defun flyhigh-after-save-hook ()
  (when flyhigh-mode
    (flyhigh-log :debug "starting syntax check as buffer was saved")
    (flyhigh-start t)))

(defun flyhigh-kill-buffer-hook ()
  (when flyhigh-timer
    (cancel-timer flyhigh-timer)
    (setq flyhigh-timer nil)))

(defun flyhigh-find-file-hook ()
  (unless (or flyhigh-mode
              (null flyhigh-diagnostic-functions))
    (flyhigh-mode)
    (flyhigh-log :warning "Turned on in `flyhigh-find-file-hook'")))


;;; Mode-line and menu

;;; Diagnostics buffer

(defvar-local flyhigh--diagnostics-buffer-source nil)

(defvar flyhigh-diagnostics-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'flyhigh-goto-diagnostic)
    (define-key map (kbd "SPC") 'flyhigh-show-diagnostic)
    map))

(defun flyhigh-show-diagnostic (pos &optional other-window)
  "Show location of diagnostic at POS."
  (interactive (list (point) t))
  (let* ((id (or (tabulated-list-get-id pos)
                 (user-error "Nothing at point")))
         (diag (plist-get id :diagnostic)))
    (with-current-buffer (flyhigh--diag-buffer diag)
      (with-selected-window
          (display-buffer (current-buffer) other-window)
        (goto-char (flyhigh--diag-beg diag))
        (pulse-momentary-highlight-region (flyhigh--diag-beg diag)
                                          (flyhigh--diag-end diag)
                                          'highlight))
      (current-buffer))))

(defun flyhigh-goto-diagnostic (pos)
  "Show location of diagnostic at POS.
POS can be a buffer position or a button"
  (interactive "d")
  (pop-to-buffer
   (flyhigh-show-diagnostic (if (button-type pos) (button-start pos) pos))))

(defun flyhigh--diagnostics-buffer-entries ()
  nil
  ;; (with-current-buffer flyhigh--diagnostics-buffer-source
  ;;   (cl-loop for diag in (flyhigh-diagnostics)
  ;;            for (line . col) =
  ;;            (save-excursion
  ;;              (goto-char (flyhigh--diag-beg diag))
  ;;              (cons (line-number-at-pos)
  ;;                    (- (point)
  ;;                       (line-beginning-position))))
  ;;            for type = (flyhigh--diag-type diag)
  ;;            collect
  ;;            (list (list :diagnostic diag
  ;;                        :line line
  ;;                        :severity (flyhigh--lookup-type-property
  ;;                                   type
  ;;                                   'severity (warning-numeric-level :error)))
  ;;                  `[,(format "%s" line)
  ;;                    ,(format "%s" col)
  ;;                    ,(propertize (format "%s" type)
  ;;                                 'face (flyhigh--lookup-type-property
  ;;                                        type 'mode-line-face 'flyhigh-error))
  ;;                    (,(format "%s" (flyhigh--diag-text diag))
  ;;                     mouse-face highlight
  ;;                     help-echo "mouse-2: visit this diagnostic"
  ;;                     face nil
  ;;                     action flyhigh-goto-diagnostic
  ;;                     mouse-action flyhigh-goto-diagnostic)])))
  )

(define-derived-mode flyhigh-diagnostics-buffer-mode tabulated-list-mode
  "Flyhigh diagnostics"
  "A mode for listing Flyhigh diagnostics."
  (setq tabulated-list-format
        `[("Line" 5 (lambda (l1 l2)
                      (< (plist-get (car l1) :line)
                         (plist-get (car l2) :line)))
           :right-align t)
          ("Col" 3 nil :right-align t)
          ("Type" 8 (lambda (l1 l2)
                      (< (plist-get (car l1) :severity)
                         (plist-get (car l2) :severity))))
          ("Message" 0 t)])
  (setq tabulated-list-entries
        'flyhigh--diagnostics-buffer-entries)
  (tabulated-list-init-header))

(defun flyhigh--diagnostics-buffer-name ()
  (format "*Flyhigh diagnostics for %s*" (current-buffer)))

(defun flyhigh-show-diagnostics-buffer ()
  "Show a list of Flyhigh diagnostics for current buffer."
  (interactive)
  (let* ((name (flyhigh--diagnostics-buffer-name))
         (source (current-buffer))
         (target (or (get-buffer name)
                     (with-current-buffer (get-buffer-create name)
                       (flyhigh-diagnostics-buffer-mode)
                       (setq flyhigh--diagnostics-buffer-source source)
                       (current-buffer)))))
    (with-current-buffer target
      (revert-buffer)
      (display-buffer (current-buffer)))))


(provide 'flyhigh)

;;; flyhigh.el ends here
