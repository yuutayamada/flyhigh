;;; flyhigh-nimsuggest.el --- on the fly syntax highlight -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Yuta Yamada

;; Author: Yuta Yamada <cokesboy<at>gmail.com>
;; Keywords:
;; Package-Requires: ((emacs "26.1") (nim-mode "0.4.2"))

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

;;; Code:

(require 'flyhigh)
(require 'nim-mode)

;;;###autoload
(when (version<= "26" (number-to-string emacs-major-version))
  (add-hook 'nimsuggest-mode-hook 'flyhigh-nimsuggest-setup)
  (add-hook 'prog-mode-hook 'flyhigh-mode))

(defconst nimsuggest-symkinds
  ;; From ast.nim, some of them aren't used as completion maybe...
  '((skUnknown      . 'error)
    (skConditional  . 'error)
    (skDynLib       . 'error);
    (skParam        . font-lock-type-face)
    (skGenericParam . font-lock-type-face)
    (skTemp         . font-lock-type-face)
    (skModule       . font-lock-type-face)
    (skType         . font-lock-type-face)
    (skVar          . font-lock-variable-name-face)
    (skLet          . font-lock-variable-name-face)
    (skConst        . font-lock-constant-face)
    (skResult       . font-lock-variable-name-face)
    (skProc         . font-lock-function-name-face)
    (skFunc         . font-lock-function-name-face)
    (skMethod       . font-lock-function-name-face)
    (skIterator     . font-lock-function-name-face)
    (skConverter    . font-lock-function-name-face)
    (skMacro        . font-lock-function-name-face)
    (skTemplate     . font-lock-function-name-face);
    (skField        . font-lock-builtin-face)
    (skEnumField    . font-lock-builtin-face)
    (skForVar       . font-lock-builtin-face)
    (skLabel        . font-lock-builtin-face)
    (skStub         . font-lock-builtin-face);
    (skPackage      . font-lock-builtin-face)
    (skAlias        . font-lock-builtin-face)))

(defun nimsuggest--flyhigh-get-face (hint)
  ""
  (condition-case err
      (or (alist-get (intern hint) nimsuggest-symkinds)
          'error)
    (error err)))

;;;###autoload
(defun flyhigh-nimsuggest-setup()
  ""
  (if flyhigh-mode
      (add-hook  'flyhigh-diagnostic-functions 'flyhigh-nimsuggest nil t)
    (remove-hook 'flyhigh-diagnostic-functions 'flyhigh-nimsuggest t)))

(defun flyhigh--visible-end ()
  ""
  (- (window-end) (window-start)))

(defun flyhigh--nimsuggest-region (buf line col)
  "Calculate beg and end for FILE, BUF, LINE, and COL.
Workaround for https://github.com/nim-lang/nim-mode/issues/183."
  (with-current-buffer buf
    (let ((line (min (max line 1)
                     (line-number-at-pos (point-max) 'absolute))))
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (forward-char col)
        (bounds-of-thing-at-point 'symbol)))))

(defun flyhigh--nimsuggest-filter (highlights buffer)
  ""
  (cl-loop with (sl . el) = (flyhigh--offset (flyhigh--line (window-start))
                                             (flyhigh--line (window-end)))
           for (_ sk _ file _ line col _ _) in highlights

           if (and (<= sl line) (<= line el)
                   (eq (get-file-buffer file) buffer))
           collect (list line col (nimsuggest--flyhigh-get-face sk))))

(require 'deferred)
(defun flyhigh-nimsuggest* (highlight buffer report-fn)
  ""
  (deferred:$
    (deferred:next
      (lambda ()
        (nim-log "flyhigh started: %d" (length highlight))
        (flyhigh--nimsuggest-filter highlight buffer)))
    (deferred:nextc it
      (lambda (hs)
        (nim-log "flyhigh parsing hs: %d" (length hs))
        (cl-loop for (line col face) in hs
                 for (beg . end) = (flyhigh--nimsuggest-region buffer line col)
                 if (and beg end) ; TODO
                 collect (list buffer beg end face))))
    (deferred:nextc it
      (lambda (point-based-hs)
        (nim-log "flyhigh make diagnostic objects: %d" (length point-based-hs))
        (cl-loop for (buffer beg end face) in point-based-hs
                 collect (flyhigh-make-diagnostic buffer beg end face))))
    (deferred:nextc it
      (lambda (report-action)
        (nim-log "flyhigh started report function: %d" (length report-action))
        (funcall report-fn report-action)))
    (deferred:error it
      (lambda (err)
        (nim-log "Wrong input : %s" err)))))

(defun flyhigh-nimsuggest (report-fn &rest _args)
  "A Flymake backend for Nim language using Flyhigh.
See `flymake-diagnostic-functions' for REPORT-FN and ARGS."
  (let ((buffer (current-buffer)))
    (nimsuggest--call-epc
     'highlight
     (lambda (highlight)
       (flyhigh-nimsuggest* highlight buffer report-fn)))))

(provide 'flyhigh-nimsuggest)
;;; flyhigh-nimsuggest.el ends here
