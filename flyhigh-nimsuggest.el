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
(require 'nim-suggest)

;;;###autoload
(when (version<= "26" (number-to-string emacs-major-version))
  (add-hook 'nimsuggest-mode-hook 'flyhigh-nimsuggest-setup)
  (add-hook 'prog-mode-hook 'flyhigh-mode))


(defmacro nimsuggest-high-face (name inherit)
  `(defface ,name
     '((t (:inherit ,inherit)))
     ,(format "Font Lock face for %s." (symbol-name name))
     :group 'nim))

(nimsuggest-high-face nim-unknown-face font-lock-preprocessor-face)
(nimsuggest-high-face nim-conditional-face font-lock-preprocessor-face)

(nimsuggest-high-face nim-param-face font-lock-function-name-face)
(nimsuggest-high-face nim-generic-param-face font-lock-function-name-face)
(nimsuggest-high-face nim-let-face font-lock-variable-name-face)
(nimsuggest-high-face nim-module-face font-lock-function-name-face)

(nimsuggest-high-face nim-proc-face font-lock-function-name-face)
(nimsuggest-high-face nim-func-face font-lock-function-name-face)
(nimsuggest-high-face nim-method-face font-lock-function-name-face)
(nimsuggest-high-face nim-iterator-face font-lock-function-name-face)
(nimsuggest-high-face nim-converter-face font-lock-function-name-face)
(nimsuggest-high-face nim-macro-face font-lock-function-name-face)
(nimsuggest-high-face nim-template-face font-lock-function-name-face)
(nimsuggest-high-face nim-field-face font-lock-builtin-face)
(nimsuggest-high-face nim-enum-face font-lock-builtin-face)
(nimsuggest-high-face nim-forvar-face font-lock-builtin-face)
(nimsuggest-high-face nim-label-face font-lock-builtin-face)
(nimsuggest-high-face nim-stub-face font-lock-builtin-face)
(nimsuggest-high-face nim-package-face font-lock-builtin-face)
(nimsuggest-high-face nim-alias-face font-lock-builtin-face)

(defconst nimsuggest-symkinds
  ;; From ast.nim, some of them aren't used as completion maybe...
  '((skUnknown      . nim-unknown-face)
    ;; unknown symbol: used for parsing assembler blocks
    ;; and first phase symbol lookup in generics
    (skConditional  . nim-conditional-face)
    ;; symbol for the preprocessor (may become obsolete)
    (skDynLib       . error);
    ;; symbol represents a dynamic library; this is used
    ;; internally; it does not exist in Nim code
    (skParam        . nim-param-face) ; a parameter
    (skGenericParam . nim-generic-param-face) ; a generic parameter; eq in ``proc x[eq=`==`]()``
    (skTemp         . font-lock-type-face) ; a temporary variable (introduced by compiler)
    (skModule       . nim-module-face) ; module identifier
    (skType         . font-lock-type-face) ; a type
    (skVar          . font-lock-variable-name-face) ; a variable
    (skLet          . nim-let-face) ; a 'let' symbol
    (skConst        . font-lock-constant-face)      ; a constant
    (skResult       . font-lock-variable-name-face) ; special 'result' variable
    (skProc         . nim-proc-face) ; a proc
    (skFunc         . nim-func-face) ; a func
    (skMethod       . nim-method-face) ; a method
    (skIterator     . nim-iterator-face) ; an iterator
    (skConverter    . nim-convertor-face) ; a converter
    (skMacro        . nim-macro-face) ; a macro
    (skTemplate     . nim-template-face) ; a template
    (skField        . nim-field-face) ;  a field in a record or object
    (skEnumField    . nim-enum-face) ; an identifier in an enum
    (skForVar       . nim-forvar-face) ; a for loop variable
    (skLabel        . nim-label-face) ; a label (for block statement)
    (skStub         . nim-stub-face);; symbol is a stub and not yet loaded from the ROD file
    (skPackage      . nim-package-face) ; symbol is a package (used for canonicalization)
    (skAlias        . nim-alias-face) ; an alias (needs to be resolved immediately)
    ))

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

(defun flyhigh--nimsuggest-filter (highlight buffer)
  "Return highlight candidates from HIGHLIGHT on the BUFFER.
Unnecessary lines will be removed (outside of
`flyhigh-window-bounds-with-offset')."
  (cl-loop with (sl . el) = (flyhigh-window-bounds-with-offset)
           for (_ sk _ file _ line col _ _) in highlight
           if (and (<= sl line) (<= line el)
                   (eq (get-file-buffer file) buffer))
           collect (list line col (nimsuggest--flyhigh-get-face sk))))

(defun flyhigh-nimsuggest-core (highlight buffer report-fn)
  ""
  (deferred:$
    (deferred:next
      (lambda ()
        (nim-log "flyhigh started: %d" (length highlight))
        (flyhigh--nimsuggest-filter highlight buffer)))
    (deferred:nextc it
      (lambda (line-based-hs)
        (nim-log "flyhigh parsing hs: %d" (length line-based-hs))
        (cl-loop for (line col face) in line-based-hs
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
        (nim-log "flyhigh something wrong: %s" err)))))

(defun flyhigh-nimsuggest (report-fn &rest _args)
  "A Flyhigh backend for Nim language using Flyhigh.
See `flyhigh-diagnostic-functions' for REPORT-FN and ARGS."
  (let ((buffer (current-buffer)))
    (nimsuggest--call-epc
     'highlight
     (lambda (highlight)
       (flyhigh-nimsuggest-core highlight buffer report-fn)))))

(provide 'flyhigh-nimsuggest)
;;; flyhigh-nimsuggest.el ends here
