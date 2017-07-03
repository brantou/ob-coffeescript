;;; ob-coffee.el --- org-babel functions for coffee evaluation

;; Copyright (C) Brantou

;; Author: Brantou
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating coffee-script code.

;;; Requirements:

;; node.js and coffee-script


;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("coffee" . "coffee"))

(defvar org-babel-default-header-args:coffee '()
  "Default header arguments for coffee code blocks.")

(defcustom org-babel-coffee-command "coffee"
  "Name of command used to evaluate coffee blocks."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defvar org-babel-coffee-eoe-indicator  "'org_babel_coffee_eoe'"
  "String to indicate that evaluation has completed.")

(defvar org-babel-coffee-function-wrapper
  "
fs = require('fs')
_wrapper = ->
%s

fs.writeFile('%s', '' + _wrapper()) "
  "coffee-script code to print value of body.")

(defun org-babel-execute:coffee (body params)
  "Execute a block of Coffee code with org-babel.
 This function is called by `org-babel-execute-src-block'"
  (message "executing Coffee source code block")
  (let* ((org-babel-coffee-command
          (or (cdr (assq :coffee params))
              org-babel-coffee-command))
         (session (org-babel-coffee-initiate-session
                   (cdr (assq :session params))))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
         (full-body (org-babel-expand-body:generic
                     body params (org-babel-variable-assignments:coffee params)))
         (result (org-babel-coffee-evaluate
                  session full-body result-type result-params)))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name (cdr (assq :colname-names params))
                          (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
                          (cdr (assq :rownames params))))))

(defun org-babel-coffee-evaluate
    (session body &optional result-type result-params preamble)
  "Evaluate BODY as Coffee code."
  (if session
      (org-babel-coffee-evaluate-session
       session body result-type result-params)
    (org-babel-coffee-evaluate-external-process
     body result-type result-params)))

(defun org-babel-coffee-evaluate-external-process
    (body &optional result-type result-params)
  "Evaluate BODY in external coffee process.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let ((result
         (let* ((script-file (org-babel-temp-file "coffee-script-"))
                (tmp-file (org-babel-temp-file "coffee-")))
           (with-temp-file script-file
             (insert
              (if (string= result-type "value")
                  (format org-babel-coffee-function-wrapper
                          (mapconcat
                           (lambda (line) (format "\t%s" line))
                           (split-string (org-remove-indentation (org-trim body))
                                         "[\r\n]")
                           "\n")
                          (org-babel-process-file-name tmp-file 'noquote))
                full-body)))
           (let ((eval-cmd
                   (format "%s %s"
                           org-babel-coffee-command
                           (org-babel-process-file-name script-file))))
              (pcase result-type
                (`output (org-babel-eval eval-cmd ""))
                (`value (when (org-babel-eval eval-cmd "")
                         (org-babel-eval-read-file tmp-file))))))))
    (org-babel-result-cond result-params
      result (org-babel-coffee-read result))))

(defun org-babel-coffee-evaluate-session
    (session body &optional result-type result-params)
  "Pass BODY to the Python process in SESSION.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (let ((result
         (pcase result-type
           (`output
            (let ((tmp-redir-file (org-babel-temp-file "coffee-redir-")))
              (org-babel-comint-with-output
                  (session org-babel-coffee-eoe-indicator t body)
                (comint-send-input nil t)
                (mapc
                 (lambda (line)
                   (insert (org-babel-chomp line)) (comint-send-input nil t))
                 (append
                  (list
                   (format "_stdout=console._stdout;console._stdout=fs.createWriteStream('%s');"
                                (org-babel-process-file-name tmp-redir-file 'noquote)))
                  (list body)
                  (list "console._stdout=_stdout;")
                  (list org-babel-coffee-eoe-indicator)))
                (comint-send-input nil t))
              (org-babel-eval-read-file tmp-redir-file)))
           (`value
            (let* ((tmp-file (org-babel-temp-file "coffee-")))
              (org-babel-comint-with-output
                  (session org-babel-coffee-eoe-indicator t body)
                (comint-send-input nil t)
                (mapc
                 (lambda (line)
                   (insert (org-babel-chomp line)) (comint-send-input nil t))
                 (append
                  (list body)
                  (list (format "fs.writeFile('%s', _.toString())"
                                (org-babel-process-file-name tmp-file 'noquote)))
                  (list org-babel-coffee-eoe-indicator)))
                (comint-send-input nil t))
              (org-babel-eval-read-file tmp-file))))))
    (unless (string= (substring org-babel-coffee-eoe-indicator 1 -1) result)
      (org-babel-result-cond result-params
        result
        (org-babel-coffee-read (org-trim result))))))

(defun org-babel-prep-session:coffee (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-coffee-initiate-session session))
         (var-lines (org-babel-variable-assignments:coffee params)))
    (org-babel-comint-in-buffer session
      (sit-for .5) (goto-char (point-max))
      (mapc (lambda (var)
              (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)
              (sit-for .1) (goto-char (point-max))) var-lines))
    session))

(defun org-babel-load-session:coffee (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:coffee session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

(defun org-babel-coffee-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
 Return the initialized session."
  (unless (string= session "none")
    (require 'coffee-mode)
    (let* ((buffer (get-buffer "CoffeeREPL"))
           (session-buffer
            (or buffer
                (save-window-excursion
                  (coffee-repl)
                  (current-buffer)))))
      (if (org-babel-comint-buffer-livep session-buffer)
          (progn (sit-for .25) session-buffer)
        (sit-for .5)
        (org-babel-coffee-initiate-session session)))))

(defun org-babel-coffee-var-to-coffee (var)
  "Convert an elisp var into a string of coffee source code
 specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-coffee-var-to-coffee var ", ") "]")
    (replace-regexp-in-string "\n" "\\\\n" (format "%S" var))))

(defun org-babel-variable-assignments:coffee (params)
  "Return list of Javascript statements assigning the block's variables."
  (mapcar
   (lambda (pair) (format "%s=%s"
                          (car pair) (org-babel-coffee-var-to-coffee (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-coffee-read (results)
  "Convert RESULTS into an appropriate elisp value.
If RESULTS look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (and (stringp results)
            (string-prefix-p "[" results)
            (string-suffix-p "]" results))
       (org-babel-read
        (concat "'"
                (replace-regexp-in-string
                 "\\[" "(" (replace-regexp-in-string
                            "\\]" ")" (replace-regexp-in-string
                                       ",[[:space:]]" " "
                                       (replace-regexp-in-string
                                        "'" "\"" results))))))
     results)))

(provide 'ob-coffee)
 ;;; ob-coffee.el ends here
