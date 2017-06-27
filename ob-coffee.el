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
  "Default header arguments for coffee-script code blocks.")

(defcustom org-babel-cjs-cmd "node"
  "Name of command used to evaluate js blocks."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defvar org-babel-coffee-function-wrapper
  "console.log( do -> %s )"
  "coffee-script code to print value of body.")

(defun org-babel-execute:coffee (body params)
  "Execute a block of Coffee code with org-babel.
 This function is called by `org-babel-execute-src-block'"
  (message "executing Coffee source code block")
  (let* ((tmp-src-file (org-babel-temp-file "cjs-src-" ".coffee"))
         (tmp-js-file (dired-replace-in-string "coffee" "js" tmp-src-file))
         (org-babel-cjs-cmd (or (cdr (assq :cmd params)) org-babel-cjs-cmd))
         (result-type (cdr (assq :result-type params)))
         (full-body (org-babel-expand-body:generic
                     body params (org-babel-variable-assignments:coffee params))))
    (with-temp-file tmp-src-file
      (insert
       (if (string= result-type "value")
           (format org-babel-coffee-function-wrapper (replace-regexp-in-string "\n" ";" full-body))
         full-body
         )))
    (org-babel-eval
     (format "coffee -c -b %s"
             (org-babel-process-file-name tmp-src-file)) "")
    (let ((results
           (org-babel-eval
            (format "%s %s" org-babel-cjs-cmd
                    (org-babel-process-file-name tmp-js-file)) "")))
      (org-babel-reassemble-table
       (org-babel-result-cond (cdr (assq :result-params params))
         results (org-babel-coffee-read (org-trim results)))
       (org-babel-pick-name (cdr (assq :colname-names params))
                            (cdr (assq :colnames params)))
       (org-babel-pick-name (cdr (assq :rowname-names params))
                            (cdr (assq :rownames params)))))))

(defun org-babel-prep-session:coffee (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "Session evaluation with coffee-script is not supported"))

(defun org-babel-coffee-var-to-coffee (var)
  "Convert an elisp var into a string of coffee source code
 specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-coffee-var-to-coffee var ", ") "]")
    (replace-regexp-in-string "\n" "\\\\n" (format "%S" var))))

(defun org-babel-coffee-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
 Return the initialized session."
  (error "Session evaluation with coffee-script is not supported"))

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
