;;; sml-poly-ml.el: Modifies inferior-sml-mode defaults for Poly/ML.

;; Copyright (C) 1994,1997 Matthew J. Morley

;; $Revision: 1.1 $
;; $Date: 2000-01-21 10:07:13 $

;; This file is not part of GNU Emacs, but it is distributed under the
;; same conditions.

;; ====================================================================

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; ====================================================================

;;; DESCRIPTION

;; To use this library just put

;;(autoload 'sml-poly-ml "sml-poly-ml" "Set up and run Poly/ML." t)

;; in your .emacs file. If you only ever use Poly/ML then you might as
;; well put something like

;;(setq sml-mode-hook
;;      '(lambda() "SML mode defaults to Poly/ML"
;;	 (define-key  sml-mode-map "\C-cp" 'sml-poly-ml)))

;; for your sml-load-hook. The command prompts for the program name
;; and the database to use, if any. 

;; If you need to reset the default value of sml-program-name, or any
;; of the other compiler variables, put something like

;;(eval-after-load "sml-poly-ml" '(setq sml-program-name "whatever"))

;; in your .emacs -- or you can use the inferior-sml-{load,mode}-hooks
;; to achieve the same ends.

;;; CODE

(require 'sml-proc)

(defconst sml-poly-ml-error-regexp
  "^\\(Error\\|Warning:\\) in '\\(.*\\)', line \\([0-9]+\\)"
  "Default regexp matching Poly/ML error messages.")

;; The reg-expression used when looking for errors. Poly/ML errors:

;; Warning: in 'puzz.sml', line 28
;; Matches are not exhaustive.

;; Error
;; Value or constructor (tl) has not been declared
;; Found near tl(tl(tl(tl(N))))

;; (when input is from std_in -- i.e. entered directly at the prompt).

(defun sml-poly-ml-error-parser (pt) 
 "This function parses a Poly/ML error message into a 3 element list.
  (file start-line start-col) required by `sml-next-error'."
 (save-excursion
   (goto-char pt)
   (if (not (looking-at sml-poly-ml-error-regexp))
       ;; the user loses big time.
       (list nil nil nil)      
     (list (match-string 2)                    ; the file
           (string-to-int (match-string 3))    ; the start line
           1))))                               ; the start col

;;;###autoload
(defun sml-poly-ml (pfx)
   "Set up and run Poly/ML.
Prefix argument means accept the defaults below.

Note: defaults set here will be clobbered if you setq them in the
inferior-sml-mode-hook.

 sml-program-name  <option> \(default \"poly\"\)
 sml-default-arg   <option dbase> \(default \"\"\)
 sml-use-command   \"PolyML.use \\\"%s\\\"\"
 sml-cd-command    \"PolyML.cd \\\"%s\\\"\"
 sml-prompt-regexp \"^[>#] *\"
 sml-error-regexp  sml-poly-ml-error-regexp
 sml-error-parser  'sml-poly-ml-error-parser"
   (interactive "P")
   (let ((cmd (if pfx "poly"
                (read-string "Command name: " sml-program-name)))
	 (arg (if pfx ""
                (read-file-name "Poly database? (default none): " "" ""))))
     ;; sml-mode global variables
     (setq sml-program-name cmd)
     (setq sml-default-arg  (if (equal arg "") "" (expand-file-name arg)))
     ;; buffer-local (compiler-local) variables
     (setq-default sml-use-command   "PolyML.use \"%s\""
                   sml-cd-command    "PolyML.cd \"%s\""
                   sml-prompt-regexp "^[>#] *"
                   sml-error-regexp  sml-poly-ml-error-regexp
                   sml-error-parser  'sml-poly-ml-error-parser)
     (sml-run cmd sml-default-arg)))

;;; Do the default setup on loading this file.

;; setqing these two may override user's hooked defaults. users
;; therefore need load this file before setting sml-program-name or
;; sml-default-arg in their inferior-sml-load-hook. sorry.

(setq         sml-program-name  "poly"
              sml-default-arg   "")

;; same sort of problem here too: users should to setq-default these
;; after this file is loaded, on inferior-sml-load-hook. as these are
;; buffer-local, users can instead set them on inferior-sml-mode-hook.

(setq-default sml-use-command   "PolyML.use \"%s\""
              sml-cd-command    "PolyML.cd \"%s\""
              sml-prompt-regexp "^[>#] *"
              sml-error-regexp  sml-poly-ml-error-regexp
              sml-error-parser  'sml-poly-ml-error-parser)

;;; sml-poly-ml.el ended just there
