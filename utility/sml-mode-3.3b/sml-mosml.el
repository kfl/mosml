;;; sml-mosml.el: Modifies inferior-sml-mode defaults for Moscow ML.

;; Copyright (C) 1997, Matthew J. Morley

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

;;(autoload 'sml-mosml "sml-mosml" "Set up and run Moscow ML." t)

;; in your .emacs file. If you only ever use Moscow ML then you might
;; as well put something like

;;(setq sml-mode-hook
;;      '(lambda() "SML mode defaults to Moscow ML"
;;	 (define-key  sml-mode-map "\C-cp" 'sml-mosml)))

;; for your sml-mode-hook. The command prompts for the program name 
;; and any command line options. 

;; If you need to reset the default value of sml-program-name, or any
;; of the other compiler variables, put something like

;;(eval-after-load "sml-mosml" '(setq sml-program-name "whatever"))

;; in your .emacs -- or you can use the inferior-sml-{load,mode}-hooks
;; to achieve the same ends.

;;; CODE

(require 'sml-proc)

;; The regular expression used when looking for errors. Moscow ML errors:

(defconst sml-mosml-error-regexp
  (concat "^File \"\\([^\"]+\\)\","                    ;1
          " line \\([0-9]+\\)-?\\([0-9]+\\)?,"         ;2-3?
          " characters \\([0-9]+\\)-\\([0-9]+\\):")    ;4-5
  "Default regexp matching Moscow ML error messages.
If you change this significantly you may also need to redefine 
`sml-mosml-error-parser' (qv).")

;; File "puzz.ml", line 30-31, characters 10-70:
;; ! ..........first 0 l = []
;; !         | first n (h::t) = h::(first (n-1) t)
;; ! Warning: pattern matching is not exhaustive

;; ! Toplevel input:
;; ditto

(defconst sml-mosml-error-messages
  (concat "^! \\("
          (mapconcat 'identity
                     (list "\\(Warning: .*\\)"
                           "\\(Type clash\\):"
                           "\\(Ill-formed infix expression\\)"
                           "\\(Syntax error.*\\)")
                     "\\|")
          "\\).*$")
  "RE to match Moscow ML type-of-error reports. This regular expression
must follow the whole line pattern \"^! \\\\(%s\\\\).*$\", and the %s 
stands for a \"\\\\|\" separated list of regular expressions each of
which must, I repeat *must*, contain at least one \"\\\\(%s\\\\)\" group.
The %s regexp in the first such group will be the actual error report
echoed to the user.")

(defun sml-mosml-error-parser (pt)
 "This function looks for the next Moscow ML error message following PT
and parses an error message into a list
  \(file start-line start-col end-of-err msg\)
where

  FILE is the file in which the error occurs

  START-LINE is the line number in the file where the error occurs

  START-COL is the character position on START-LINE where the error occurs

  END-OF-ERR is an Emacs Lisp expression that when evaluated at
  \(start-line,start-col\) moves point to the end of the errorful text

  MSG is the text of the error message given by the compiler, if such text
  can be found.

The first three are mandatory return values for `sml-next-error'. 
See also `sml-error-parser'."
 (save-excursion
   (goto-char pt)
   (if (not (looking-at sml-mosml-error-regexp))
       ;; the user loses big time.
       (list nil nil nil)
     (let* ((file (match-string 1))                  ; the file
            (slin (string-to-int (match-string 2)))  ; the start line
            ;; char range is (n,m], 0 is column 1 of slin
            (scol (string-to-int (match-string 4)))  ; the start col
            ;; get to the end by doing "forward-char m - n"
            (eoe `(forward-char ,(- (string-to-int (match-string 5)) scol)))
            (msg))
       ;; look for the error message at end of the chunk of "! " lines
       (forward-line 1)
       (while (and (looking-at "^! ")
                   (not (looking-at sml-mosml-error-messages)))
         (forward-line 1))
       ;; found one if match-beginning 1 is non-nil.
       (if (match-beginning 1)
           (progn 
             (setq msg (match-string 1))
             ;; refine since m-begin 1 implies m-begin N for some N>1 as
             ;; long as sml-mosml-error-messages is sane as advertised.
             ;; match-data is a list N+1 of pairs, consecutive elts being
             ;; beg and end markers for the \( \) in the match. 0 is the
             ;; whole match.
             (let ((matches (1- (/ (length (match-data)) 2))) ; ignore 0th
                   (group 2))                                 ; & ignore 1st
               (while (and (not (match-beginning group))
                           (<= group matches))
                 (setq group (1+ group)))
               (if (<= group matches)
                   (setq msg (match-string group))))))
       ;; 1+ scol because char 0 means column 1 of slin.
       (nconc (list file slin (1+ scol)) (list eoe) (list msg))))))

;;;###autoload
(defun sml-mosml (pfx)
   "Set up and run Moscow ML.
Prefix argument means accept the defaults below.

Note: defaults set here will be clobbered if you setq them in the
inferior-sml-mode-hook.

 sml-program-name  <option> \(default \"mosml\"\)
 sml-default-arg   <option> \(default \"\"\)
 sml-use-command   \"use \\\"%s\\\"\"
 sml-cd-command    \"load \"FileSys\"; FileSys.chDir \\\"%s\\\"\"
 sml-prompt-regexp \"^- *\"
 sml-error-regexp  sml-mosml-error-regexp
 sml-error-parser  'sml-mosml-error-parser"
   (interactive "P")
   (let ((cmd (if pfx "mosml"
                (read-string "Command name: " sml-program-name)))
         (arg (if pfx ""
                (read-string "Any arguments or options (default none): " ""))))
     ;; sml-mode global variables
     (setq sml-program-name cmd)
     (setq sml-default-arg  arg)
     ;; buffer-local (compiler-local) variables
     (setq-default sml-use-command   "use \"%s\""
                   sml-cd-command    "load \"FileSys\"; FileSys.chDir \"%s\""
                   sml-prompt-regexp "^- *"
                   sml-error-regexp  sml-mosml-error-regexp
                   sml-error-parser  'sml-mosml-error-parser)
     (sml-run cmd sml-default-arg)))

;;; Do the default setup on loading this file.

;; setqing these two may override user's hooked defaults. users
;; therefore need load this file before setting sml-program-name or
;; sml-default-arg in their inferior-sml-load-hook. sorry.

(setq         sml-program-name  "mosml"
              sml-default-arg   "")

;; same sort of problem here too: users should to setq-default these
;; after this file is loaded, on inferior-sml-load-hook. as these are
;; buffer-local, users can instead set them on inferior-sml-mode-hook.

(setq-default sml-use-command   "use \"%s\""
              sml-cd-command    "load \"FileSys\"; FileSys.chDir \"%s\""
              sml-prompt-regexp "^- *"
              sml-error-regexp  sml-mosml-error-regexp
              sml-error-parser  'sml-mosml-error-parser)

;;; sml-mosml.el endeded
