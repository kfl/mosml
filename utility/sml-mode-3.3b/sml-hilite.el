;;; sml-hilite.el. Highlighting for sml-mode using hilit19.

;; Copyright (C) 1995 Frederick Knabe
;;
;; Author:     Fritz Knabe <knabe@ecrc.de>
;;             ECRC GmbH, Arabellastr. 17, 81925 Munich, Germany
;;
;; Created:    08-Nov-94, Fritz Knabe <knabe@ecrc.de>
;; Modified:   14-Apr-97, M.J.Morley <mjm@scs.leeds.ac.uk>
;;             Added a few keywords to hilit-set-mode-patters.

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

;; Put this code *after* the (require 'hilit19) in your .emacs.
;; Alternatively, put it in an (eval-after-load "hilit19" ...).

;; Better, use sml-load-hook like this:

;; (setq sml-load-hook
;;       '(lambda() "Highlights." (require 'sml-hilite)))

;; hilit19 does not currently appear to belong to XEmacs -- they
;; favour `font-lock'. Font-lock patterns in sml-font.el

;;; CODE

(require 'hilit19)

(cond ((and (x-display-color-p) (eq hilit-background-mode 'light))
       ;; for SML
       (hilit-translate sml-comment	'firebrick-italic)
       (hilit-translate sml-string	'ForestGreen-italic)
       (hilit-translate sml-keyword	'blue-bold))
      ((and (x-display-color-p) (eq hilit-background-mode 'dark))
       ;; for SML
       (hilit-translate sml-comment	'moccasin-italic)
       (hilit-translate sml-string	'green-italic)
       (hilit-translate sml-keyword	'cyan-bold))
      (t
       ;; for SML
       (hilit-translate sml-comment	'default-italic)
       (hilit-translate sml-string	'default-bold-italic)
       (hilit-translate sml-keyword	'default-bold)))

(hilit-set-mode-patterns
 'sml-mode
 '((kn-hilit-sml-string-find "" sml-string)
   (kn-hilit-sml-comment-find "" sml-comment)
   ;; The old patterns
   ;;("\"" "[^\\]\"" sml-string)
   ;;("(\\*" "\\*)" sml-comment)
   ("\\(\\`\\|[^_']\\)\
\\<\\(\
a\\(bstype\\|nd\\(\\|also\\)\\|s\\)\\|case\\|d\\(atatype\\|o\\)\\|\
e\\(lse\\|nd\\|qtype\\|xception\\)\\|f\\(n\\|un\\(\\|ctor\\)\\)\\|\handle\\|\
i\\([fn]\\|n\\(clude\\|fixr?\\)\\)\\|l\\(et\\|ocal\\)\\|nonfix\\|\
o\\([fp]\\|pen\\|relse\\|verload\\)\\|r\\(aise\\|ec\\)\\|\
s\\(haring\\|ig\\(\\|nature\\)\\|truct\\(\\|ure\\)\\)\\|t\\(hen\\|ype\\)\\|\
val\\|w\\(h\\(ere\\|ile\\)\\|ith\\(\\|type\\)\\)
\\)\\>\
\\(\\'\\|[^_']\\)" 2 sml-keyword)))

(defun kn-hilit-sml-string-find (dummy)
  "Find an SML string and return (START . END); if none, returns nil. 
Skips over potentially nested comments when searching for the start of the
string. Skips over \f...f\ (where f is whitespace) sequences in strings."
  (let ((nest 0)
	(continue t)
	st en)
    (while (and continue
		(re-search-forward "\\(\"\\)\\|\\((\\*\\)\\|\\(\\*)\\)" nil t))
      (cond
       ((match-beginning 1) (setq continue (> nest 0)))
       ((match-beginning 2) (setq nest (+ nest 1)))
       ((match-beginning 3) (setq nest (- nest 1)))))
    (if (not continue)
	(progn
	  (setq st (match-beginning 1))
	  (while (and (re-search-forward
		       "\\(\"\\)\\|\\(\\\\\\s-*\\\\\\)\\|\\(\\\\\"\\)" nil t)
		      (cond
		       ((match-beginning 1) (setq en (point)) nil)
		       ((match-beginning 2) t)
		       ((match-beginning 3) t))))
	  (and en (cons st en))))))

(defun kn-hilit-sml-comment-find (dummy)
  "Find an SML comment and return (START . END); if none, returns nil.
Handles nested comments. Ensures that the comment starts outside of a string."
  (let ((continue t)
	(nest 1)
	st en)
    (while (and continue
		(re-search-forward "\\(\"\\)\\|\\((\\*\\)" nil t))
      (cond
       ((match-beginning 1)
	(while (and (re-search-forward
		     "\\(\"\\)\\|\\(\\\\\\s-*\\\\\\)\\|\\(\\\\\"\\)" nil t)
		    (cond
		     ((match-beginning 1) nil)
		     ((match-beginning 2) t)
		     ((match-beginning 3) t)))))
       ((match-beginning 2) (setq continue nil))))
    (if (not continue)
	(progn
	  (setq st (match-beginning 2))
	  (setq continue t)
	  (while (and continue
		      (re-search-forward "\\((\\*\\)\\|\\(\\*)\\)" nil t))
	    (cond
	     ((match-beginning 1) (setq nest (+ nest 1)))
	     ((match-beginning 2)
	      (setq nest (- nest 1)) (setq continue (> nest 0)))))
	  (if (not continue)
	      (cons st (match-end 2)))))))

(provide 'sml-hilite)

;;; no more sml-hilite.el, it's finished.
