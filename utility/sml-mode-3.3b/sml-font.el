;;; sml-font.el --- Highlighting for sml-mode using font-lock.
;;
;; Copyright (C) 1995 Frederick Knabe
;;
;; Author:     Fritz Knabe <knabe@ecrc.de>
;;             ECRC GmbH, Arabellastr. 17, 81925 Munich, Germany
;; Created:    26 June 1995
;; Modified:   14 April 1997, M.J.Morley <mjm@scs.leeds.ac.uk>
;;             Add a couple of keywords to s-f-l-standard-keywords.
;;
;; $Revision: 1.1 $
;; $Date: 2000-01-21 10:07:13 $
;;
;; ====================================================================
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; If you did not receive a copy of the GNU General Public License with
;; this program, write to the Free Software Foundation, Inc., 675 Mass
;; Ave, Cambridge, MA 02139, USA.
;; ====================================================================
;;
;;; DESCRIPTION
;;
;; This package sets up highlighting of SML using font-lock.  If you
;; use the new version of font-lock distributed in GNU Emacs, SML's
;; nested comments as well as its special string escapes will be
;; handled properly.  The version of font-lock distributed with XEmacs
;; can also be used, but these special cases will not be handled.
;;
;; Should the fontification become incorrect while editing (for
;; example, when uncommenting), M-x font-lock-fontify-buffer will clear
;; things up.
;;
;; To install (assuming that you use sml-mode 3.1), put the following
;; in your .emacs:
;;
;;       (setq sml-hilite nil)     ; Turn off highlighting based on hilit19
;;
;;	  ;; For GNU Emacs
;;       (eval-after-load "sml-mode" '(require 'sml-font))
;;
;;       ;; For XEmacs
;;       (require 'sml-font)
;;
;;
;; Versions 3.2 and later of sml-mode define sml-load-hook (and the
;; variable sml-hilite is spurious), so you can simply put:
;;
;;       (setq sml-load-hook
;;             '(lambda() "Fontify SML." (require 'sml-font)))
;;
;; By default, font-lock will be turned on automatically for every SML
;; buffer.  If you don't want this, also add the following:
;;
;;       (setq sml-font-lock-auto-on nil)
;;
;; If you want to add to the keywords that will be fontified, set the
;; variable sml-font-lock-extra-keywords (see its documentation).
;;
;; Thanks to Matthew Morley <morley@gmd.de> for suggestions and fixes.
;; 

(require 'font-lock)

(defvar sml-font-lock-auto-on t
  "*If non-nil, turn on font-lock unconditionally for every SML buffer.")

(defvar sml-font-lock-extra-keywords nil
  ;; The example is easier to read if you load this package and use C-h v
  ;; to view the documentation.
  "*List of regexps to fontify as additional SML keywords.

For example, to add `xfun', `xfn', `special', and `=>', the value could be

	(\"\\=\\=\\=\\\\=\\=\\=\\<xfu?n\\\\|special\\\\>\" \"=>\")

The word delimiters in the first pattern prevent spurious highlighting
of keywords embedded inside other words (e.g., we don't want the tail of
`myxfun' to be highlighted).  You cannot use word delimiters with
symbolic patterns, however, because only alphanumerics are defined as
Emacs word constituents.  The second pattern would match the tail of a
symbolic identifier such as `==>', which might not be what you want.")

(defvar sml-font-lock-standard-keywords
  ;; Generated with Simon Marshall's make-regexp:
  ;; (make-regexp
  ;;  '("abstype" "and" "andalso" "as" "case" "datatype"
  ;;    "else" "end" "eqtype" "exception" "do" "fn" "fun" "functor"
  ;;    "handle" "if" "in" "include" "infix" "infixr" "let" "local" "nonfix"
  ;;    "of" "op" "open" "orelse" "overload" "raise" "rec" "sharing" "sig"
  ;;    "signature" "struct" "structure" "then" "type" "val" "where" "while"
  ;;    "with" "withtype") t)

  "\\<\\(a\\(bstype\\|nd\\(\\|also\\)\\|s\\)\\|case\\|d\\(atatype\\|o\\)\\|\
e\\(lse\\|nd\\|qtype\\|xception\\)\\|f\\(n\\|un\\(\\|ctor\\)\\)\\|\handle\\|\
i\\([fn]\\|n\\(clude\\|fixr?\\)\\)\\|l\\(et\\|ocal\\)\\|nonfix\\|\
o\\([fp]\\|pen\\|relse\\|verload\\)\\|r\\(aise\\|ec\\)\\|\
s\\(haring\\|ig\\(\\|nature\\)\\|truct\\(\\|ure\\)\\)\\|t\\(hen\\|ype\\)\\|\
val\\|w\\(h\\(ere\\|ile\\)\\|ith\\(\\|type\\)\\)\\)\\>"

  "Regexp matching standard SML keywords.")

(defvar sml-font-lock-all nil
  "Font-lock matchers for SML.")

(defun sml-font-lock-setup ()
  "Set buffer-local font-lock variables and possibly turn on font-lock."
  (let ((new-font-lock (boundp 'font-lock-defaults)))
    ;; If new-font-lock is t, use sml-font-comments-and-strings to do
    ;; fontification of comments and strings.  Otherwise, do
    ;; fontification using the SML syntax table (which will not always
    ;; be correct).
    (or sml-font-lock-all
	(setq sml-font-lock-all
	      (append
	       (and new-font-lock (list (list 'sml-font-comments-and-strings)))
	       sml-font-lock-extra-keywords
	       (list (list sml-font-lock-standard-keywords 1
			   'font-lock-keyword-face)))))
    (cond (new-font-lock
	   (make-local-variable 'font-lock-defaults)
	   (setq font-lock-defaults '(sml-font-lock-all t)))
	  (t
	   (setq font-lock-keywords sml-font-lock-all))))
  (and sml-font-lock-auto-on (turn-on-font-lock)))

(add-hook 'sml-mode-hook 'sml-font-lock-setup)

(defvar sml-font-cache '((0 . normal))
  "List of (POSITION . STATE) pairs for an SML buffer.
The STATE is either `normal', `comment', or `string'.  The POSITION is
immediately after the token that caused the state change.")

(make-variable-buffer-local 'sml-font-cache)

(defun sml-font-comments-and-strings (limit)
  "Fontify SML comments and strings up to LIMIT.
Handles nested comments and SML's escapes for breaking a string over lines.
Uses sml-font-cache to maintain the fontification state over the buffer."
  (let ((beg (point))
	last class)
    (while (< beg limit)
      (while (and sml-font-cache
		  (> (car (car sml-font-cache)) beg))
	(setq sml-font-cache (cdr sml-font-cache)))
      (setq last (car (car sml-font-cache)))
      (setq class (cdr (car sml-font-cache)))
      (goto-char last)
      (cond
       ((eq class 'normal)
	(cond
	 ((not (re-search-forward "\\((\\*\\)\\|\\(\"\\)" limit t))
	  (goto-char limit))
	 ((match-beginning 1)
	  (setq sml-font-cache (cons (cons (point) 'comment) sml-font-cache)))
	 ((match-beginning 2)
	  (setq sml-font-cache (cons (cons (point) 'string) sml-font-cache)))))
       ((eq class 'comment)
	(cond
	 ((let ((nest 1))
	    (while (and (> nest 0)
			(re-search-forward "\\((\\*\\)\\|\\(\\*)\\)" limit t))
	      (cond
	       ((match-beginning 1) (setq nest (+ nest 1)))
	       ((match-beginning 2) (setq nest (- nest 1)))))
	    (> nest 0))
	  (goto-char limit))
	 (t
	  (setq sml-font-cache (cons (cons (point) 'normal) sml-font-cache))))
	(put-text-property (- last 2) (point) 'face 'font-lock-comment-face))
       ((eq class 'string)
	(while (and (re-search-forward
		     "\\(\"\\)\\|\\(\\\\\\s-*\\\\\\)\\|\\(\\\\\"\\)" limit t)
		     (not (match-beginning 1))))
	(cond
	 ((match-beginning 1)
	  (setq sml-font-cache (cons (cons (point) 'normal) sml-font-cache)))
	 (t
	  (goto-char limit)))
	(put-text-property (- last 1) (point) 'face 'font-lock-string-face)))
      (setq beg (point)))))

(provide 'sml-font)
