;;; sml-site.el. Site initialisation for sml-mode

;; Copyright (C) 1997, Matthew J. Morley
;; Thanks to Ken Larsen <kla@it.dtu.dk> for his suggestions.

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

;; This file is provided for site administrators to install and
;; configure sml-mode for the convenience of all their users. Even if
;; you only install sml-mode for your private use, this is still a
;; good place to do the necessary configuration.

;; Follow the comments below to set the (few) necessary defaults; add
;; any other configurations to the end of the file. Users just need to
;; put

;;    (require 'sml-site)

;; in their .emacs files (along with any personal customisations).
;; Make sure this file is on the user's *default* load-path!

;;; CODE

;; *******************
;; sml-lisp-directory:
;; *******************

;; This is where the sml-mode lisp (.el and/or .elc) files are to be
;; kept. It is used for no purpose other than resetting the load-path
;; variable. Site administrators might consider setqing this in their
;; site-init.el file instead.

;; A subdirectory of site-lisp directory seems a reasonable place...

(defvar sml-lisp-directory "/usr/local/share/emacs/site-lisp/sml-mode"
  "*The directory where sml-mode lisp files are located.
Used in sml-site.el in resetting the Emacs lisp `load-path' (qv).")

(if (member sml-lisp-directory load-path)
    ()                                  ;take no prisoners
  (setq load-path (cons sml-lisp-directory load-path)))

;; ****************
;; auto-mode-alist:
;; ****************

;; Buffers for files that end with these extensions will be placed in
;; sml-mode automatically.

(if (rassoc 'sml-mode auto-mode-alist)
    ()                                  ;assume user has her own ideas
  (setq auto-mode-alist
        (append '(("\\.sml$" . sml-mode) 
                  ("\\.ML$"  . sml-mode)
                  ("\\.sig$" . sml-mode)) auto-mode-alist)))

;; **************
;; sml-mode-info:
;; **************

;; This is where sml-mode will look for it's online documentation. 

;; The default value in sml-mode.el is "sml-mode" which is correct if
;; sml-mode.info is placed somewhere on Emacs' default info directory
;; path. If you move sml-mode.info to the root of the site's info
;; hierarchy don't forget to add a `dir' file menu entry like

;;    * SML: (sml-mode).    Editing & Running Standard ML from Emacs 

;; If you can't (or won't) move the .info file onto the default info
;; directory path, uncomment this defvar and set the full name here.

;;(defvar sml-mode-info "/usr/???/sml-mode" "*Where to find sml-mode Info.")

;; *****************
;; sml-program-name:
;; *****************

;; sml-mode (sml-proc.el) defaults all its complier settings to SML/NJ
;; (0.93, in this release of sml-mode). If the New Jersey compiler is
;; called anything other than "sml" at your site, uncomment this
;; defvar and set the correct name here.

;;(defvar sml-program-name "sml" "*Program to run as ML.")

;; The info file (Configuration) explains how to set up sml-mode for
;; use with other ML compilers. Point users in that direction.

;;; AUTOLOADS

(autoload 'sml-mode "sml-mode" "Major mode for editing Standard ML." t)
(autoload 'sml "sml-proc" "Run an inferior ML process." t)

;; By all means set up Moscow ML and/or Poly/ML to autoload, but first
;; check that "mosml" and/or "poly" appear on the user's default PATH.

(autoload 'sml-mosml "sml-mosml" "Set up and run Moscow ML." t)
(autoload 'sml-poly-ml "sml-poly-ml" "Set up and run Poly/ML." t)

;; If they don't, users will winge until they discover how to change
;; their PATH, or redefine sml-program-name, for themselves.

;; Then

(provide 'sml-site)

;; and tell users to (require 'sml-site) in their .emacs files for the
;; above to take effect. Byte compile this file or not, as you wish.

;;; sml-site.el endeth.

