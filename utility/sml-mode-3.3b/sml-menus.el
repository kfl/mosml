;;; sml-menus.el. Simple menus for sml-mode

;; Copyright (C) 1994, Matthew J. Morley

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

;; You need auc-menu or easymenu on your lisp load-path.

;; Menus appear only when the cursor is in an sml-mode buffer. They
;; should appear automatically as long as sml-mode can find this file
;; and easymenu.el (or auc-menu.el), but not otherwise.

;; If you load sml-proc.el to run an inferior ML process -- or even a
;; superior one, who knows? -- the "Process" submenu will become active.

;;; CODE 

(condition-case () (require 'easymenu) (error (require 'auc-menu)))

;; That's FSF easymenu, distributed with GNU Emacs 19, or Per
;; Abrahamsen's auc-menu distributed with AUCTeX, or from the Emacs
;; lisp archive, or the IESD (ftp://sunsite.auc.dk/packages/auctex/)
;; lisp archive at Aalborg (auc-menu works with XEmacs too).

(defconst sml-menu
  (list ;"SML"
        (list "Process"
              ["Start default ML compiler"  sml
                :active (fboundp 'sml)]
              ["-" nil nil]
              ["load ML source file"        sml-load-file 
                :active (featurep 'sml-proc)]
              ["switch to ML buffer"        switch-to-sml
                :active (featurep 'sml-proc)]
              ["--" nil nil]
              ["send buffer contents"       sml-send-buffer 
                :active (featurep 'sml-proc)]
              ["send region"                sml-send-region 
                :active (featurep 'sml-proc)]
              ["send paragraph"             sml-send-function 
                :active (featurep 'sml-proc)]
              ["goto next error"            sml-next-error 
                :active (featurep 'sml-proc)]
              ["---" nil nil]
              ["Standard ML of New Jersey"  sml-smlnj
                :active (fboundp 'sml-smlnj)]
              ["Poly/ML"                    sml-poly-ml
                :active (fboundp 'sml-poly-ml)]
              ["Moscow ML"                  sml-mosml
                :active (fboundp 'sml-mosml)]
              ["Help for Inferior ML"   (describe-function 'inferior-sml-mode) 
                :active (featurep 'sml-proc)]
              )
        ["electric pipe"     sml-electric-pipe t]
        ["insert SML form"   sml-insert-form t]
        (list "Forms" 
              ["abstype"     sml-form-abstype t]
              ["datatype"    sml-form-datatype t]
              ["-" nil nil]
              ["let"         sml-form-let t]
              ["local"       sml-form-local t]
              ["case"        sml-form-case t]
              ["--" nil nil]
              ["signature"   sml-form-signature t]
              ["functor"     sml-form-functor t]
              ["structure"   sml-form-structure t])
        (list "Format/Mode Variables"
              ["indent region"             sml-indent-region t]
              ["outdent"                   sml-back-to-outer-indent t]
              ["-" nil nil]
              ["set indent-level"          sml-indent-level t]
              ["set pipe-indent"           sml-pipe-indent t]
              ["--" nil nil]
              ["toggle type-of-indent"     (sml-type-of-indent) t]
              ["toggle nested-if-indent"   (sml-nested-if-indent) t]
              ["toggle case-indent"        (sml-case-indent) t]
              ["toggle electric-semi-mode" (sml-electric-semi-mode) t])
        ["-----" nil nil]
        ["SML mode help (brief)"       describe-mode t]
        ["SML mode *info*"             sml-mode-info t]
        ["SML mode version"            sml-mode-version t]
        ["-----" nil nil]
        ["Fontify buffer"    (sml-mode-fontify-buffer)
                :active (or (featurep 'sml-font) (featurep 'sml-hilite))]
        ["Remove overlay"    (sml-error-overlay 'undo)
                :active (sml-overlay-active-p)]
        ))

(defun sml-mode-fontify-buffer ()
  "Just as it suggests."
  (cond ((featurep 'sml-font) 
         (font-lock-fontify-buffer))
        ((featurep 'sml-hilite) 
         (hilit-rehighlight-buffer))
        (t
         (message "No highlight scheme specified")))) ; belt & braces

(easy-menu-define sml-mode-menu
    sml-mode-map
    "Menu used in sml-mode."
    (cons "SML" sml-menu))

;;; Make's sure they appear in the menu bar when sml-mode-map is active.

;; On the hook for XEmacs only -- see easy-menu-add in auc-menu.el.

(defun sml-mode-menu-bar ()
  "Make sure menus appear in the menu bar as well as under mouse 3."
  (and (eq major-mode 'sml-mode)
       (easy-menu-add sml-mode-menu sml-mode-map)))

(add-hook 'sml-mode-hook 'sml-mode-menu-bar)

;; Autoload all the process code if these are selected.

(autoload 'sml "sml-proc" sml-no-doc t)

;; Not these two.
;; (autoload 'sml-poly-ml "sml-poly-ml" sml-no-doc t) 
;; (autoload 'sml-mosml "sml-mosml" sml-no-doc t) 

(provide 'sml-menus)

;;; sml-menu.el is over now.
