;;; sml-proc.el. Comint based interaction mode for Standard ML.

;; Copyright (C) 1989, Lars Bo Nielsen, 1994,1997 Matthew J. Morley

;; $Revision: 1.1 $
;; $Date: 2000-01-21 10:07:13 $

;; ====================================================================

;; This file is not part of GNU Emacs, but it is distributed under the
;; same conditions.

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
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 0139, USA.
;; (See sml-mode.el for HISTORY.) 

;; ====================================================================

;; [MJM 10/94] Separating this from sml-mode means sml-mode will run
;; under 18.59 (or anywhere without comint, if there are such places).
;; See sml-mode.el for further information.

;;; DESCRIPTION

;; Inferior-sml-mode is for interacting with an ML process run under
;; emacs. This uses the comint package so you get history, expansion,
;; backup and all the other benefits of comint. Interaction is
;; achieved by M-x sml which starts a sub-process under emacs. You may
;; need to set this up for autoloading in your .emacs:

;; (autoload 'sml "sml-proc" "Run an inferior ML process." t)

;; Exactly what process is governed by the variable sml-program-name
;; -- just "sml" by default. If you give a prefix argument (C-u M-x
;; sml) you will be prompted for a different program to execute from
;; the default -- if you just hit RETURN you get the default anyway --
;; along with the option to specify any command line arguments. Once
;; you select the ML program name in this manner, it remains the
;; default (unless you set in a hook, or otherwise).

;; NOTE: inferior-sml-mode-hook is run AFTER the ML program has been
;; launched. inferior-sml-load-hook is run only when sml-proc.el is
;; loaded into Emacs.

;; When running an ML process some further key-bindings are effective
;; in sml-mode buffer(s). C-c C-s (switch-to-sml) will split the
;; screen into two windows if necessary and place you in the ML
;; process buffer. In the interaction buffer, C-c C-s is bound to the
;; `sml' command by default (in case you need to restart).

;; C-c C-l (sml-load-file) will load an SML source file into the
;; inferior process, C-c C-r (sml-send-region) will send the current
;; region of text to the ML process, etc. Given a prefix argument to
;; these commands will switch you from the SML buffer to the ML
;; process buffer as well as sending the text. If you get errors
;; reported by the compiler, C-c ` (sml-next-error) will step through
;; the errors with you.

;; NOTE. There is only limited support for this as it obviously
;; depends on the compiler's error messages being recognised by the
;; mode. Error reporting is currently only geared up for SML/NJ,
;; Moscow ML, and Poly/ML (see file sml-{mosml,poly-ml}.el). Look at
;; the documentation for sml-error-parser and sml-next-error -- you
;; may only need to modify the former to recover this feature for some
;; other ML systems, along with sml-error-regexp.

;; While small pieces of text can be fed quite happily into the ML
;; process directly, lager pieces should (probably) be sent via a
;; temporary file making use of the compiler's "use" command. 

;; CURRENT RATIONALE: you get sense out of the error messages if
;; there's a real file associated with a block of code, and XEmacs is
;; less likely to hang. These are likely to change.

;; For more information see the variable sml-temp-threshold. You
;; should set the variable sml-use-command appropriately for your ML
;; compiler. By default things are set up to work for the SML/NJ
;; compiler.

;;; FOR YOUR .EMACS

;; Here  are some ideas for inferior-sml-*-hooks:

;; (setq inferior-sml-load-hook
;;       '(lambda() "Set global defaults for inferior-sml-mode"
;;          (define-key inferior-sml-mode-map "\C-cd"    'sml-cd)
;;          (define-key          sml-mode-map "\C-cd"    'sml-cd)
;;          (define-key          sml-mode-map "\C-c\C-f" 'sml-send-function)
;;          (setq sml-temp-threshold 0))) ; safe: always use tmp file

;; (setq inferior-sml-mode-hook
;;       '(lambda() "Inferior SML mode defaults"
;;          (setq comint-scroll-show-maximum-output t
;;                comint-scroll-to-bottom-on-output t
;;                comint-input-autoexpand nil)))

;; ===================================================================

;;; INFERIOR ML MODE VARIABLES

(require 'sml-mode)
(require 'comint)
(provide 'sml-proc)

(defvar sml-program-name "sml"
  "*Program to run as ML.")

(defvar sml-default-arg ""
  "*Default command line option to pass, if any.")

(defvar sml-display-frame-alist
  '((height . 24) (width . 80) (menu-bar-lines . 0))
  "*Alist of frame parameters used in creating dedicated ML interaction frames.
These supersede the values given in `default-frame-alist'.
You might like a larger screen

  \(setcdr \(assoc 'height sml-display-frame-alist\) 40\)

or you might like a small font

  \(setq sml-display-frame-alist 
        \(cons '\(font . \"7x14\"\) sml-display-frame-alist\)\)

in your `inferior-sml-load-hook', say. The parameters

  '\(\(unsplittable . t\) \(icon-name . \"*sml*\"\)\)

are always added to sml-display-frame-alist by default, though the value of
icon-name is actually culled from `sml-program-name'. 

See also the documentation for `modify-frame-parameters'.")

(defvar sml-dedicated-frame (if window-system t nil)
  "*If non-nil, interaction buffers display in their own frame.
Default is equivalent to variable `window-system'.
If you reset this variable after starting the compiler, you might have
to reset the window-dedicated property of the window displaying the
interaction buffer. See `set-window-dedicated-p'.")

;;(defvar sml-raise-on-error nil
;;  "*When non-nil, `sml-next-error' will raise the ML process's frame.")

(defvar sml-temp-threshold 0
  "*Controls when emacs uses temporary files to communicate with ML. 
If not a number (e.g., NIL), then emacs always sends text directly to
the subprocess. If an integer N, then emacs uses a temporary file
whenever the text is longer than N chars. `sml-temp-file' contains the
name of the temporary file for communicating. See variable
`sml-use-command' and function `sml-send-region'.

Sending regions directly through the pty (not using temp files)
doesn't work very well -- e.g., SML/NJ nor Poly/ML incorrectly report
the line # of errors occurring in std_in.")

(defvar sml-temp-file (make-temp-name "/tmp/ml")
  "*Temp file that emacs uses to communicate with the ML process.
See `sml-temp-threshold'. Defaults to \(make-temp-name \"/tmp/ml\"\)")

(defvar inferior-sml-mode-hook nil
  "*This hook is run when the inferior ML process is started.
All buffer local customisations for the interaction buffers go here.")

(defvar inferior-sml-load-hook nil
  "*Hook run when inferior-sml-mode (sml-proc.el) is loaded into Emacs.
This is a good place to put your preferred key bindings.")

(defvar sml-buffer nil
  "*The current ML process buffer.

MULTIPLE PROCESS SUPPORT (Whoever wants multi-process support anyway?)
=====================================================================
sml-mode supports, in a fairly simple fashion, running multiple ML
processes. To run multiple ML processes, you start the first up with
\\[sml]. It will be in a buffer named *sml*. Rename this buffer with
\\[rename-buffer]. You may now start up a new process with another
\\[sml]. It will be in a new buffer, named *sml*. You can switch
between the different process buffers with \\[switch-to-buffer].

NB *sml* is just the default name for the buffer. It actually gets
it's name from the value of `sml-program-name' -- *poly*, *smld*,...

If you have more than one ML process around, commands that send text
from source buffers to ML processes -- like `sml-send-function' or
`sml-send-region' -- have to choose a process to send it to. This is
determined by the global variable `sml-buffer'. Suppose you have three
inferior ML's running:
    Buffer      Process
    sml         #<process sml>
    mosml       #<process mosml>
    *sml*       #<process sml<2>>
If you do a \\[sml-send-function] command on some ML source code, 
what process do you send it to?

- If you're in a process buffer (sml, mosml, or *sml*), you send it to
  that process (usually makes sense only to `sml-load-file').
- If you're in some other buffer (e.g., a source file), you send it to
  the process attached to buffer `sml-buffer'.

This process selection is performed by function `sml-proc' which looks
at the value of `sml-buffer' -- which must be a lisp buffer object, or
a string \(or nil\).

Whenever \\[sml] fires up a new process, it resets `sml-buffer' to be
the new process's buffer. If you only run one process, this will do
the right thing. If you run multiple processes, you can change
`sml-buffer' to another process buffer with \\[set-variable], or
use the command \\[sml-buffer] in the interaction buffer of choice.")


;;; ALL STUFF THAT DEFAULTS TO THE SML/NJ COMPILER (0.93)

(defvar sml-use-command "use \"%s\""
  "*Template for loading a file into the inferior ML process.
Set to \"use \\\"%s\\\"\" for SML/NJ or Edinburgh ML; 
set to \"PolyML.use \\\"%s\\\"\" for Poly/ML, etc.")

(defvar sml-cd-command "System.Directory.cd \"%s\""
  "*Command template for changing working directories under ML.
Set this to nil if your compiler can't change directories.

The format specifier \"%s\" will be converted into the directory name
specified when running the command \\[sml-cd].")

(defvar sml-prompt-regexp "^[\-=] *"
  "*Regexp used to recognise prompts in the inferior ML process.")

(defvar sml-error-parser 'sml-smlnj-error-parser
  "*This function parses an error message into a 3-5 element list:

    \(file start-line start-col end-line-col err-msg\).

The first three components are required by `sml-next-error', but the other
two are optional. If the file associated with the input is the standard
input stream, this function should probably return

    \(\"std_in\" start-line start-col\).

This function will be called in a context in which the match data \(see
`match-data'\) are current for `sml-error-regexp'. The mode sets the
default value to the function `sml-smlnj-error-parser'.

In a step towards greater sml-mode modularity END-LINE-COL can be either

  - the symbol nil \(in which case it is ignored\)

or

  - an Emacs Lisp expression that when `eval'd at \(start-line,start-col\)
    will move point to the end of the errorful text in the file.

Note that the compiler should return the full path name of the errorful
file, and that this might require you to fiddle with the compiler's
prettyprinting switches.")

;; std_in:2.1-4.3 Error: operator and operand don't agree (tycon mismatch)
;; std_in:2.1 Error: operator and operand don't agree (tycon mismatch)

(defconst sml-smlnj-error-regexp
  (concat
   "^[-= ]*\\(.+\\):"                     ;file name
   "\\([0-9]+\\)\\.\\([0-9]+\\)"          ;start line.column
   "\\(-\\([0-9]+\\)\\.\\([0-9]+\\)\\)?"  ;end line.colum
   ".+\\(\\(Error\\|Warning\\): .*\\)")   ;the message

  "Default regexp matching SML/NJ error and warning messages.

There should be no need to customise this, though you might decide
that you aren't interested in Warnings -- my advice would be to modify
`sml-error-regexp' explicitly to do that though.

If you do customise `sml-smlnj-error-regexp' you may need to modify
the function `sml-smlnj-error-parser' (qv).")

(defvar sml-error-regexp sml-smlnj-error-regexp
  "*Regexp for matching \(the start of\) an error message.")

(defun sml-smlnj-error-parser (pt)
 "This parses the SML/NJ error message at PT into a 5 element list

    \(file start-line start-col end-of-err msg\)

where FILE is the file in which the error occurs\; START-LINE is the line
number in the file where the error occurs\; START-COL is the character
position on that line where the error occurs. 

If present, the fourth return value is a simple Emacs Lisp expression that
will move point to the end of the errorful text, assuming that point is at
\(start-line,start-col\) to begin with\; and MSG is the text of the error
message given by the compiler."

 ;; This function uses `sml-smlnj-error-regexp' to do the parsing, and
 ;; assumes that regexp groups 1, 2, and 3 correspond to the first three
 ;; elements of the list returned\; and groups 5, 6 and 7 correspond to the
 ;; optional elements in that order.

 (save-excursion
   (goto-char pt)
   (if (not (looking-at sml-smlnj-error-regexp))
       ;; the user loses big time.
       (list nil nil nil)
     (let ((file (match-string 1))                  ; the file
           (slin (string-to-int (match-string 2)))  ; the start line
           (scol (string-to-int (match-string 3)))  ; the start col
           (msg (if (match-beginning 7) (match-string 7))))
       ;; another loss: buggy sml/nj's produce nonsense like file:0.0 Error
       (if (zerop slin) (list file nil scol)
         ;; ok, was a range of characters mentioned?
         (if (match-beginning 4)
             ;; assume m-b 4 implies m-b 5 and m-b 6 (sml-smlnj-error-regexp)
             (let* ((elin (string-to-int (match-string 5))) ; end line
                    (ecol (string-to-int (match-string 6))) ; end col
                    (jump (if (= elin slin)
                              ;; move forward on the same line
                              `(forward-char ,(1+ (- ecol scol)))
                            ;; otherwise move down, and over to ecol
                            `(progn
                               (forward-line ,(- elin slin))
                               (forward-char ,ecol)))))
               ;; nconc glues lists together. jump & msg aren't lists
               (nconc (list file slin scol) (list jump) (list msg)))
           (nconc (list file slin scol) (list nil) (list msg))))))))

(defun sml-smlnj (pfx)
   "Set up and run Standard ML of New Jersey.
Prefix argument means accept the defaults below.

Note: defaults set here will be clobbered if you setq them in the
inferior-sml-mode-hook.

 sml-program-name  <option> \(default \"sml\"\)
 sml-default-arg   <option> \(default \"\"\) 
 sml-use-command   \"use \\\"%s\\\"\"
 sml-cd-command    \"System.Directory.cd \\\"%s\\\"\"
 sml-prompt-regexp \"^[\\-=] *\"
 sml-error-regexp  sml-sml-nj-error-regexp
 sml-error-parser  'sml-sml-nj-error-parser"
   (interactive "P")
   (let ((cmd (if pfx "sml"
                (read-string "Command name: " sml-program-name)))
         (arg (if pfx ""
                (read-string "Any arguments or options (default none): "))))
     ;; sml-mode global variables
     (setq sml-program-name cmd)
     (setq sml-default-arg  arg)
     ;; buffer-local (compiler-local) variables
     (setq-default sml-use-command   "use \"%s\""
                   sml-cd-command    "System.Directory.cd \"%s\""
                   sml-prompt-regexp "^[\-=] *"
                   sml-error-regexp  sml-smlnj-error-regexp
                   sml-error-parser  'sml-smlnj-error-parser)
     (sml-run cmd sml-default-arg)))


;;; CODE

(defvar inferior-sml-mode-map nil)

;; buffer-local

(defvar sml-error-file nil)             ; file from which the last error came
(defvar sml-real-file nil)              ; used for finding source errors
(defvar sml-error-cursor nil)           ;   ditto
(defvar sml-error-barrier nil)          ;   ditto

(defun sml-proc-buffer ()
  "Returns the current ML process buffer,
or the current buffer if it is in `inferior-sml-mode'. Raises an error
if the variable `sml-buffer' does not appear to point to an existing
buffer."
  (let ((buffer
         (cond ((eq major-mode 'inferior-sml-mode)
                ;; default to current buffer if it's in inferior-sml-mode
                (current-buffer))
               ((bufferp sml-buffer)
               ;; buffer-name returns nil if the buffer has been killed
                (buffer-name sml-buffer))
               ((stringp sml-buffer)
                ;; get-buffer returns nil if there's no buffer of that name
                (get-buffer sml-buffer)))))
    (or buffer
        (error "No current process buffer. See variable sml-buffer"))))

(defun sml-proc ()
  "Returns the current ML process. See variable `sml-buffer'."
  (let ((proc (get-buffer-process (sml-proc-buffer))))
    (or proc
        (error "No current process. See variable sml-buffer"))))

(defun sml-buffer (echo)
  "Make the current buffer the current `sml-buffer' if that is sensible.
Lookup variable `sml-buffer' to see why this might be useful."
  (interactive "P")
  (let ((current
         (cond ((bufferp sml-buffer) (or (buffer-name sml-buffer) "undefined"))
               ((stringp sml-buffer) sml-buffer)
               (t "undefined"))))
  (if echo (message (format "ML process buffer is %s." current))
    (let ((buffer (if (eq major-mode 'inferior-sml-mode) (current-buffer))))
      (if (not buffer) (message (format "ML process buffer is %s." current))
        (setq sml-buffer buffer)
        (message (format "ML process buffer is %s." (buffer-name buffer))))))))

(defun sml-noproc () 
  "Nil iff `sml-proc' returns a process."
  (condition-case nil (progn (sml-proc) nil) (error t)))

(defun sml-proc-tidy ()
  "Something to add to `kill-emacs-hook' to tidy up tmp files on exit."
  (if (file-readable-p sml-temp-file)
      (delete-file sml-temp-file)))

(defun inferior-sml-mode ()
  "Major mode for interacting with an inferior ML process.

The following commands are available:
\\{inferior-sml-mode-map}

An ML process can be fired up (again) with \\[sml].

Customisation: Entry to this mode runs the hooks on `comint-mode-hook'
and `inferior-sml-mode-hook' (in that order).

Variables controlling behaviour of this mode are

`sml-program-name' (default \"sml\")
    Program to run as ML.

`sml-use-command' (default \"use \\\"%s\\\"\")
    Template for loading a file into the inferior ML process.

`sml-cd-command' (default \"System.Directory.cd \\\"%s\\\"\")
    ML command for changing directories in ML process (if possible).

`sml-prompt-regexp' (default \"^[\\-=] *\")
    Regexp used to recognise prompts in the inferior ML process.

`sml-temp-threshold' (default 0)
    Controls when emacs uses temporary files to communicate with ML. 
    If an integer N, then emacs uses a temporary file whenever the
    text is longer than N chars. 

`sml-temp-file' (default (make-temp-name \"/tmp/ml\"))
    Temp file that emacs uses to communicate with the ML process.

`sml-error-regexp' 
   (default -- complicated)
    Regexp for matching error messages from the compiler.

`sml-error-parser' (default 'sml-smlnj-error-parser)
    This function parses a error messages into a 3, 4 or 5 element list:
    (file start-line start-col (end-line end-col) err-msg).

You can send text to the inferior ML process from other buffers containing
ML source.  
    `switch-to-sml' switches the current buffer to the ML process buffer.
    `sml-send-function' sends the current *paragraph* to the ML process.
    `sml-send-region' sends the current region to the ML process.

    Prefixing the sml-send-<whatever> commands with \\[universal-argument]
    causes a switch to the ML process buffer after sending the text.

For information on running multiple processes in multiple buffers, see
documentation for variable `sml-buffer'.

Commands:
RET after the end of the process' output sends the text from the 
    end of process to point.
RET before the end of the process' output copies the current line
    to the end of the process' output, and sends it.
DEL converts tabs to spaces as it moves back.
TAB file name completion, as in shell-mode, etc.."
  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (setq comint-prompt-regexp sml-prompt-regexp)
  (sml-mode-variables)

  ;; For sequencing through error messages:
  (make-local-variable 'sml-error-cursor)
  (setq sml-error-cursor (marker-position (point-max-marker)))
  (make-local-variable 'sml-error-barrier)
  (setq sml-error-barrier (marker-position (point-max-marker)))
  (make-local-variable 'sml-real-file)
  (setq sml-real-file (cons nil 0))

  (make-local-variable 'sml-use-command)
  (make-local-variable 'sml-cd-command)
  (make-local-variable 'sml-prompt-regexp)
  (make-local-variable 'sml-error-parser)
  (make-local-variable 'sml-error-regexp)

  (setq major-mode 'inferior-sml-mode)
  (setq mode-name "Inferior ML")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-sml-mode-map)
  (add-hook 'kill-emacs-hook 'sml-proc-tidy)

  (run-hooks 'inferior-sml-mode-hook))

;;; FOR RUNNING ML FROM EMACS

;;;###autoload 
(defun sml (&optional pfx)
  "Run an inferior ML process, input and output via buffer *sml*. 
With a prefix argument, this command allows you to specify any command
line options to pass to the complier. The command runs hook functions
on `comint-mode-hook' and `inferior-sml-mode-hook' in that order.

If there is a process already running in *sml*, just switch to that
buffer instead. 

In fact the name of the buffer created is chosen to reflect the name
of the program name specified by `sml-program-name', or entered at the
prompt. You can have several inferior ML process running, but only one
current one -- given by `sml-buffer' (qv).

\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive "P")
  (let ((cmd (if pfx
                 (read-string "ML command: " sml-program-name)
               sml-program-name))
        (args (if pfx
                  (read-string "Any args: " sml-default-arg)
                sml-default-arg)))
    (sml-run cmd args)))

(defun sml-run (cmd arg)
  "Run the ML program CMD with given arguments ARGS.
This usually updates `sml-buffer' to a buffer named *CMD*."
  (let* ((pname (file-name-nondirectory cmd))
         (bname (format "*%s*" pname))
         (args (if (equal arg "") () (sml-args-to-list arg))))
    (if (comint-check-proc bname)
        (sml-pop-to-buffer t)           ;do nothing but switch buffer
      (setq sml-buffer 
            (if (null args) 
                ;; there is a good reason for this; to ensure
                ;; *no* argument is sent, not even a "".
                (set-buffer (apply 'make-comint pname cmd nil))
              (set-buffer (apply 'make-comint pname cmd nil args))))
      (message (format "Starting \"%s\" in background." pname))
      (inferior-sml-mode)
      (goto-char (point-max))
      ;; and this -- to keep these as defaults even if
      ;; they're set in the mode hooks.
      (setq sml-program-name cmd)
      (setq sml-default-arg arg))))

(defun sml-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((not (= where 0))
           (cons (substring string 0 where)
                 (sml-args-to-list (substring string (+ 1 where)
                                              (length string)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (if (null pos)
                   nil
                   (sml-args-to-list (substring string pos
                                                (length string)))))))))

(defun sml-temp-threshold (&optional thold)
  "Set the variable to the given prefix (nil, if no prefix given).
This is really mainly here to help debugging sml-mode!"
  (interactive "P")
  (setq sml-temp-threshold 
        (if current-prefix-arg (prefix-numeric-value thold)))
  (message "%s" sml-temp-threshold))

;;;###autoload 
(defun switch-to-sml (eob-p)
  "Switch to the ML process buffer.
With prefix argument, positions cursor at point, otherwise at end of buffer."
  (interactive "P")
  (sml-pop-to-buffer t)
  (cond ((not eob-p)
         (push-mark (point) t)
         (goto-char (point-max)))))

;; Fakes it with a "use <temp-file>;" if necessary.

;;;###autoload 
(defun sml-send-region (start end &optional and-go)
  "Send current region to the inferior ML process.
Prefix argument means switch-to-sml afterwards.

If the region is longer than `sml-temp-threshold' and the variable
`sml-use-command' is defined, the region is written out to a temporary file
and a \"use <temp-file>\" command is sent to the compiler\; otherwise the
text in the region is sent directly to the compiler. In either case a
trailing \"\;\\n\" will be added automatically.

See variables `sml-temp-threshold', `sml-temp-file' and `sml-use-command'."
  (interactive "r\nP")
  (if (sml-noproc) (save-excursion (sml t)))
  (cond ((equal start end)
         (message "The region is zero (ignored)"))
        ((and sml-use-command
              (numberp sml-temp-threshold)
              (< sml-temp-threshold (- end start)))
         ;; Just in case someone is still reading from sml-temp-file:
         (if (file-exists-p sml-temp-file)
             (delete-file sml-temp-file))
         (write-region start end sml-temp-file nil 'silently)
         (sml-update-barrier (buffer-file-name (current-buffer)) start)
         (sml-update-cursor (sml-proc-buffer))
         (comint-send-string (sml-proc)
                 (concat (format sml-use-command sml-temp-file) ";\n")))
        (t
         (comint-send-region (sml-proc) start end)
         (comint-send-string (sml-proc) ";\n")))
  (if and-go (switch-to-sml nil)))

;; Update the buffer-local variables sml-real-file and sml-error-barrier
;; in the process buffer:

(defun sml-update-barrier (file pos)
  (let ((buf (current-buffer)))
    (unwind-protect
        (let* ((proc (sml-proc))
               (pmark (marker-position (process-mark proc))))
          (set-buffer (process-buffer proc))
          ;; update buffer local variables
          (setq sml-real-file (and file (cons file pos)))
          (setq sml-error-barrier pmark))
      (set-buffer buf))))

;; Update the buffer-local error-cursor in proc-buffer to be its
;; current proc mark.

(defun sml-update-cursor (proc-buffer)  ;always= sml-proc-buffer
  (let ((buf (current-buffer)))
    (unwind-protect
        (let* ((proc (sml-proc))        ;just in case?
               (pmark (marker-position (process-mark proc))))
          (set-buffer proc-buffer)
          ;; update buffer local variable
          (setq sml-error-cursor pmark))
      (set-buffer buf))))

;; This is quite bogus, so it isn't bound to a key by default.
;; Anyone coming up with an algorithm to recognise fun & local
;; declarations surrounding point will do everyone a favour!

(defun sml-send-function (&optional and-go)
  "Send current paragraph to the inferior ML process. 
With a prefix argument switch to the sml buffer as well 
\(cf. `sml-send-region'\)."
  (interactive "P")
  (save-excursion
    (sml-mark-function)
    (sml-send-region (point) (mark)))
  (if and-go (switch-to-sml nil)))

;;;###autoload 
(defun sml-send-buffer (&optional and-go)
  "Send buffer to inferior shell running ML process. 
With a prefix argument switch to the sml buffer as well
\(cf. `sml-send-region'\)."
  (interactive "P")
  (if (memq major-mode sml-source-modes)
    (sml-send-region (point-min) (point-max) and-go)))

;; Since sml-send-function/region take an optional prefix arg, these
;; commands are redundant. But they are kept around for the user to
;; bind if she wishes, since its easier to type C-c r than C-u C-c C-r.

(defun sml-send-region-and-go (start end)
  "Send current region to the inferior ML process, and go there."
  (interactive "r")
  (sml-send-region start end t))

(defun sml-send-function-and-go ()
  "Send current paragraph to the inferior ML process, and go there."
  (interactive)
  (sml-send-function t))


;;; Mouse control and handling dedicated frames for Inferior ML

;; simplified from frame.el in Emacs: special-display-popup-frame...

;; Display BUFFER in its own frame, reusing an existing window if any.
;; Return the window chosen.

(defun sml-display-popup-frame (buffer &optional args)
  (let ((window (get-buffer-window buffer t)))
    (if window
        ;; If we have a window already, make it visible.
        (let ((frame (window-frame window)))
          (make-frame-visible frame)
          (raise-frame frame)
          window)
      ;; otherwise no window yet, make one in a new frame.
      (let* ((frame (make-frame (append args sml-display-frame-alist)))
             (window (frame-selected-window frame)))
        (set-window-buffer window buffer)
        ;; XEmacs mostly ignores this
        (set-window-dedicated-p window t)
        window))))

(defun sml-proc-frame ()
  "Returns the current ML process buffer's frame, or creates one first."
  (let ((buffer (sml-proc-buffer)))
    (window-frame 
     (or
      ;; if its already displayed on some frame, take that as default...
      (get-buffer-window buffer t)
      ;; ...irrespective of what sml-dedicated-frame says, otherwise
      ;; create a new frame (or raise an old one) perhaps...
      (and sml-dedicated-frame 
           (sml-display-popup-frame buffer
                                    (list (cons 'icon-name buffer)
                                          '(unsplittable . t))))
      ;; ...or default to the current frame anyway.
      (frame-selected-window)))))

(defun sml-pop-to-buffer (warp)
  "(Towards) handling multiple frames properly.
Raises the frame, and warps the mouse over there, only if WARP is non-nil."
  (let ((current (window-frame (selected-window)))
        (buffer  (sml-proc-buffer)))
    (let ((frame (sml-proc-frame)))
      (if (eq current frame)
          (pop-to-buffer buffer)           ; stay on the same frame.
        (select-frame frame)               ; XEmacs sometimes moves focus.
        (select-window (get-buffer-window buffer)) ; necc. for XEmacs
        ;; (raise-frame frame)
        (if warp (sml-warp-mouse frame))))))


;;; H A C K   A T T A C K !   X E M A C S   V E R S U S   E M A C S

;; Only these two functions have to dance around the inane differences 
;; between Emacs and XEmacs (fortunately)

(defun sml-warp-mouse (frame)
  "Warp the pointer across the screen to upper right corner of FRAME."
  (raise-frame frame)
  (cond ((string-match "\\(Lucid\\|XEmacs\\)" emacs-version)
         ;; LUCID (19.10) or later... set-m-pos needs a WINDOW
         (set-mouse-position (frame-root-window frame) (1- (frame-width)) 0))
        (t
         ;; GNU, post circa 19.19... set-m-pos needs a FRAME
         (set-mouse-position frame (1- (frame-width)) 0)
         ;; probably not needed post 19.29
         (if (fboundp 'unfocus-frame) (unfocus-frame)))))

(defun sml-drag-region (event)
  "Highlight the text the mouse is dragged over, and send it to ML.
This must be bound to a button-down mouse event, currently \\[sml-drag-region].

If you drag the mouse (ie, keep the mouse button depressed) the
program text sent to the complier is delimited by where you started
dragging the mouse, and where you release the mouse button.

If you only click the mouse, the program text sent to the compiler is
delimited by the current position of point and the place where you
click the mouse.

In either event, the values of both point and mark are left
undisturbed once this operation is completed."
  (interactive "e")
  (let ((mark-ring)                     ;BAD: selection start gets cons'd
        (pmark (point)))                ;where point is now
    (if (fboundp 'mouse-track-default)
        ;; Assume this is XEmacs, otherwise assume its Emacs
        (save-excursion
          (let ((zmacs-regions))
            (set-marker (mark-marker) nil)
            (mouse-track-default event)
            (if (not (region-exists-p)) (push-mark pmark nil t))
            (call-interactively 'sml-send-region)))
      ;; Emacs: making this buffer-local ought to happen in sml-mode
      (make-local-variable 'transient-mark-mode)
      (save-excursion
        (let ((transient-mark-mode 1))
          (mouse-drag-region event)
          (if (not mark-active) (push-mark pmark nil t))
          (call-interactively 'sml-send-region))))))


;;; LOADING AND IMPORTING SOURCE FILES:

(defvar sml-source-modes '(sml-mode) 
  "*Used to determine if a buffer contains ML source code. 
If it's loaded into a buffer that is in one of these major modes, it's
considered an ML source file by `sml-load-file'. Used by these commands
to determine defaults.")

(defvar sml-prev-l/c-dir/file nil
  "Caches the (directory . file) pair used in the last `sml-load-file'
or `sml-cd' command. Used for determining the default in the next one.")

;;;###autoload 
(defun sml-load-file (&optional and-go)
  "Load an ML file into the current inferior ML process. 
With a prefix argument switch to sml buffer as well.

This command uses the ML command template `sml-use-command' to construct
the command to send to the ML process\; a trailing \"\;\\n\" will be added
automatically."
  (interactive "P")
  (if (sml-noproc) (save-excursion (sml t)))
  (if sml-use-command
      (let ((file 
             (car (comint-get-source "Load ML file: " sml-prev-l/c-dir/file
                                     sml-source-modes t))))
        ;; Check if buffer needs saved. Should (save-some-buffers) instead?
        (comint-check-source file)
        (setq sml-prev-l/c-dir/file
              (cons (file-name-directory file) (file-name-nondirectory file)))
        (sml-update-cursor (sml-proc-buffer))
        (comint-send-string
         (sml-proc) (concat (format sml-use-command file) ";\n")))
    (message "Can't load files if `sml-use-command' is undefined!"))
  (if and-go (switch-to-sml nil)))

(defun sml-cd (dir)
  "Change the working directory of the inferior ML process.
The default directory of the process buffer is changed to DIR. If the
variable `sml-cd-command' is non-nil it should be an ML command that will
be executed to change the compiler's working directory\; a trailing
\"\;\\n\" will be added automatically."
  (interactive "DSML Directory: ")
  (let* ((buf (sml-proc-buffer))
         (proc (get-buffer-process buf))
         (dir (expand-file-name dir)))
    (save-excursion
      (set-buffer buf)
      (if sml-cd-command
          (process-send-string proc
                               (concat (format sml-cd-command dir) ";\n")))
      (cd dir))
    (setq sml-prev-l/c-dir/file (cons dir nil))))

;;; PARSING ERROR MESSAGES

;; to a very large extent "find-file-other-window" works admirably when the
;; compiler is running in a dedicated, *unsplittable* window, and so all
;; the goop in sml-file-other-frame-or-window is of questionable worth.
;; unhappily, XEmacs doesn't (yet, will it ever?) implement the window
;; unsplittable property, hence this nonsense...

(defun sml-file-other-frame-or-window (file &optional window)
  "Find or make another frame on which to display FILE.
Start in ML interaction buffer, by hypothesis, and try not to use
this window to display the file (with bugs in it). FILE may already
be on display somewhere, so use that frame by default; otherwise,
try to find a window that is displaying an sml buffer; if there is
no such frame/window, find the nearest non-dedicated buffer or,
in the last resort, create a whole new frame.

If optional WINDOW is supplied, just use that window to display FILE."
  (if window
      (progn                            ; just reuse it
        (set-window-buffer window (find-file-noselect file))
        (select-window window))         ; assume "this" frame's selected)
    (let* ((buf (find-file-noselect file))
           (win (get-buffer-window buf t))
           (frm (if win (window-frame win))))
      (if frm
          ;; buf is displayed in win on some frame: select frame & window
          (progn (select-window win) (raise-frame (select-frame frm)))
        (let* ((frame (selected-frame))   ;current frame & window
               (window (selected-window)))
          ;; look through all (but minibuffer) windows for an sml buffer
          (while (and (not (eq window
                               (select-window
                                (previous-window (selected-window) 'mini t))))
                      (not (memq major-mode sml-source-modes))))
          (if (not (eq window (selected-window)))
              ;; found window displaying an sml buffer: use that window & frame
              (raise-frame (select-frame (window-frame (selected-window))))
            ;; otherwise, cycle through frames looking for a spare one
            ;; select-frame also selects the top (or root) window
            (while (and (not (eq frame (select-frame (previous-frame
                                                      (selected-frame) nil))))
                        (window-dedicated-p (selected-window))))
            ;; if no suitable frame, create one and (belt & braces) select it
            (if (eq frame (selected-frame))
                ;; sml-dedicated-frame iff window-dedicated-p (selected-window)
                (if sml-dedicated-frame
                    (progn
                      (sml-warp-mouse (select-frame (make-frame)))
                      (set-window-buffer
                       (frame-selected-window (selected-frame)) buf))
                  (switch-to-buffer-other-window buf))
              (raise-frame (selected-frame))))
          (switch-to-buffer buf))))))

;; This should need no modification to support other compilers. 

;;;###autoload 
(defun sml-next-error (skip)
  "Find the next error by parsing the inferior ML buffer. 
A prefix argument means `sml-skip-errors' (qv) instead.

Move the error message on the top line of the window\; put the cursor
\(point\) at the beginning of the error source.

If the error message specifies a range, and `sml-error-parser' returns
the range, the mark is placed at the end of the range. If the variable
`sml-error-overlay' is non-nil, the region will also be highlighted.

If `sml-error-parser' returns a fifth component this is assumed to be
a string to indicate the nature of the error: this will be echoed in
the minibuffer.

Error interaction only works if there is a real file associated with
the input -- though of course it also depends on the compiler's error
messages \(also see documantation for `sml-error-parser'\).

However: if the last text sent went via `sml-load-file' (or the temp
file mechanism), the next error reported will be relative to the start
of the region sent, any error reports in the previous output being
forgotten. If the text went directly to the compiler the succeeding
error reported will be the next error relative to the location \(in
the output\) of the last error. This odd behaviour may have a use...?"
  (interactive "P")
  (if skip (sml-skip-errors) (sml-do-next-error)))

(defun sml-bottle (msg)
  "Function to let `sml-next-error' give up gracefully."
  (sml-warp-mouse (selected-frame))
  (error msg))

(defun sml-do-next-error ()
  "The buisiness end of `sml-next-error' (qv)"
  (let ((case-fold-search nil)
        ;; set this variable iff we called sml-next-error in a SML buffer
        (sml-window (if (memq major-mode sml-source-modes) (selected-window)))
        (proc-buffer (sml-proc-buffer)))
    ;; undo (don't destroy) the previous overlay to be tidy
    (sml-error-overlay 'undo 1 1
                       (and sml-error-file (get-file-buffer sml-error-file)))
    ;; go to interaction buffer but don't raise it's frame 
    (sml-pop-to-buffer nil)
    ;; go to the last remembered error, and search for the next one.
    (goto-char sml-error-cursor)
    (if (not (re-search-forward sml-error-regexp (point-max) t))
        ;; no more errors -- move point to the sml prompt at the end
        (progn
          (goto-char (point-max))
          (if sml-window (select-window sml-window)) ;return there, perhaps
          (message "No error message(s) found."))
      ;; error found: point is at end of last match; set the cursor posn.
      (setq sml-error-cursor (point))
      ;; move the SML window's text up to this line
      (set-window-start (get-buffer-window proc-buffer) (match-beginning 0))
      (let* ((pos)
             (parse (funcall sml-error-parser (match-beginning 0)))
             (file (nth 0 parse))
             (line0 (nth 1 parse))
             (col0 (nth 2 parse))
             (line/col1 (nth 3 parse))
             (msg (nth 4 parse)))
        ;; Give up immediately if the error report is scribble
        (if (or (null file) (null line0))
            (sml-bottle "Failed to parse/locate this error properly!"))
        ;; decide what to do depending on the file returned
        (if (string= file "std_in")
            ;; presently a fundamental limitation i'm afraid.
            (sml-bottle "Sorry, can't locate errors on std_in.")
          (if (string= file sml-temp-file)
              ;; errors found in tmp file; seek the real file
              (if (< (point) sml-error-barrier)
                  ;; weird. user cleared *sml* and use'd the tmp file?
                  (sml-bottle "Temp file error report is not current.")
                (if (not (car sml-real-file))
                    ;; sent from a buffer w/o a file attached.
                    ;; DEAL WITH THIS EVENTUALLY.
                    (sml-bottle "No real file associated with the temp file.")
                  ;; real file and error-barrier
                  (setq file (car sml-real-file))
                  (setq pos (cdr sml-real-file))))))
        (if (not (file-readable-p file))
            (sml-bottle (concat "Can't read " file))
          ;; instead of (find-file-other-window file) to lookup the file
          (sml-file-other-frame-or-window file sml-window)
          ;; no good if the buffer's narrowed, still...
          (goto-char (or pos 1))        ; line 1 if no tmp file
          (forward-line (1- line0))
          (forward-char (1- col0))
          ;; point is at start of error text; seek the end.
          (let ((start (point))
                (end (and line/col1
                          (condition-case nil
                              (progn (eval line/col1) (point))
                            (error nil)))))
            ;; return to start anyway
            (goto-char start)
            ;; if point went to end, put mark there, and maybe highlight
            (if end (progn (push-mark end t)
                           (sml-error-overlay nil start end)))
            (setq sml-error-file file)   ; remember this for next time
            (if msg (message msg)))))))) ; echo the error/warning message

(defun sml-skip-errors ()
  "Skip past the rest of the errors."
  (interactive)
  (if (memq major-mode sml-source-modes) (sml-error-overlay 'undo))
  (sml-update-cursor (sml-proc-buffer))
  (if (eq major-mode 'sml-inferior-mode) (goto-char (point-max))))

;;; Set up the inferior mode keymap, using sml-mode bindings...

(cond ((not inferior-sml-mode-map)
       (setq inferior-sml-mode-map
             (copy-keymap comint-mode-map))
       (install-sml-keybindings inferior-sml-mode-map)
       (define-key inferior-sml-mode-map "\C-c\C-s" 'sml)
       (define-key inferior-sml-mode-map "\t"       'comint-dynamic-complete)))

;;; H A C K   A T T A C K !   X E M A C S   /   E M A C S   K E Y S

(if window-system
    (cond ((string-match "\\(Lucid\\|XEmacs\\)" emacs-version)
	   ;; LUCID (19.10) or later...
	   (define-key sml-mode-map '(meta shift button1) 'sml-drag-region))
	  (t
	   ;; GNU, post circa 19.19
	   (define-key sml-mode-map [M-S-down-mouse-1] 'sml-drag-region))))

;;; ...and do the user's customisations.

(run-hooks 'inferior-sml-load-hook)

;;; Here is where sml-proc.el ends
