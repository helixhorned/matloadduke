;;; m32script-mode.el -- Major mode for editing Eduke .con files

;; Author: Philipp Kutin, based on a tutorial by Scott Andrew Borton
;; Created: 01 Sep 2007
;; Last updated: 23 Jan 2011
;; Keywords: Duke3D Mapster32 script con major-mode

;; Copyright (C) 2007-2012 Philipp Kutin
;; Copyright (C) 2000, 2003 Scott Andrew Borton

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Installation:
;;
;; Have the directory which contains this file in your
;; load-path, for example: (add-to-list 'load-path "~/.emacs-lisp")
;; Add the following to your ~/.emacs file:
;; (require 'm32script-mode)

;;; Commentary:
;; 
;; The mode is based on a tutorial about Emacs
;; mode creation which can be found here:
;; http://two-wugs.net/emacs/mode-tutorial.html
;; (link dead as of 2009-03-23)

;; The following features are implemented:
;; imenu, eldoc, movement by top-level definitions (states, onevents)
;; using the keys for by-defun-movement (`C-M-a' and `C-M-e' by default),
;; outline (outlining top-level definitions).
;; A function `m32script-find-definition' which takes the symbol at point
;; and searches `m32script-main-con-file-name' and files included therein
;; for a "define" or "gamevar" definition of that symbol. If it is found,
;; the line containing the definition is printed on the mode line.
;; For this to work, `m32script-working-directory' must also be set.

;; The following keybindings are made:
;; `C-c C-d': `m32script-find-definition'
;; `C-c C-s', `C-c C-e' insert a new state and onevent block, respectively.

;; Also, check out the customization group `m32script' for some
;; user-customizable options.

;;; Code:
(defvar m32script-mode-hook nil)
(defvar m32script-mode-map
  (let ((m32script-mode-map (make-sparse-keymap)))
	(define-key m32script-mode-map "\C-c\C-d" 'm32script-find-definition)
	(define-key m32script-mode-map "\C-c\C-s" 'm32script-insert-state)
	(define-key m32script-mode-map "\C-c\C-e" 'm32script-insert-event)
    m32script-mode-map)
  "Keymap for M32SCRIPT major mode")

(add-to-list 'auto-mode-alist '("\\.m32\\'" . m32script-mode))

(defconst m32script-keywords-1
  '(array gamearray gamevar var definequote define include))  ; #define

(defconst m32script-keywords-2
  '(defstate ends onevent endevent))

(defconst m32script-keywords-3
  '(else return break switch case default endswitch getcurraddress jump
 ifl ifle ifg ifge ife ifn ifand ifor ifxor ifeither ifboth whilen whilel
 ifvarl ifvarle ifvarg ifvarge ifvare ifvarn ifvarand ifvaror ifvarxor ifvareither ifvarboth whilevarn whilevarl
 ifvarvarl ifvarvarle ifvarvarg ifvarvarge ifvarvare ifvarvarn ifvarvarand ifvarvaror ifvarvarxor ifvarvareither ifvarvarboth whilevarvarn whilevarvarl
 ifhitkey ifhighlighted ifholdkey ifrnd ifangdiffl ifspritepal ifactor ifsound ifpdistl ifpdistg
 ifin3dmode ifinside ifinteractive
 ifeitheralt ifeitherctrl ifeithershift ifawayfromwall ifcansee ifonwater ifinwater ifoutside ifnosounds 
 ifaimingsprite ifaimingwall ifaimingsector for))

(defconst m32script-keywords-4
  '(nullop state
 a2xy ah2xyz al shiftl shiftr rand set add sub mul div mod and or xor
 setsector getsector setwall getwall setsprite getsprite getspritelinktype gettspr settspr
 setarray getarraysize resizearray copy
 randvar displayrandvar setvar addvar subvar mulvar divvar modvar andvar orvar xorvar shiftvarl shiftvarr shiftvarvarl shiftvarvarr
 randvarvar displayrandvarvar setvarvar addvarvar subvarvar mulvarvar divvarvar modvarvar andvarvar orvarvar xorvarvar sin cos
 displayrand
 itof ftoi clamp inv sqrt mulscale divscale dist ldist getangle getincangle globalsound getsoundflags
 calchypotenuse clipmove clipmovenoslide lineintersect rayintersect
 setkey sort
 resetkey insertsprite dupsprite tdupsprite deletesprite lastwall updatecursectnum updatesector updatesectorz getzrange hitscan cansee canseespr neartag rotatepoint dragpoint getceilzofslope getflorzofslope alignceilslope alignflorslope bsetsprite setfirstwall changespritestat changespritesect headspritestat prevspritestat nextspritestat headspritesect prevspritesect nextspritesect sectorofwall fixrepeats getclosestcol
 addlogvar addlog debug
 redefinequote print quote error printmessage16 printmessage256 printext256 printext16 getnumber16 getnumber256 getnumberfromuser qsprintf qstrcat qstrcpy qstrlen
 qstrncat qsubstr
 findnearsprite findnearspritevar findnearsprite3d findnearsprite3dvar findnearspritez findnearspritezvar
 getticks gettimedate setaspect
 seti sizeat cstat cstator clipdist spritepal cactor spgetlotag spgethitag sectgetlotag sectgethitag gettexturefloor gettextureceiling
 sethighlight sethighlightsector collectsectors sound soundonce stopallsounds stopsound updatehighlight updatehighlightsector
 drawline16 drawline16b drawline16z drawcircle16 drawcircle16b drawcircle16z rotatesprite16 rotatesprite rotatespritea setgamepalette))

(defconst m32script-sector-labels '(wallptr wallnum ceilingz
  floorz ceilingstat floorstat ceilingpicnum ceilingslope
  ceilingshade ceilingpal ceilingxpanning ceilingypanning
  floorpicnum floorslope floorshade floorpal floorxpanning
  floorypanning visibility alignto lotag hitag extra ))

(defconst m32script-wall-labels '(x y point2 nextwall nextsector
  cstat picnum overpicnum shade pal xrepeat yrepeat xpanning
  ypanning lotag hitag extra))

(defconst m32script-sprite-labels '(x y z cstat picnum shade pal
  clipdist detail xrepeat yrepeat xoffset yoffset sectnum statnum
  ang owner xvel yvel zvel lotag hitag extra))

(defconst m32script-iter-tokens '(allsprites allsectors allwalls
selsprites selsectors selwalls drawnsprites spritesofsector
loopofwall wallsofsector range
selspr selsec sprofsec walofsec))




;;; Font lock
(defgroup m32script nil
  "Major mode for editing Duke3D m32scipt files."
  :group 'languages
  :link `(url-link :tag "Mail the author"
          ,(concat (string 109 97 105 108 116 111 58 104 101 108 105
120 104 111 114 110 101 100 64 103 109 97 105 108 46 99 111 109)
"?subject=M32Scipt Mode")))

(defvar m32script-static-keywords 'm32script-static-keywords)
(defvar m32script-block-keywords 'm32script-block-keywords)
(defvar m32script-flow-keywords 'm32script-flow-keywords)
(defvar m32script-misc-keywords 'm32script-misc-keywords)
(defvar m32script-label-keywords 'm32script-label-keywords)

(defface m32script-static-keywords '((t (:inherit font-lock-builtin-face)))
  "*Keywords with a meaning at compilation time." :group 'm32script :group 'faces)
(defface m32script-block-keywords '((t (:inherit font-lock-builtin-face)))
  "*Block-enclosing keywords for M32script Mode." :group 'm32script :group 'faces)
(defface m32script-flow-keywords '((t (:inherit font-lock-keyword-face)))
  "*Control-flow keywords for M32script Mode." :group 'm32script :group 'faces)
(defface m32script-misc-keywords '((t (:inherit font-lock-constant-face :weight bold)))
  "*All other keywords for M32script Mode." :group 'm32script :group 'faces)
(defface m32script-label-keywords '((t ()))
  "*Sector/sprite/wall labels for M32script Mode." :group 'm32script :group 'faces)

(defconst m32script-font-lock-keywords-1
  `(
	(,(concat "\\_<" (regexp-opt 
                      (mapcar 'symbol-name m32script-keywords-1)
                      t)
              "\\_>") .  m32script-static-keywords) )

;  '(
;    ("\\_<\\(?:game\\)?\\(?:var\\|array\\)\\|define" (0 font-lock-builtin-face)
;     ("\\<[A-Za-z_0-9]+\\>" nil nil (0 font-lock-variable-name-face)))
;    ("\\_<\\(\\(?:game\\)?\\(?:var\\|array\\)\\|define\\)\\s +\\([A-Za-z_0-9]+\\)"
;     (0 font-lock-builtin-face) (1 font-lock-variable-name-face))
;)
  "Compile-time primitives highlighted in M32SCRIPT mode.")

(defconst m32script-font-lock-keywords-2
  (append m32script-font-lock-keywords-1
		  `(
			(,(concat "\\b" (regexp-opt
                              (mapcar 'symbol-name m32script-keywords-2) t) "\\_>")
             . m32script-block-keywords) ) )
  "Block-enclosing primitives highlighted in M32SCRIPT mode.")

(defconst m32script-font-lock-keywords-3
  (append m32script-font-lock-keywords-2
		  `(
			(,(concat "\\_<" (regexp-opt
                              (mapcar 'symbol-name m32script-keywords-3) t) "\\_>")
             . m32script-flow-keywords) ) )
  "Control-flow primitives highlighted in M32SCRIPT mode.")

(defconst m32script-font-lock-keywords-4
  (append m32script-font-lock-keywords-3
		  `(
			(,(concat "\\_<" (regexp-opt
                              (mapcar 'symbol-name m32script-keywords-4) t) "\\_>")
             . m32script-misc-keywords) )
		  )
  "All other primitives highlighted in M32SCRIPT mode.")

(defconst m32script-font-lock-keywords-5
  (append 
		  `(
			(,(concat "\\.\\<" (regexp-opt
                              (append
                               (mapcar 'symbol-name m32script-sector-labels)
                               (mapcar 'symbol-name m32script-sprite-labels)
                               (mapcar 'symbol-name m32script-wall-labels))
                              t) "\\_>")
             . m32script-label-keywords) )
          m32script-font-lock-keywords-4 )
  ; labels come first because .cstat shall not be fontified as a keyword
  "Sprite/sector/wall labels highlighted in M32SCRIPT mode.")

(defvar m32script-font-lock-keywords m32script-font-lock-keywords-5
  "Default highlighting expressions for M32SCRIPT mode.")


(defcustom m32script-tab-width 4
  "Tab width for M32script Mode."
  :group 'm32script)

(defvar m32script-paragraph-start "^\\(defstate\\|onevent\\)")
(defvar m32script-paragraph-seperate "^end\\(s\\|event\\)")

(defvar m32script-outline-regexp m32script-paragraph-start)

(defvar m32script-imenu-generic-expression
  (let ((id-regex "[[:alpha:]_][[:alnum:]_]+")) ;duke syntax is actually more permissive
	`(("states" ,(concat "^\\(defstate\\)[ 	]+\\(" id-regex "\\)") 2)
	  ("events" ,(concat "^\\(onevent\\)[ 	]+\\(" id-regex "\\)") 2)
	  )))

(defun m32script-beginning-of-defun ()
  (interactive)
  (search-backward-regexp m32script-paragraph-start))

(defun m32script-end-of-defun ()
  (interactive)
  (search-forward-regexp m32script-paragraph-seperate))

(defun m32script-indent-line ()
  "Indent current line as M32SCRIPT code."
  (interactive)
    (beginning-of-line)
    (if (bobp)
        (indent-line-to 0)  ; First line is always non-indented
      (let ((not-indented t) cur-indent)
                                        ; If the line we are looking at is the end of a block, then decrease the indentation
        (cond
         ((looking-at "^[ \t]*\}")
          (save-excursion
            (forward-line -1)
            (setq cur-indent (- (current-indentation) m32script-tab-width)))
          (if (< cur-indent 0) (setq cur-indent 0)))
         ((looking-at "^[ \t]*\\(end\\(a\\|s\\|event\\)\\)")
          (setq cur-indent 0))

         (t
          (save-excursion
            (while not-indented ; Iterate backwards until we find an indentation hint
              (forward-line -1)
                                        ; This hint indicates that we need to indent at the level of the end... or } token
              (if (looking-at "^[ \t]*\\(end\\(s\\|event\\)\\|\}\\)")
                  (progn
                    (setq cur-indent (current-indentation))
                    (setq not-indented nil))
                                        ; This hint indicates that we need to indent an extra level
                (if (looking-at "^\\(defstate\\|onevent\\|\.*{\\)")
                    (progn
                      (setq cur-indent (+ (current-indentation) m32script-tab-width))
                      (setq not-indented nil))
                  (if (bobp)
                      (setq not-indented nil)))))))
         )
        (if cur-indent
            (indent-line-to cur-indent)
          (indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

(defvar m32script-mode-syntax-table
  (let ((m32script-mode-syntax-table (make-syntax-table)))
	; recognize braces pairs
	(modify-syntax-entry ?{ "(}" m32script-mode-syntax-table)
	(modify-syntax-entry ?} "){" m32script-mode-syntax-table)
	
	; Comment styles are same as C++
	(modify-syntax-entry ?/ ". 124b" m32script-mode-syntax-table)
	(modify-syntax-entry ?* ". 23" m32script-mode-syntax-table)
	(modify-syntax-entry ?\n "> b" m32script-mode-syntax-table)
	m32script-mode-syntax-table)
  "Syntax table for m32script-mode")


;;; Eldoc support
(setq m32script-eldoc-obarray (make-vector 307 0))

(defun m32script-add-eldoc-strings ()
  (dolist (l m32script-eldoc-string-list)
	(set (intern (car l) m32script-eldoc-obarray) (cadr l))))

(defun m32script-eldoc-function ()
  "Returns a documentation string appropriate for the current context or nil."
  (let* ((sym (intern-soft (thing-at-point 'symbol) m32script-eldoc-obarray))
		 (doc (symbol-value sym)))
	(if sym nil
	  (save-excursion
		(catch 'break
		  (while (search-backward-regexp "\\_<.*?\\_>" (point-at-bol) t)
			(setq sym (intern-soft (match-string-no-properties 0) m32script-eldoc-obarray))
			(when sym
			  (setq doc (symbol-value sym))
			  (throw 'break t))
			))))
	(when doc
	  (put-text-property 0 (- (match-end 0) (match-beginning 0)) 'face 'font-lock-keyword-face doc)
	  doc)))

(defconst m32script-eldoc-orientation-string
  (let ((f '(lambda (s) (propertize s 'face 'font-lock-keyword-face)))
		(g '(lambda (s) (propertize s 'face 'font-lock-comment-face))))
	(concat "\norientation: "
			(funcall f "1") (funcall g ":t1 ") (funcall f "2") (funcall g ":320x200 ")
			(funcall f "4") (funcall g ":invY ") (funcall f "8") (funcall g ":clip start[ud]most ")
			(funcall f "16") (funcall g ":center top-left ") (funcall f "32") (funcall g ":t2 ")
			(funcall f "64") (funcall g ":masking off"))))

(defconst m32script-eldoc-string-list 
  `(
; TODO: ...
	("getangle" "getangle <x> <y>")
	("rotatepoint" "rotatepoint <xpivot> <ypivot> <x> <y> <ang> <xreturnvar> <yreturnvar>")
	("updatesector" "updatesector <x> <y> <gamevar>")
	("updatesectorz" "updatesectorz <x> <y> <z> <gamevar>")
	))


(defun m32script-mode ()
  "Major mode for editing Eduke .con files"
  (interactive)
  (kill-all-local-variables)
  (use-local-map m32script-mode-map)
  (set-syntax-table m32script-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(m32script-font-lock-keywords))
  ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'm32script-indent-line)
  (set (make-local-variable 'paragraph-start) m32script-paragraph-start)
  (set (make-local-variable 'paragraph-seperate) m32script-paragraph-seperate)
  (set (make-local-variable 'eldoc-documentation-function) 'm32script-eldoc-function)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'beginning-of-defun-function) 'm32script-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'm32script-end-of-defun)
  (set (make-local-variable 'outline-regexp) m32script-outline-regexp)
  (set (make-local-variable 'imenu-generic-expression) m32script-imenu-generic-expression)
  (setq major-mode 'm32script-mode)
  (setq mode-name "Duke3D-CON")
  (m32script-add-eldoc-strings)
  (run-hooks 'm32script-mode-hook))

;; --------------------------------

(defun m32script-insert-block (name start end &optional nospace) (interactive "M")
  (insert (concat start (if nospace "" " ") name "\n\n" end))
  (previous-line)
  (m32script-indent-line))

(defun m32script-insert-state (name) (interactive "MState name: ")
  (m32script-insert-block name "defstate" "ends"))

(defun m32script-insert-event (name) (interactive "MEvent name: ")
  (m32script-insert-block name "onevent EVENT_" "endevent" t))


(defcustom m32script-main-con-file-name "GAME.CON"
  "* File which will be searched first for \"includes\"."
  :group 'm32script :type 'string)

(defcustom m32script-working-directory
  "D:/Games/EDuke32/"
  "* Duke working directory, must end in a slash. Will be searched for other con files."
  :group 'm32script :type 'string)

(defun m32script-try-set-buffer-cases (name)
  "Try to return the buffer named NAME, trying different case."
   (condition-case nil (set-buffer name)
	 (error (condition-case nil (set-buffer (downcase name))
			  (error (condition-case nil (set-buffer (upcase name))
					   (error (setq buffer-was-there nil))))))))

(defun m32script-find-definition ()
  "Try to find the definition of a \"define\" or \"gamevar\",
gathering con files to search from m32script-main-con-file-name.
Don't recurse con files."
  (interactive)
  (let* (cons-to-examine-list
		 (case-fold-search nil)	;make search case-sensitive
		 (sym-to-search (substring-no-properties (thing-at-point 'symbol)))
		 (search-str
		  (concat "\\<\\(define\\|gamevar\\) " sym-to-search "\\>.*$"))
		 (include-con-regexp "\\<include[ \t]*\\(.*\.[Cc][Oo][Nn]\\>\\)"))
	  (set-buffer (find-file-noselect m32script-main-con-file-name t))
	  (save-excursion
		(goto-char (point-min))
		(while (re-search-forward include-con-regexp nil t)
		  (let* ((conname (match-string-no-properties 1)))
			(add-to-list 'cons-to-examine-list conname t))));)
	  (dolist (cur-con cons-to-examine-list)
;		(when (equal cur-con (car cons-to-examine-list))
;		  (message (concat "\`" sym-to-search "\' not found!")))
		;; first, see if the buffer in question is already open
		(let ((buffer-was-there (m32script-try-set-buffer-cases cur-con)))
		  (if (not buffer-was-there)
			  (set-buffer (find-file-noselect
						   (concat m32script-working-directory cur-con) t)))
		  (save-excursion
			(goto-char (point-min))
			(when (re-search-forward search-str nil t)
			  (message (concat cur-con
							   ":  "
							   (replace-regexp-in-string
								"[ \t]+" " " (match-string-no-properties 0))))))
		  (if (not buffer-was-there) (kill-this-buffer))
		  ))))
;; --------------------------------

(provide 'm32script-mode)

;;; m32script-mode.el ends here
