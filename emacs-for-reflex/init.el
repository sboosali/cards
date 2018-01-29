;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;NOTES

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pseudo custom init.el

;; see https://emacs.stackexchange.com/questions/4253/how-to-start-emacs-with-a-custom-user-emacs-directory

(setq user-init-file       (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; M-: user-init-file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPORTS

(require 'package) 
(require 'use-package)
(require 'dante) 

;; NOTE `t` 
(package-initialize t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DANTE

;; :init
;; execute code before a package is loaded
;;
;; :config
;; execute code after a package is loaded 
;;
;; 
(use-package dante
  :ensure   t
  :commands dante-mode
  :after    haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

;; Company completion				| company-mode		| 
;; Type of selection				| dante-type-at		| C-c .
;; Info at point				| dante-info		| C-c ,
;; Apply Ghc suggestion for error at point	| dante-auto-fix	| C-c /
;; Goto definition				| xref-find-definitions	| M-.
;; Back from definition                         | pop-global-mark       | 
;; Find uses					| xref-find-references	| M-?
;; REPLoid (*)					| dante-eval-block	| C-c ”
;; Restart                                      | dante-restart         | 
;; Diagnossis                                   | dante-diagnose        | 

; for `xref`
(setq global-mark-ring-max 1000)

;; Customization
;; `dante-project-root`
;; `dante-repl-command-line`
;; `dante-load-flags`
;;
;; to make sure that GHCi is properly loaded by dante
;; run `M-x customize-group dante` to read the documentation
;; supports per-file or per-project customization via file-local and directory-local variables (respectively).

;; (setq-directory dante-project-root ".")

;; (setq repl-frontend
;;  "nix-shell -A shells.ghc --run 'cabal new-repl cards-frontend'")

;; (setq repl-backend
;;  "nix-shell -A shells.ghc --run 'cabal new-repl cards-backend'")

;; (setq repl-common
;;  "nix-shell -A shells.ghc --run 'cabal new-repl cards-common'")

;; (setq-directory
;;   dante-repl-command-line
;;   repl-frontend)

;; aka `./repl ghc cards-frontend`

;; NOTE there is no `ghcjsi` yet
;;
;; ;; aka `./repl ghcjs cards-frontend`
;; (setq repl-ghcjs-frontend-script "nix-shell -A shells.ghcjs --run 'cabal new-repl cards-frontend --project-file=cabal-ghcjs.project --builddir=dist-ghcjs'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DESKTOP

;;(desktop-save-mode 1)
;;(setq desktop-auto-save-timeout 5) ;; in seconds 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SAVING

(require 'real-auto-save)
(add-hook 'fundamental-mode 'real-auto-save-mode)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(add-hook 'text-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 1) ;; in seconds

(setq auto-save-visited-file-name t) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS  

(defun eval-region-or-last-sexp ()
  (interactive)
  (if (region-active-p) (call-interactively 'eval-region)
    (call-interactively 'eval-last-sexp)))

(defun darkroom-mode ()
        (interactive)
	(color-theme-retro-green)  ;; requires color-theme
        (set-face-foreground 'mode-line "gray15")
        (set-face-background 'mode-line "black")
        (auto-fill-mode 1))

(defun maximize-frame () (interactive) 
  (set-frame-parameter nil 'fullscreen 'maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDING SETTINGS

(global-set-key (kbd "<tab>") 'dabbrev-expand)
(global-set-key (kbd "<f2>")  'eval-expression)
(global-set-key (kbd "<f11>") 'pp-eval-expression) ;; eval-expression
(global-set-key (kbd "<f12>") 'execute-extended-command)
(global-set-key (kbd "<kp-insert>") 'electric-buffer-list) 
(global-set-key (kbd "<pause>") 'set-mark-command)
(global-set-key "\M-r" 'query-replace-regexp)
(global-set-key "\M-`" 'previous-buffer) ;; mnemonic: it's near ALT-TAB 

;; NOTE Windows-keyboard-specific 
(global-set-key (kbd "<apps>") 'execute-extended-command)

(global-set-key (kbd "C-M-m") 'maximize-frame) 

;;
(global-set-key (kbd "<f9>") 'pop-tag-mark)
(global-set-key (kbd "<kp-home>") 'other-window)

(global-set-key (kbd "<kp-home>")	'dante-type-at)		; C-c .
(global-set-key (kbd "<kp-end>")	'dante-info)		; C-c ,
(global-set-key (kbd "<kp-space>")     	'xref-find-definitions)	; M-.
;; (global-set-key (kbd "<kp-decimal>")     	'xref-find-definitions)	; M-.
;; (global-set-key (kbd "<kp-up>")	'xref-find-definitions)	; M-.
;; (global-set-key (kbd "<kp-down>")	'xref-find-references)	; M-?
;; (global-set-key (kbd "<kp-prior>")     	'xref-find-definitions)	; M-.
;; (global-set-key (kbd "<kp-next>") 	'xref-find-references)	; M-?
;; (global-set-key (kbd "<kp-KEY>")	'dante-eval-block)	; C-c "
;; (global-set-key (kbd "<kp-KEY>") 'dante-auto-fix) 		; C-c /

;; <kp-0>		0
;; <kp-1>		1
;; <kp-2>		2
;; <kp-3>		3
;; <kp-4>		4
;; <kp-5>		5
;; <kp-6>		6
;; <kp-7>		7
;; <kp-8>		8
;; <kp-9>		9
;; <kp-add>	+
;; <kp-begin>	<begin>
;; <kp-decimal>	.
;; <kp-delete>	<deletechar>
;; <kp-divide>	/
;; <kp-down>	<down>
;; <kp-end>	<end>
;; <kp-enter>	RET
;; <kp-equal>	=
;; <kp-home>	<home>
;; <kp-insert>	<insert>
;; <kp-left>	<left>
;; <kp-multiply>	*
;; <kp-next>	<next>
;; <kp-prior>	<prior>
;; <kp-right>	<right>
;; <kp-separator>	,
;; <kp-space>	SPC
;; <kp-subtract>	-
;; <kp-tab>	TAB
;; <kp-up>		<up>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISCELLANEOUS SETTINGS

(cua-mode t)
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

(ffap-bindings)

;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; disable automatic indentation on newlines 
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(prefer-coding-system 'utf-8)

(setq
 redisplay-dont-pause t
;;  scroll-margin 10
 scroll-step 1
;;  scroll-conservatively 10000
 scroll-preserve-screen-position 1)

(add-hook 'text-mode-hook #'turn-on-visual-line-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq mode-require-final-newline nil)
(setq require-final-newline nil)

(set-background-color "#f4f4f4")
;; https://ux.stackexchange.com/questions/8153/what-are-the-negative-and-positive-aspects-of-dark-color-scheme

;; suppresses obnoxious sights and sounds
(setq visible-bell t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FRAME, WINDOW, AND BUFFER SETTINGS

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq Buffer-menu-name-width 30)
;; (setq Buffer-menu-size-width 6)
(add-hook 'Buffer-menu-mode-hook (lambda() 
;;  (setq Buffer-menu-files-only t) 
  (revert-buffer)))
  ;; see buff-menu.el 

(set-frame-parameter nil 'title "Emacs For Reflex") 
;; the title of the operating-system-window 

(set-frame-parameter nil 'fullscreen 'maximized)
;; fullwidth, fullheight, fullboth, or maximized

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION

(find-file user-init-file) ;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom-set-variables...

