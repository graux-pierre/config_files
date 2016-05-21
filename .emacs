(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") 
(load-theme 'solarized-dark t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("9884f04091adebcf3017c9cfa29cd65495e0fd6eb1552b60f7c23569039e2f7d" default)))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(show-paren-mode 1)
(global-set-key [M-right] 'next-buffer)
(global-set-key [M-left] 'previous-buffer)
(put 'downcase-region 'disabled nil)
(ido-mode 1)
(global-set-key (kbd "<f5>") 'recompile)
(put 'upcase-region 'disabled nil)
(setq inhibit-splash-screen t)

;; Mutt support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))


;; Projet GL
(setq auto-mode-alist (cons '("\\.ass$" . asm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.deca$" . java-mode) auto-mode-alist))
(setq-default c-basic-offset 4 tab-width 4)  ;; Set identation
(add-to-list 'load-path "~/.emacs.D/lisp/")
(setq auto-mode-alist (cons '("\\.g4$" . antlr-mode) auto-mode-alist))
