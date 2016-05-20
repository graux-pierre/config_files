(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key [M-right] 'next-buffer)
(global-set-key [M-left] 'previous-buffer)

(global-set-key (kbd "<f5>") 'recompile)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(show-paren-mode 1)

(setq inhibit-splash-screen t)
