
(load "/usr/local/ess/ess-5.2.3/lisp/ess-site"); MODIFY THIS AS NECESSARY
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(require 'tex-site)
;;(require 'hilit-LaTeX)

(setq load-path (cons "/APPS/ESS-5.0" load-path)); MODIFY THIS AS NECESSARY
(require 'ess-site)
;; (require 'viper)

(fmakunbound 'c-mode)
(makunbound 'c-mode-map)
(fmakunbound 'c++-mode)
(makunbound 'c++-mode-map)
(makunbound 'c-style-alist)

(load "cc-mode")

(setq-default transient-mark-mode t)

(global-set-key "\M-N" 'scroll-up-4)
(global-set-key "\M-P" 'scroll-down-4)
(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(auto-compression-mode t nil (jka-compr))
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode t nil (font-lock))
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )
