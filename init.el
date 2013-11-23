;; -------- 配置插件 --------
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(load "base.el")
;; -------- cedet --------
(require 'cedet)
;; -------- Semantic --------
;; smart complitions
(require 'semantic/ia)
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(defconst user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public"
        "../.." "../../include" "../../inc" "../../common" "../../public"))
(defconst win32-include-dirs
   (list ""))
;;  (list "C:/MinGW/include"
;;        "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))

(let ((include-dirs user-include-dirs))
  (when (eq system-type 'windows-nt)
    (setq include-dirs (append include-dirs win32-include-dirs)))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))
;; -------- end of Semantic --------


(global-ede-mode t)
;; -------- cedet --------

;; -------- auto-complete --------
 (add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete")
 (require 'auto-complete-config)
 (add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/dict")
(ac-config-default)
;; -------- auto-complete --------
;; -------- yasnippet --------
(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet")
(require 'yasnippet )
(yas/global-mode 1) 
;; -------- end of yasnippet --------

;; -------- csMode --------
(add-to-list 'load-path "~/.emacs.d/site-lisp/csMode")
(load "csMode.el")
;; -------- end of csMode --------

;; -------- 结束配置插件 --------
