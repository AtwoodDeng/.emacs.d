;; -------- 配置插件 --------
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(load "base.el")
;; -------- 
(require 'cedet)
;; -------- Help --------
;;;; Helper tools.
(custom-set-variables
'(semantic-default-submodes (quote (global-semantic-decoration-mode global-semantic-idle-completions-mode
global-semantic-idle-scheduler-mode global-semanticdb-minor-mode
global-semantic-idle-summary-mode global-semantic-mru-bookmark-mode)))
'(semantic-idle-scheduler-idle-time 3))
;; -------- End of Help --------

;; -------- Semantic --------
;; smart complitions
(require 'semantic/ia)
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))

;;;包含设置
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/c)
(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public"
        "../.." "../../include" "../../inc" "../../common" "../../public" "../lib"))
(defconst cedet-sys-include-dirs
   (list "/usr/include"))
;;  (list "C:/MinGW/include"
;;        "C:/Program Files/Microsoft Visual Studio/VC98/MFC/Include"))

(let ((include-dirs cedet-user-include-dirs))
(setq include-dirs (append include-dirs cedet-sys-include-dirs))
(mapc (lambda (dir)
  (semantic-add-system-include dir 'c++-mode)
  (semantic-add-system-include dir 'c-mode))
  include-dirs))

(setq semantic-c-dependency-system-include-path "/usr/include/")

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
;;(add-to-list 'load-path "~/.emacs.d/site-lisp/csMode")
;;(load "csMode.el")
;; -------- end of csMode --------

;; -------- 结束配置插件 --------
