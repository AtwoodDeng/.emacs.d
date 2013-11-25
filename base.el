;;设置威utf8

;;-------- 外观设置 --------
;; basic setting
(tool-bar-mode 0) ;;隐藏工具栏
(scroll-bar-mode 0) ;;设置

;;设置半透明 这里按f1能在半透明之间转换
(global-set-key [(f1)] 'loop-alpha)  ;;注意这行中的F1 , 可以改成你想要的按键    
    
(setq alpha-list '((85 55) (100 100)))    
    
(defun loop-alpha ()    
  (interactive)    
  (let ((h (car alpha-list)))                    
    ((lambda (a ab)    
       (set-frame-parameter (selected-frame) 'alpha (list a ab))    
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))    
       ) (car h) (car (cdr h)))    
    (setq alpha-list (cdr (append alpha-list (list h))))    
    )    
)

;;-------- 主题设置 -------- 
 (set-foreground-color "dark goldenrod")
 (set-background-color "linen")
 (set-cursor-color "gold3")
 (set-mouse-color "gold3")
;;设置字体
;;设置等宽
;; (setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2) ("WenQuanYi Zen Hei" . 1.2)))
;;启动设置
 ;;(setq default-frame-alist  '((vertical-scroll-bars)   (top . 25)    (left . 45) (width . 180)  (height . 55)
 ;;(background-color . "black") (foreground-color . "grey")(cursor-color     . "gold1") (mouse-color      . "gold1")  (tool-bar-lines . 0)  (menu-bar-lines . 1)  (right-fringe)   (left-fringe)))

 ;;启动自动最大化(数据自己调整，注意格式，如(top . 0)，圆点前后都要留有空格)
 (setq initial-frame-alist '((top . 0) (left . 0) (width . 120 ) (height . 35)))
  
 ;; 设置另外一些颜色：语法高亮显示的背景和主题，区域选择的背景和主题，二次选择的背景和选择
;; (set-face-foreground 'highlight "dark goldenrod")
 (set-face-background 'highlight "tan")
;; (set-face-foreground 'region "cyan")
 (set-face-background 'region "dark khaki")
;; (set-face-foreground 'secondary-selection "skyblue")
 (set-face-background 'secondary-selection "sandy brown")


;;-------- 结束主题设置 --------

;;------------显示时间设置------------------------------

 (display-time-mode 1);;启用时间显示设置，在minibuffer上面的那个杠上
 (setq display-time-24hr-format t);;时间使用24小时制
 (setq display-time-day-and-date t);;时间显示包括日期和具体时间
 (setq display-time-use-mail-icon t);;时间栏旁边启用邮件设置
 (setq display-time-interval 10);;时间的变化频率，单位多少来着？
 ;;------------显示时间设置结束--------------

;;-------- 结束外观设置 --------


;;-------- 操作习惯  --------

 ;;(setq visible-bell t)
 ;;关闭烦人的出错时的提示声
 
 (setq inhibit-startup-message 0)
 ;;关闭emacs启动时的画面
 
 (setq gnus-inhibit-startup-message 0)
 ;;关闭gnus启动时的画面
 
 (fset 'yes-or-no-p 'y-or-n-p)
 ;; 改变 Emacs 固执的要你回答 yes 的行为。按 y 或空格键表示 yes，n 表示 no。
  
 (setq column-number-mode t)
 (setq line-number-mode t)
 ;;显示行列号
 (global-linum-mode t)

;; Fonts setting
;; 设置两个字体变量，一个中文的一个英文的
;; 之所以两个字体大小是因为有的中文和英文相同字号的显示大小不一样，需要手动调整一下。
(setq cjk-font-size 16)
(setq ansi-font-size 16)

;; 设置一个字体集，用的是create-fontset-from-fontset-spec内置函数
;; 中文一个字体，英文一个字体混编。显示效果很好。
(defun set-font()
  (interactive)
  (create-fontset-from-fontset-spec
   (concat
    "-*-fixed-medium-r-normal-*-*-*-*-*-*-*-fontset-myfontset," 
    (format "ascii:-outline-Consolas-normal-normal-normal-mono-%d-*-*-*-c-*-iso8859-1," ansi-font-size)
    (format "unicode:-microsoft-Microsoft YaHei-normal-normal-normal-*-%d-*-*-*-*-0-iso8859-1," cjk-font-size)
    (format "chinese-gb2312:-microsoft-Microsoft YaHei-normal-normal-normal-*-%d-*-*-*-*-0-iso8859-1," cjk-font-size)
    ;; (format "unicode:-outline-文泉驿等宽微米黑-normal-normal-normal-sans-*-*-*-*-p-*-gb2312.1980-0," cjk-font-size)
    ;; (format "chinese-gb2312:-outline-文泉驿等宽微米黑-normal-normal-normal-sans-*-*-*-*-p-*-gb2312.1980-0," cjk-font-size)
    )))
 
;; 函数字体增大，每次增加2个字号，最大48号
(defun increase-font-size()
  "increase font size"
  (interactive)
  (if (< cjk-font-size 48)
      (progn
        (setq cjk-font-size (+ cjk-font-size 2))
        (setq ansi-font-size (+ ansi-font-size 2))))
  (message "cjk-size:%d pt, ansi-size:%d pt" cjk-font-size ansi-font-size)
  (set-font)
  (sit-for .5))

;; 函数字体增大，每次减小2个字号，最小2号
(defun decrease-font-size()
  "decrease font size"
  (interactive)
  (if (> cjk-font-size 2)
      (progn 
        (setq cjk-font-size (- cjk-font-size 2))
        (setq ansi-font-size (- ansi-font-size 2))))
  (message "cjk-size:%d pt, ansi-size:%d pt" cjk-font-size ansi-font-size)
  (set-font)
  (sit-for .5))

;; 恢复成默认大小16号
(defun default-font-size()
  "default font size"
  (interactive)
  (setq cjk-font-size 16)
  (setq ansi-font-size 16)
  (message "cjk-size:%d pt, ansi-size:%d pt" cjk-font-size ansi-font-size)
  (set-font)
  (sit-for .5))

;; 只在GUI情况下应用字体。Console时保持终端字体。
(if window-system
    (progn
      (set-font)
      ;; 把上面的字体集设置成默认字体
      ;; 这个字体名使用是create-fontset-from-fontset-spec函数的第一行的最后两个字段
      (set-frame-font "fontset-myfontset")

      ;; 鼠标快捷键绑定
      (global-set-key '[C-wheel-up] 'increase-font-size)
      (global-set-key '[C-wheel-down] 'decrease-font-size)
      ;; 键盘快捷键绑定
      (global-set-key (kbd "C--") 'decrease-font-size) ;Ctrl+-
      (global-set-key (kbd "C-0") 'default-font-size)  ;Ctrl+0
      (global-set-key (kbd "C-=") 'increase-font-size) ;Ctrl+=
      ))


;;询问是否递归的复制子目录
;;( dired-recursive-copies top)

;;-------- 结束操作习惯 --------
