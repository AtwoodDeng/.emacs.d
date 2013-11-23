;;; csharp-mode.el --- C# mode derived mode

;; Author     : Dylan R. E. Moonfire (original)
;; Maintainer : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : Feburary 2005
;; Modified   : May 2011
;; Version    : 0.8.6
;; Keywords   : c# languages oop mode
;; X-URL      : http://code.google.com/p/csharpmode/
;; Last-saved : <2011-May-21 20:28:30>

;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;    This is a major mode for editing C# code. It performs automatic
;;    indentation of C# syntax; font locking; and integration with compile.el;
;;    flymake.el; yasnippet.el; and imenu.el.
;;
;;    csharp-mode requires CC Mode 5.30 or later.  It works with
;;    cc-mode 5.31.3, which is current at this time.
;;
;; Features:
;;
;;   - font-lock and indent of C# syntax including:
;;       all c# keywords and major syntax
;;       attributes that decorate methods, classes, fields, properties
;;       enum types
;;       #if/#endif  #region/#endregion
;;       instance initializers
;;       anonymous functions and methods
;;       verbatim literal strings (those that begin with @)
;;       generics
;;
;;   - automagic code-doc generation when you type three slashes.
;;
;;   - intelligent insertion of matched pairs of curly braces.
;;
;;   - compile tweaks. Infers the compile command from special comments
;;     in the file header.  Also, sets the regex for next-error, so that
;;     compile.el can handle csc.exe output.
;;
;;   - flymake integration
;;       - select flymake command from code comments
;;       - infer flymake command otherwise (presence of makefile, etc)
;;       - Turn off query-on-exit-flag for the flymake process.
;;       - define advice to flymake-goto-line , to allow it to goto the
;;         appropriate column for the error on a given line. This works
;;         with `flymake-goto-next-error' etc.
;;
;;   - yasnippet integration
;;       - preloaded snippets
;;
;;   - imenu integration - generates an index of namespaces, classes,
;;     interfaces, methods, and properties for easy navigation within
;;     the buffer.
;;


;; Installation instructions
;; --------------------------------
;;
;; Put csharp-mode.el somewhere in your load path, optionally byte-compile
;; it, and add the following to your .emacs file:
;;
;;   (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;;   (setq auto-mode-alist
;;      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;;
;;
;; Optionally, define and register a mode-hook function. To do so, use
;; something like this in your .emacs file:
;;
;;   (defun my-csharp-mode-fn ()
;;      "function that runs when csharp-mode is initialized for a buffer."
;;      (turn-on-auto-revert-mode)
;;      (setq indent-tabs-mode nil)
;;      (require 'flymake)
;;      (flymake-mode 1)
;;      (require 'yasnippet)
;;      (yas/minor-mode-on)
;;      (require 'rfringe)
;;      ...insert more code here...
;;      ...including any custom key bindings you might want ...
;;   )
;;   (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)
;;
;;
;;  General
;;  ----------------------------
;;
;;  Mostly C# mode will "just work."  Use `describe-mode' to see the
;;  default keybindings and the highlights of the mode.
;;
;;
;;  Flymake Integration
;;  ----------------------------
;;
;;  You can use flymake with csharp mode to automatically check the
;;  syntax of your csharp code, and highlight errors.  To do so, add a
;;  comment line like this to each .cs file that you use flymake with:
;;
;;   //  flymake: c:\.net3.5\csc.exe /t:module /nologo /R:Foo.dll @@FILE@@
;;
;;  That lines specifies a command "stub".  Flymake appends the name of
;;  the file to compile, and then runs the command to check
;;  syntax. Flymake assumes that syntax errors will be noted in the
;;  output of the command in a form that fits one of the regexs in the
;;  `compilation-error-regexp-alist-alist'. Check the flymake module for
;;  more information on that.
;;
;;  Some rules for the command:
;;
;;    1. it must appear all on a single line.
;;
;;    2. csharp-mode generally looks for the marker line in the first N
;;       lines of the file, where N is set in
;;       `csharp-cmd-line-limit'.  See the documentation on that
;;       variable for more information.
;;
;;    3. the command SHOULD use @@FILE@@ in place of the name of the
;;       source file to be compiled, normally the file being edited.
;;       This is because normally flymake saves a copy of the buffer
;;       into a temporary file with a unique name, and then compiles
;;       that temporary file. The token @@FILE@@ is replaced by
;;       csharp-mode with the name of the temporary file created by
;;       flymake, before invoking the command.
;;
;;    4. The command should include /R options specifying external
;;       libraries that the code depends on.
;;
;;  If you have no external dependencies, then you need not specify any
;;  flymake command at all. csharp-mode will implicitly act as if you had
;;  specified the command:
;;
;;      // flymake: c:\.net3.5\csc.exe /t:module /nologo @@FILE@@
;;
;;
;;  If you use csc.exe as the syntax check tool (as almost everyone
;;  will), the /t:module is important. csharp-mode assumes that the
;;  syntax-check compile command will produce a file named
;;  NAME.netmodule, which is the default when using /t:module. (Remember
;;  than NAME is dynamically generated).  csharp-mode will remove the
;;  generated netmodule file after the syntax check is complete. If you
;;  don't specify /t:module, then csharp-mode won't know what file to
;;  delete.
;;
;;  csharp-mode also fiddles with some other flymake things.  In
;;  particular it: adds .cs to the flymake "allowed filename masks";
;;  adds parsing for csc error messages; and adds advice to the error
;;  parsing logic. This all should be pretty benign for all other
;;  flymake buffers.  But it might not be.
;;
;;  You can explicitly turn the flymake integration for C# off by
;;  setting `csharp-want-flymake-fixup' to nil.
;;
;;
;;  Compile Integration
;;  ----------------------------
;;
;;  csharp-mode binds the function `csharp-invoke-compile-interactively'
;;  to "\C-x\C-e" .  This function attempts to intellgently guess the
;;  format of the compile command to use for a buffer.  It looks in the
;;  comments at the head of the buffer for a line that begins with
;;  compile: .  If found, csharp-mode suggests the text that follows as
;;  the compilation command when running `compile' .  If such a line is
;;  not found, csharp-mode falls back to a msbuild or nmake command.
;;  See the documentation on `csharp-cmd-line-limit' for further
;;  information.
;;
;;  Also, csharp-mode installs an error regexp for csc.exe into
;;  `compilation-error-regexp-alist-alist', which allows `next-error'
;;  and `previous-error' (defined in compile.el) to navigate to the next
;;  and previous compile errors in the cs buffer, after you've run `compile'.
;;
;;
;;  YASnippet integration
;;  -----------------------------
;;
;;  csharp-mode defines some built-in snippets for
;;  convenience.  For example, if statements, for, foreach, and
;;  so on.  You can see them on the YASnippet menu that is displayed
;;  when a csharp-mode buffer is opened.  csharp-mode defines this
;;  snippets happens only if ya-snippet is available. (It is done in an
;;  `eval-after-load' clause.)  The builtin snippets will not overwrite
;;  snippets that use the same name, if they are defined in the normal
;;  way (in a compiled bundle) with ya-snippet.
;;
;;  You can explicitly turn off ya-snippet integration. See the var,
;;  `csharp-want-yasnippet-fixup'.
;;
;;
;;  imenu integration
;;  -----------------------------
;;
;;  This should just work. For those who don't know what imenu is, it
;;  allows navigation to different points within the file from an
;;  "Index" menu, in the window's menubar.  csharp-mode computes the
;;  menu containing the namespaces, classes, methods, and so on, in the
;;  buffer.  This happens at the time the file is loaded; for large
;;  files it takes a bit of time to complete the scan.  If you don't
;;  want this capability, set `csharp-want-imenu' to nil.
;;
;;


;;; Known Bugs:
;;
;;   The imenu scan is text-based and naive. For example, if you
;;   intersperse comments between the name of a class/method/namespace,
;;   and the curly brace, the scan will not recognize the thing being
;;   declared. This is fixable - would need to extract the buffer
;;   substring then remove comments before doing the regexp checks - but
;;   it would make the scan much slower.  Also, the scan doesn't deal
;;   with preproc symbol definitions and #if/#else. Those things are
;;   invisible to the scanner csharp-mode uses to build the imenu menu.
;;
;;   Leading identifiers are no longer being fontified, for some reason.
;;   See matchers-before. (Not sure this is still a problem - 19 may
;;   2011 DPC)
;;
;;   Method names with a preceding attribute are not fontified.
;;
;;   The symbol followng #if is not fontified.  It should be treated like
;;   define and get font-lock-variable-name-face .
;;
;;   This code doesn't seem to work when you compile it, then
;;   load/require in the emacs file. You will get an error (error
;;   "`c-lang-defconst' must be used in a file") which happens because
;;   cc-mode doesn't think it is in a buffer while loading directly
;;   from the init. However, if you call it based on a file extension,
;;   it works properly. Interestingly enough, this doesn't happen if
;;   you don't byte-compile cc-mode.
;;
;;
;;
;;  Todo:
;;
;;   imenu should scan for and find delegates and events, in addition
;;   to the classes, structs, properties and methods it does currently.
;;
;;   Get csharp-mode.el accepted as part of the emacs standard distribution.
;;   Must contact monnier at iro.umontreal.ca to make this happen.
;;
;;   Add refactoring capabilities?
;;     - extract as method - extract a block of code into a method
;;     - extract as Func<> - extract a block of code into an Action<T>
;;
;;   More code-gen power:
;;     - interface implementation - I think would require csharp-shell
;;
;;
;;  Acknowledgements:
;;
;;    Thanks to Alan Mackenzie and Stefan Monnier for answering questions
;;    and making suggestions. And to Trey Jackson for sharing his
;;    knowledge of emacs lisp.
;;
;;

;;; Versions:
;;
;;    0.1.0 - Initial release.
;;    0.2.0 - Fixed the identification on the "enum" keyword.
;;          - Fixed the font-lock on the "base" keyword
;;    0.3.0 - Added a regex to fontify attributes. It isn't the
;;            the best method, but it handles single-like attributes
;;            well.
;;          - Got "super" not to fontify as a keyword.
;;          - Got extending classes and interfaces to fontify as something.
;;    0.4.0 - Removed the attribute matching because it broke more than
;;            it fixed.
;;          - Corrected a bug with namespace not being properly identified
;;            and treating the class level as an inner object, which screwed
;;            up formatting.
;;          - Added "partial" to the keywords.
;;    0.5.0 - Found bugs with compiled cc-mode and loading from init files.
;;          - Updated the eval-when-compile to code to let the mode be
;;            compiled.
;;    0.6.0 - Added the c-filter-ops patch for 5.31.1 which made that
;;            function in cc-langs.el unavailable.
;;          - Added a csharp-lineup-region for indention #region and
;;            #endregion block differently.
;;    0.7.0 - Added autoload so update-directory-autoloads works
;;            (Thank you, Nikolaj Schumacher)
;;          - Fontified the entire #region and #endregion lines.
;;          - Initial work to get get, set, add, remove font-locked.
;;    0.7.1 - Added option to indent #if/endif with code
;;          - Fixed c-opt-cpp-prefix defn (it must not include the BOL
;;            char (^).
;;          - proper fontification and indent of classes that inherit
;;            (previously the colon was confusing the parser)
;;          - reclassified namespace as a block beginner
;;          - removed $ as a legal symbol char - not legal in C#.
;;          - added struct to c-class-decl-kwds so indent is correct
;;            within a struct.
;;    0.7.2 - Added automatic codedoc insertion.
;;    0.7.3 - Instance initializers (new Type { ... } ) and
;;            (new Type() { ...} ) are now indented properly.
;;          - proper fontification and indent of enums as brace-list-*,
;;            including special treatment for enums that explicitly
;;            inherit from an int type. Previously the colon was
;;            confusing the parser.
;;          - proper fontification of verbatim literal strings,
;;            including those that end in slash. This edge case was not
;;            handled at all before; it is now handled correctly.
;;          - code cleanup and organization; removed the formfeed.
;;          - intelligent curly-brace insertion with
;;            `csharp-insert-open-brace'
;;    0.7.4 - added a C# style
;;          - using is now a keyword and gets fontified correctly
;;          - fixed a bug that had crept into the codedoc insertion.
;;    0.7.5 - now fontify namespaces in the using statements. This is
;;            done in the csharp value for c-basic-matchers-before .
;;          - also fontify the name following namespace decl.
;;            This is done in the csharp value for c-basic-matchers-after .
;;          - turn on recognition of generic types. They are now
;;            fontified correctly.
;;          - <> are now treated as syntactic parens and can be jumped
;;            over with c-forward-sexp.
;;          - Constructors are now fontified.
;;          - Field/Prop names inside object initializers are now fontified.
;;
;;    0.7.7 - relocate running c-run-mode-hooks to the end of
;;            csharp-mode, to allow user to modify key bindings in a
;;            hook if he doesn't like the defaults.
;;
;;    0.7.8 - redefine csharp-log to insert timestamp.
;;          - Fix byte-compile errors on emacs 23.2 ?  Why was
;;            c-filter-ops duplicated here?  What was the purpose of its
;;            presence here, I am not clear.
;;
;;    0.8.0 - include flymake magic into this module.
;;          - include yasnippet integration
;;
;;    0.8.2 2011 April DPC
;;          - small tweaks; now set a one-time bool for flymake installation
;;          - some doc updates on flymake
;;
;;    0.8.3 2011 May 17  DPC
;;          - better help on csharp-mode
;;          - csharp-move-* functions for manual navigation.
;;          - imenu integration for menu-driven navigation - navigate to
;;            named methods, classes, etc.
;;          - adjusted the flymake regexp to handle output from fxcopcmd,
;;            and extended the help to provide examples how to use this.
;;
;;    0.8.4 DPC 2011 May 18
;;          - fix a basic bug in the `csharp-yasnippet-fixup' fn.
;;
;;    0.8.5 DPC 2011 May 21
;;          - imenu: correctly parse Properties that are part of an
;;            explicitly specified interface. Probably need to do this
;;            for methods, too.
;;          - fontify the optional alias before namespace in a using (import).
;;          - Tweak open-curly magic insertion for object initializers.
;;          - better fontification of variables and references
;;          - "sealed" is now fontified as a keyword
;;          - imenu: correctly index ctors that call this or base.
;;          - imenu: correctly index Extension methods (this System.Enum e)
;;          - imenu: correctly scan  method params tagged with out, ref, params
;;          - imenu scan: now handle curlies within strings.
;;          - imenu: split menus now have better labels, are sorted correctly.
;;
;;    0.8.6 DPC 2011 May ??
;;          -


(require 'cc-mode)

(message  (concat "Loading " load-file-name))


;; ==================================================================
;; c# upfront stuff
;; ==================================================================

;; This is a copy of the function in cc-mode which is used to handle the
;; eval-when-compile which is needed during other times.
;;
;; NB: I think this is needed to satisfy requirements when this module
;; calls `c-lang-defconst'. (DPC)

;; (defun c-filter-ops (ops opgroup-filter op-filter &optional xlate)
;;   ;; See cc-langs.el, a direct copy.
;;   (unless (listp (car-safe ops))
;;     (setq ops (list ops)))
;;   (cond ((eq opgroup-filter t)
;;          (setq opgroup-filter (lambda (opgroup) t)))
;;         ((not (functionp opgroup-filter))
;;          (setq opgroup-filter `(lambda (opgroup)
;;                                  (memq opgroup ',opgroup-filter)))))
;;   (cond ((eq op-filter t)
;;          (setq op-filter (lambda (op) t)))
;;         ((stringp op-filter)
;;          (setq op-filter `(lambda (op)
;;                             (string-match ,op-filter op)))))
;;   (unless xlate
;;     (setq xlate 'identity))
;;   (c-with-syntax-table (c-lang-const c-mode-syntax-table)
;;     (delete-duplicates
;;      (mapcan (lambda (opgroup)
;;                (when (if (symbolp (car opgroup))
;;                          (when (funcall opgroup-filter (car opgroup))
;;                            (setq opgroup (cdr opgroup))
;;                            t)
;;                        t)
;;                  (mapcan (lambda (op)
;;                            (when (funcall op-filter op)
;;                              (let ((res (funcall xlate op)))
;;                                (if (listp res) res (list res)))))
;;                          opgroup)))
;;              ops)
;;      :test 'equal)))



;; These are only required at compile time to get the sources for the
;; language constants.  (The load of cc-fonts and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)

(eval-when-compile
  (let ((load-path
         (if (and (boundp 'byte-compile-dest-file)
                  (stringp byte-compile-dest-file))
             (cons (file-name-directory byte-compile-dest-file) load-path)
           load-path)))
    (load "cc-mode" nil t)
    (load "cc-fonts" nil t)
    (load "cc-langs" nil t)))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'csharp-mode 'java-mode))

;; ==================================================================
;; end of c# upfront stuff
;; ==================================================================





;; ==================================================================
;; constants used in this module
;; ==================================================================

;;(error (byte-compile-dest-file))
;;(error (c-get-current-file))

(defconst csharp-aspnet-directive-re
  "<%@.+?%>"
  "Regex for matching directive blocks in ASP.NET files (.aspx, .ashx, .ascx)")


(defconst csharp-enum-decl-re
  (concat
   "\\<enum[ \t\n\r\f\v]+"
   "\\([[:alpha:]_][[:alnum:]_]*\\)"
   "[ \t\n\r\f\v]*"
   "\\(:[ \t\n\r\f\v]*"
   "\\("
   (c-make-keywords-re nil
     (list "sbyte" "byte" "short" "ushort" "int" "uint" "long" "ulong"))
   "\\)"
   "\\)?")
  "Regex that captures an enum declaration in C#"
  )

;; ==================================================================






;; ==================================================================
;; csharp-mode utility and feature defuns
;; ==================================================================

(defun csharp--at-vsemi-p (&optional pos)
  "Determines if there is a virtual semicolon at POS or point.
It returns t if at a position where a virtual-semicolon is.
Otherwise nil.

This is the C# version of the function. It gets set into
the variable `c-at-vsemi-p-fn'.

A vsemi is a cc-mode concept implying the end of a statement,
where no actual end-of-statement signifier character ( semicolon,
close-brace) appears.  The concept is used to allow proper
indenting of blocks of code: Where a vsemi appears, the following
line will not indent further.

A vsemi appears in 3 cases in C#:

 - after an attribute that decorates a class, method, field, or
   property.

 - in an object initializer, before the open-curly?

 - after an ASPNET directive, that appears in a aspx/ashx/ascx file

An example of the former is  [WebMethod] or [XmlElement].
An example of the latter is something like this:

    <%@ WebHandler Language=\"C#\" Class=\"Handler\" %>

Providing this function allows the indenting in csharp-mode
to work properly with code that includes attributes and ASPNET
directives.

"
  (save-excursion
    (let ((pos-or-point (progn (if pos (goto-char pos)) (point))))

      (cond

       ;; before open curly in object initializer. new Foo* { }
       ((and (looking-back
              (concat "\\<new[ \t\n\f\v\r]+"
              "\\(?:[A-Za-z_][[:alnum:]]*\\.\\)*"
              "[A-Za-z_][[:alnum:]]*[\ t\n\f\v\r]*"))
             (looking-at "[ \t\n\f\v\r]*{"))
        t)

       ;; put a vsemi after an ASPNET directive, like
       ;; <%@ WebHandler Language="C#" Class="Handler" %>
       ((looking-back (concat csharp-aspnet-directive-re "$") nil t)
        t)

       ;; put a vsemi after an attribute, as with
       ;;   [XmlElement]
       ;; Except when the attribute is used within a line of code, as
       ;; specifying something for a parameter.
       ((c-safe (backward-sexp) t)
        (cond
           ((re-search-forward
             (concat
              "\\(\\["
              "[ \t\n\r\f\v]*"
              "\\("
              "\\(?:[A-Za-z_][[:alnum:]]*\\.\\)*"
              "[A-Za-z_][[:alnum:]]*"
              "\\)"
              "[^]]*\\]\\)"
              )
             (1+ pos-or-point) t)

             (c-safe (backward-sexp))
             (c-backward-syntactic-ws)
             (cond

              ((eq (char-before) 93) ;; close sq brace (a previous attribute)
               (csharp--at-vsemi-p (point))) ;; recurse

              ((or
                (eq (char-before) 59) ;; semicolon
                (eq (char-before) 123) ;; open curly
                (eq (char-before) 125)) ;; close curly
               t)

              ;; attr is used within a line of code
              (t nil)))

           (t nil)))

        (t nil))
      )))




(defun csharp-lineup-region (langelem)
  "Indent all #region and #endregion blocks inline with code while
retaining normal column-zero indention for #if and the other
processing blocks.

To use this indenting just put the following in your emacs file:
   (c-set-offset 'cpp-macro 'csharp-lineup-region)

An alternative is to use `csharp-lineup-if-and-region'.
"

  (save-excursion
    (back-to-indentation)
    (if (re-search-forward "#\\(end\\)?region" (c-point 'eol) [0]) 0  [0])))





(defun csharp-lineup-if-and-region (langelem)

"Indent all #region/endregion blocks and #if/endif blocks inline
with code while retaining normal column-zero indention for any
other processing blocks.

To use this indenting just put the following in your emacs file:
  (c-set-offset 'cpp-macro 'csharp-lineup-if-and-region)

Another option is to use `csharp-lineup-region'.

"
  (save-excursion
    (back-to-indentation)
    (if (re-search-forward "#\\(\\(end\\)?\\(if\\|region\\)\\|else\\)" (c-point 'eol) [0]) 0  [0])))




  (defun csharp-in-literal (&optional lim detect-cpp)
    "Return the type of literal point is in, if any.
Basically this works like `c-in-literal' except it doesn't
use or fill the cache (`c-in-literal-cache').

The return value is a symbol: `c' if in a C-style comment, `c++'
if in a C++ style comment, `string' if in a string literal,
`pound' if DETECT-CPP is non-nil and in a preprocessor line, or
nil if somewhere else.  Optional LIM is used as the backward
limit of the search.  If omitted, or nil, `c-beginning-of-syntax'
is used.

Note that this function might do hidden buffer changes.  See the
comment at the start of cc-engine.el for more info."

    (let ((rtn
           (save-excursion
             (let* ((pos (point))
                    (lim (or lim (progn
                                   (c-beginning-of-syntax)
                                   (point))))
                    (state (parse-partial-sexp lim pos)))
               (csharp-log 4 "parse lim(%d) state: %s" lim (prin1-to-string state))
               (cond
                ((elt state 3)
                 (csharp-log 4 "in literal string (%d)" pos)
                 'string)
                ((elt state 4)
                 (csharp-log 4 "in literal comment (%d)" pos)
                 (if (elt state 7) 'c++ 'c))
                ((and detect-cpp (c-beginning-of-macro lim)) 'pound)
                (t nil))))))
      rtn))



(defun csharp-insert-open-brace ()
  "Intelligently insert a pair of curly braces. This fn should be
bound to the open-curly brace, with

    (local-set-key (kbd \"{\") 'csharp-insert-open-brace)

The default binding for an open curly brace in cc-modes is often
`c-electric-brace' or `skeleton-pair-insert-maybe'.  The former
can be configured to insert newlines around braces in various
syntactic positions.  The latter inserts a pair of braces and
then does not insert a newline, and does not indent.

This fn provides another option, with some additional
intelligence for csharp-mode.  When you type an open curly, the
appropriate pair of braces appears, with spacing and indent set
in a context-sensitive manner:

 - Within a string literal, you just get a pair of braces, and
   point is set between them. This works for String.Format()
   purposes.

 - Following = or [], as in an array assignment, you get a pair
   of braces, with two intervening spaces, with a semincolon
   appended. Point is left between the braces.

 - Following \"new Foo\", it's an object initializer. You get:
   newline, open brace, newline, newline, close, semi.  Point is
   left on the blank line between the braces. Unless the object
   initializer is within an array initializer, in which case, no
   newlines, and the semi is replaced with a comma. (Try it to
   see what this means).

 - Following => , implying a lambda, you get an open/close pair,
   with two intervening spaces, no semicolon, and point on the
   2nd space.

 - Otherwise, you get a newline, the open curly, followed by
   an empty line and the closing curly on the line following,
   with point on the empty line.


There may be another way to get this to happen appropriately just
within emacs, but I could not figure out how to do it.  So I
wrote this alternative.

    "
  (interactive)
  (let
      (tpoint
       (in-string (string= (csharp-in-literal) "string"))
       (preceding3
        (save-excursion
          (and
           (skip-chars-backward " \t")
           (> (- (point) 2) (point-min))
           (buffer-substring-no-properties (point) (- (point) 3)))))
       (one-word-back
        (save-excursion
          (backward-word 2)
          (thing-at-point 'word))))

    (cond

     ;; Case 1: inside a string literal?
     ;; --------------------------------------------
     ;; If so, then just insert a pair of braces and put the point
     ;; between them.  The most common case is a format string for
     ;; String.Format() or Console.WriteLine().
     (in-string
      (self-insert-command 1)
      (insert "}")
      (backward-char))

     ;; Case 2: the open brace starts an array initializer.
     ;; --------------------------------------------
     ;; When the last non-space was an equals sign or square brackets,
     ;; then it's an initializer.
     ((save-excursion
        (and (c-safe (backward-sexp) t)
             (looking-at "\\(\\w+\\b *=\\|[[]]+\\)")))
      (self-insert-command 1)
      (insert "  };")
      (backward-char 3))

     ;; Case 3: the open brace starts an instance initializer
     ;; --------------------------------------------
     ;; If one-word-back was "new", then it's an object initializer.
     ((string= one-word-back "new")
      (csharp-log 2 "object initializer")
      (setq tpoint (point)) ;; prepare to indent-region later
      (backward-word 2)
      (c-backward-syntactic-ws)
      (if (or (eq (char-before) ?,)       ;; comma
              (and (eq (char-before) 123) ;; open curly
                   (progn (backward-char)
                          (c-backward-syntactic-ws)
                          (looking-back "\\[\\]"))))
          (progn
            ;; within an array - emit no newlines
            (goto-char tpoint)
            (self-insert-command 1)
            (insert "  },")
            (backward-char 3))

        (progn
          (goto-char tpoint)
          (newline)
          (self-insert-command 1)
          (newline-and-indent)
          (newline)
          (insert "};")
          (c-indent-region tpoint (point))
          (forward-line -1)
          (indent-according-to-mode)
          (end-of-line))))


     ;; Case 4: a lambda initialier.
     ;; --------------------------------------------
     ;; If the open curly follows =>, then it's a lambda initializer.
     ((string= (substring preceding3 -2) "=>")
      (csharp-log 2 "lambda init")
      (self-insert-command 1)
      (insert "  }")
      (backward-char 2))

     ;; else, it's a new scope. (if, while, class, etc)
     (t
      (save-excursion
        (csharp-log 2 "new scope")
        (set-mark (point)) ;; prepare to indent-region later
        ;; check if the prior sexp is on the same line
        (if (save-excursion
              (let ((curline (line-number-at-pos))
                    (aftline (progn
                               (if (c-safe (backward-sexp) t)
                                   (line-number-at-pos)
                                 -1))))
                (= curline aftline)))
            (newline-and-indent))
        (self-insert-command 1)
        (c-indent-line-or-region)
        (end-of-line)
        (newline)
        (insert "}")
        ;;(c-indent-command) ;; not sure of the difference here
        (c-indent-line-or-region)
        (forward-line -1)
        (end-of-line)
        (newline-and-indent)
        ;; point ends up on an empty line, within the braces, properly indented
        (setq tpoint (point)))

      (goto-char tpoint)))))


;; ==================================================================
;; end of csharp-mode utility and feature defuns
;; ==================================================================



;; ==================================================================
;; c# values for "language constants" defined in cc-langs.el
;; ==================================================================

(c-lang-defconst c-at-vsemi-p-fn
  csharp 'csharp--at-vsemi-p)


;; This c-opt-after-id-concat-key is a regexp that matches
;; dot.  In other words: "\\(\\.\\)"
;; Not sure why this needs to be so complicated.
;; This const is now internal (obsolete); need to move to
;; c-after-id-concat-ops.  I don't yet understand the meaning
;; of that variable, so for now. . .  .

;; (c-lang-defconst c-opt-after-id-concat-key
;;   csharp (if (c-lang-const c-opt-identifier-concat-key)
;;              (c-lang-const c-symbol-start)))

(c-lang-defconst c-opt-after-id-concat-key
  csharp "[[:alpha:]_]" )




;; The matchers elements can be of many forms.  It gets pretty
;; complicated.  Do a describe-variable on font-lock-keywords to get a
;; description.  (Why on font-lock-keywords? I don't know, but that's
;; where you get the help.)
;;
;; Aside from the provided documentation, the other option of course, is
;; to look in the source code as an example for what to do.  The source
;; in cc-fonts uses a defun c-make-font-lock-search-function to produce
;; most of the matchers.  Called this way:
;;
;;   (c-make-font-lock-search-function  regexp '(A B c))
;;
;; The REGEXP is used in re-search-forward, and if there's a match, then
;; A is called within a save-match-data. If B and C are non-nil, they
;; are called as pre and post blocks, respecitvely.
;;
;; Anyway the c-make-font-lock-search-function works for a single regex,
;; but more complicated scenarios such as those intended to match and
;; fontify object initializers, call for a hand-crafted lambda.
;;
;; The object initializer is special because matching on it must
;; allow nesting.
;;
;; In c#, the object initializer block is used directly after a
;; constructor, like this:
;;
;;     new MyType
;;     {
;;        Prop1 = "foo"
;;     }
;;
;; csharp-mode needs to fontify the properties in the
;; initializer block in font-lock-variable-name-face. The key thing is
;; to set the text property on the open curly, using type c-type and
;; value c-decl-id-start. This apparently allows `parse-partial-sexp' to
;; do the right thing, later.
;;
;; This simple case is easy to handle in a regex, using the basic
;; `c-make-font-lock-search-function' form.  But the general syntax for a
;; constructor + object initializer in C# is more complex:
;;
;;     new MyType(..arglist..) {
;;        Prop1 = "foo"
;;     }
;;
;; A simple regex match won't satisfy here, because the ..arglist.. can
;; be anything, including calls to other constructors, potentially with
;; object initializer blocks. This may nest arbitrarily deeply, and the
;; regex in emacs doesn't support balanced matching.  Therefore there's
;; no way to match on the "outside" pair of parens, to find the relevant
;; open curly.  What's necessary is to do the match on "new MyType" then
;; skip over the sexp defined by the parens, then set the text property on
;; the appropriate open-curly.
;;
;; To make that happen, it's good to have insight into what the matcher
;; really does.  The output of `c-make-font-lock-search-function' before
;; byte-compiling, is:
;;
;; (lambda (limit)
;;   (let ((parse-sexp-lookup-properties
;;          (cc-eval-when-compile
;;            (boundp 'parse-sexp-lookup-properties))))
;;     (while (re-search-forward REGEX limit t)
;;       (unless
;;           (progn
;;             (goto-char (match-beginning 0))
;;             (c-skip-comments-and-strings limit))
;;         (goto-char (match-end 0))
;;         (progn
;;           B
;;           (save-match-data A)
;;           C ))))
;;   nil)
;;
;; csharp-mode uses this hand-crafted form of a matcher to handle the
;; general case for constructor + object initializer, within
;; `c-basic-matchers-after' .
;;




;; (defun c-make-font-lock-search-function (regexp &rest highlights)
;;     ;; This function makes a byte compiled function that works much like
;;     ;; a matcher element in `font-lock-keywords'.  It cuts out a little
;;     ;; bit of the overhead compared to a real matcher.  The main reason
;;     ;; is however to pass the real search limit to the anchored
;;     ;; matcher(s), since most (if not all) font-lock implementations
;;     ;; arbitrarily limits anchored matchers to the same line, and also
;;     ;; to insulate against various other irritating differences between
;;     ;; the different (X)Emacs font-lock packages.
;;     ;;
;;     ;; REGEXP is the matcher, which must be a regexp.  Only matches
;;     ;; where the beginning is outside any comment or string literal are
;;     ;; significant.
;;     ;;
;;     ;; HIGHLIGHTS is a list of highlight specs, just like in
;;     ;; `font-lock-keywords', with these limitations: The face is always
;;     ;; overridden (no big disadvantage, since hits in comments etc are
;;     ;; filtered anyway), there is no "laxmatch", and an anchored matcher
;;     ;; is always a form which must do all the fontification directly.
;;     ;; `limit' is a variable bound to the real limit in the context of
;;     ;; the anchored matcher forms.
;;     ;;
;;     ;; This function does not do any hidden buffer changes, but the
;;     ;; generated functions will.  (They are however used in places
;;     ;; covered by the font-lock context.)
;;
;;     ;; Note: Replace `byte-compile' with `eval' to debug the generated
;;     ;; lambda easier.
;;     (byte-compile
;;      `(lambda (limit)
;;         (let (;; The font-lock package in Emacs is known to clobber
;;               ;; `parse-sexp-lookup-properties' (when it exists).
;;               (parse-sexp-lookup-properties
;;                (cc-eval-when-compile
;;                  (boundp 'parse-sexp-lookup-properties))))
;;           (while (re-search-forward ,regexp limit t)
;;             (unless (progn
;;                       (goto-char (match-beginning 0))
;;                       (c-skip-comments-and-strings limit))
;;               (goto-char (match-end 0))
;;               ,@(mapcar
;;                  (lambda (highlight)
;;                    (if (integerp (car highlight))
;;                        (progn
;;                          (unless (eq (nth 2 highlight) t)
;;                            (error
;;                             "The override flag must currently be t in %s"
;;                             highlight))
;;                          (when (nth 3 highlight)
;;                            (error
;;                             "The laxmatch flag may currently not be set in %s"
;;                             highlight))
;;                          `(save-match-data
;;                             (c-put-font-lock-face
;;                              (match-beginning ,(car highlight))
;;                              (match-end ,(car highlight))
;;                              ,(elt highlight 1))))
;;                      (when (nth 3 highlight)
;;                        (error "Match highlights currently not supported in %s"
;;                               highlight))
;;                      `(progn
;;                         ,(nth 1 highlight)
;;                         (save-match-data ,(car highlight))
;;                         ,(nth 2 highlight))))
;;                  highlights))))
;;         nil))
;;     )


(c-lang-defconst c-basic-matchers-before
  csharp `(
           ;;;; Font-lock the attributes by searching for the
           ;;;; appropriate regex and marking it as TODO.
           ;;,`(,(concat "\\(" csharp-attribute-regex "\\)")
           ;;   0 font-lock-function-name-face)

           ;; Put a warning face on the opener of unclosed strings that
           ;; can't span lines.  Later font
           ;; lock packages have a `font-lock-syntactic-face-function' for
           ;; this, but it doesn't give the control we want since any
           ;; fontification done inside the function will be
           ;; unconditionally overridden.
           ,(c-make-font-lock-search-function
             ;; Match a char before the string starter to make
             ;; `c-skip-comments-and-strings' work correctly.
             (concat ".\\(" c-string-limit-regexp "\\)")
             '((c-font-lock-invalid-string)))


           ;; Fontify keyword constants.
           ,@(when (c-lang-const c-constant-kwds)
               (let ((re (c-make-keywords-re nil
                           (c-lang-const c-constant-kwds))))
                 `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
                                 1 c-constant-face-name)))))


           ;; Fontify the namespaces that follow using statements.
           ;; This regex handles the optional alias, as well.
           ,`(,(concat
                "\\<\\(using\\)[ \t\n\f\v\r]+"
                "\\(?:"
                "\\([A-Za-z_][[:alnum:]]*\\)"
                "[ \t\n\f\v\r]*="
                "[ \t\n\f\v\r]*"
                "\\)?"
                "\\(\\(?:[A-Za-z_][[:alnum:]]*\\.\\)*[A-Za-z_][[:alnum:]]*\\)"
                "[ \t\n\f\v\r]*;")
              (2 font-lock-constant-face t t)
              (3 font-lock-constant-face))


           ;; Fontify all keywords except the primitive types.
           ,`(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
              1 font-lock-keyword-face)


           ;; Fontify leading identifiers as a reference? in fully
           ;; qualified names like "Foo.Bar".
           ,@(when (c-lang-const c-opt-identifier-concat-key)
               `((,(byte-compile
                    `(lambda (limit)
                       (csharp-log 3 "bmb reference? p(%d) L(%d)" (point) limit)
                       (while (re-search-forward
                               ,(concat "\\(\\<" ;; 1
                                        "\\("  ;; 2
                                        ;;"[A-Z]";; uppercase - assume upper = classname
                                        "[A-Za-z_]"  ;; any old
                                        "[A-Za-z0-9_]*" ;; old: (c-lang-const c-symbol-key)
                                        "\\)"
                                        "[ \t\n\r\f\v]*"
                                        "\\."   ;;(c-lang-const c-opt-identifier-concat-key)
                                        "[ \t\n\r\f\v]*"
                                        "\\)" ;; 1 ends
                                        "\\("
                                        "[[:alpha:]_][A-Za-z0-9_]*" ;; start of another symbolname
                                        "\\)"  ;; 3 ends
                                        )
                               limit t)
                         (csharp-log 3 "bmb ref? B(%d)" (match-beginning 0))
                         (unless (progn
                                   (goto-char (match-beginning 0))
                                   (c-skip-comments-and-strings limit))
                           (let* ((prefix  (match-string 2))
                                  (me1 (match-end 1))
                                  (first-char (string-to-char prefix))
                                  (is-upper (and (>= first-char 65)
                                                 (<= first-char 90))))
                             (csharp-log 3 "  - class/intf ref (%s)" prefix)
                             ;; only put face if not there already
                             (or (get-text-property (match-beginning 2) 'face)
                                 (c-put-font-lock-face (match-beginning 2)
                                                       (match-end 2)
                                                       (if is-upper
                                                           font-lock-type-face ;; it's a type!
                                                         font-lock-variable-name-face)))

                             (goto-char (match-end 3))
                             (c-forward-syntactic-ws limit)

                             ;; now, maybe fontify the thing afterwards, too
                             (let ((c (char-after)))
                               (csharp-log 3 "  - now lkg at c(%c)" c)

                               (cond

                                ((= c 40) ;; open paren
                                 (or (get-text-property (match-beginning 3) 'face)
                                     (c-put-font-lock-face (match-beginning 3)
                                                           (match-end 3)
                                                           font-lock-function-name-face))
                                 (goto-char (match-end 3)))

                                ;;  these all look like variables or properties
                                ((or (= c 59)  ;; semicolon
                                     (= c 91)  ;; open sq brack
                                     (= c 41)  ;; close paren
                                     (= c 44)  ;; ,
                                     (= c 33)  ;; !
                                     (= c 124) ;; |
                                     (= c 61)  ;; =
                                     (= c 43)  ;; +
                                     (= c 45)  ;; -
                                     (= c 42)  ;; *
                                     (= c 47)) ;; /
                                 (or (get-text-property (match-beginning 3) 'face)
                                     (c-put-font-lock-face (match-beginning 3)
                                                           (match-end 3)
                                                           font-lock-variable-name-face))
                                 (goto-char (match-end 3)))

                                (t
                                 (goto-char (match-end 1)))))))))))))

           ))



(c-lang-defconst c-basic-matchers-after
  csharp `(

           ;; option 1:
           ;;            ,@(when condition
           ;;                `((,(byte-compile
           ;;                     `(lambda (limit) ...
           ;;
           ;; option 2:
           ;;            ,`((lambda (limit) ...
           ;;
           ;; I don't know how to avoid the (when condition ...) in the
           ;; byte-compiled version.
           ;;
           ;; X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+X+

           ;; Case 1: invocation of constructor + maybe an object
           ;; initializer.  Some possible examples that satisfy:
           ;;
           ;;   new Foo ();
           ;;
           ;;   new Foo () { };
           ;;
           ;;   new Foo {  };
           ;;
           ;;   new Foo { Prop1= 7 };
           ;;
           ;;   new Foo {
           ;;     Prop1= 7
           ;;   };
           ;;
           ;;   new Foo {
           ;;     Prop1= 7,
           ;;     Prop2= "Fred"
           ;;   };
           ;;
           ;;   new Foo {
           ;;      Prop1= new Bar()
           ;;   };
           ;;
           ;;   new Foo {
           ;;      Prop1= new Bar { PropA = 5.6F }
           ;;   };
           ;;
           ,@(when t
               `((,(byte-compile
                    `(lambda (limit)
                        (let ((parse-sexp-lookup-properties
                               (cc-eval-when-compile
                                 (boundp 'parse-sexp-lookup-properties))))

                          (while (re-search-forward
                                  ,(concat "\\<new"
                                           "[ \t\n\r\f\v]+"
                                           "\\(\\(?:"
                                           (c-lang-const c-symbol-key)
                                           "\\.\\)*"
                                           (c-lang-const c-symbol-key)
                                           "\\)"
                                           )
                                  limit t)
                            (unless
                                (progn
                                  (goto-char (match-beginning 0))
                                  (c-skip-comments-and-strings limit))

                              (csharp-log 3 "ctor invoke? at %d" (match-beginning 1))

                              (save-match-data
                                ;; next thing could be: [] () <> or {} or nothing (semicolon, comma).

                                ;; fontify the typename
                                (c-put-font-lock-face (match-beginning 1)
                                                      (match-end 1)
                                                      'font-lock-type-face)

                                (goto-char (match-end 0))
                                (c-forward-syntactic-ws limit)
                                (if (eq (char-after) ?<) ;; ctor for generic type
                                    (progn
                                      (csharp-log 3 " - this is a generic type")
                                      ;; skip over <> safely
                                      (c-safe (c-forward-sexp 1) t)
                                      (c-forward-syntactic-ws)))

                                ;; now, could be [] or (..) or {..} or semicolon.

                                (csharp-log 3 " - looking for sexp")

                                (if (or
                                     (eq (char-after) ?{) ;; open curly
                                     (and (eq (char-after) 91) ;; open square
                                          (while (eq (char-after) 91)
                                            (c-safe (c-forward-sexp 1)))
                                          (eq (char-before) 93)) ;; close square
                                     (and (eq (char-after) 40) ;; open paren
                                          (c-safe (c-forward-sexp 1) t)))

                                    (progn
                                      ;; at this point we've jumped over any intervening s-exp,
                                      ;; like sq brackets or parens.
                                      (c-forward-syntactic-ws)
                                      (csharp-log 3 " - after fwd-syn-ws point(%d)" (point))
                                      (csharp-log 3 " - next char:  %c" (char-after))
                                      (if (eq (char-after) ?{)
                                          (let ((start (point))
                                                (end (if (c-safe (c-forward-sexp 1) t)
                                                         (point) 0)))
                                            (csharp-log 3 " -  open curly gets c-decl-id-start %d" start)
                                            (c-put-char-property start
                                                                 'c-type
                                                                 'c-decl-id-start)
                                            (goto-char start)
                                            (if (> end start)
                                                (progn
                                                  (forward-char 1) ;; step over open curly
                                                  (c-forward-syntactic-ws)
                                                  (while (> end (point))
                                                    ;; now, try to fontify/assign variables to any properties inside the curlies
                                                    (csharp-log 3 " - inside open curly  point(%d)" (point))
                                                    (csharp-log 3 " -   next char:  %c" (char-after))
                                                    ;; fontify each property assignment
                                                    (if (re-search-forward
                                                         (concat "\\(" (c-lang-const c-symbol-key) "\\)\\s*=")
                                                         end t)
                                                        (progn
                                                          (csharp-log 3 " -   found variable  %d-%d"
                                                                      (match-beginning 1)
                                                                      (match-end 1))
                                                          (c-put-font-lock-face (match-beginning 1)
                                                                                (match-end 1)
                                                                                'font-lock-variable-name-face)
                                                          (goto-char (match-end 0))
                                                          (c-forward-syntactic-ws)
                                                          ;; advance to the next assignment, if possible
                                                          (if (eq (char-after) ?@)
                                                              (forward-char 1))

                                                          (if (c-safe (c-forward-sexp 1) t)
                                                              (progn
                                                                (forward-char 1)
                                                                (c-forward-syntactic-ws))))

                                                      ;; else
                                                      (csharp-log 3 " -   no more assgnmts found")
                                                      (goto-char end)))))
                                            )))))

                              (goto-char (match-end 0))
                              )))
                        nil))
                    )))


           ;; Case 2: declaration of enum with or without an explicit
           ;; base type.
           ;;
           ;; Examples:
           ;;
           ;;  public enum Foo { ... }
           ;;
           ;;  public enum Foo : uint { ... }
           ;;
           ,@(when t
               `((,(byte-compile
                    `(lambda (limit)
                       (let ((parse-sexp-lookup-properties
                              (cc-eval-when-compile
                                (boundp 'parse-sexp-lookup-properties))))
                         (while (re-search-forward
                                 ,(concat csharp-enum-decl-re
                                          "[ \t\n\r\f\v]*"
                                          "{")
                                 limit t)

                           (csharp-log 3 "enum? at %d" (match-beginning 0))

                           (unless
                               (progn
                                 (goto-char (match-beginning 0))
                                 (c-skip-comments-and-strings limit))
                             (progn
                               (save-match-data
                                 (goto-char (match-end 0))
                                 (c-put-char-property (1- (point))
                                                      'c-type
                                                      'c-decl-id-start)
                                 (c-forward-syntactic-ws))
                               (save-match-data
                                 (c-font-lock-declarators limit t nil))
                               (goto-char (match-end 0))
                               )
                             )))
                       nil))
                  )))


           ;; Case 3: declaration of constructor
           ;;
           ;; Example:
           ;;
           ;; private Foo(...) {...}
           ;;
           ,@(when t
               `((,(byte-compile
                    `(lambda (limit)
                       (let ((parse-sexp-lookup-properties
                              (cc-eval-when-compile
                                (boundp 'parse-sexp-lookup-properties)))
                             (found-it nil))
                         (while (re-search-forward
                                 ,(concat
                                   "^[ \t\n\r\f\v]*"
                                   "\\(\\<\\(public\\|private\\|protected\\)\\)?[ \t\n\r\f\v]+"
                                   "\\(@?[[:alpha:]_][[:alnum:]_]*\\)" ;; name of constructor
                                   "[ \t\n\r\f\v]*"
                                   "\\("
                                   "("
                                   "\\)")
                                 limit t)

                           (unless
                               (progn
                                 (goto-char (match-beginning 0))
                                 (c-skip-comments-and-strings limit))

                             (goto-char (match-end 0))

                             (csharp-log 3 "ctor decl? L(%d) B(%d) E(%d)"
                                         limit (match-beginning 0) (point))

                             (backward-char 1) ;; just left of the open paren
                             (save-match-data
                               ;; Jump over the parens, safely.
                               ;; If it's an unbalanced paren, no problem,
                               ;; do nothing.
                               (if (c-safe (c-forward-sexp 1) t)
                                   (progn
                                     (c-forward-syntactic-ws)
                                     (cond

                                      ;; invokes base or this constructor.
                                      ((re-search-forward
                                        ,(concat
                                          "\\(:[ \t\n\r\f\v]*\\(base\\|this\\)\\)"
                                          "[ \t\n\r\f\v]*"
                                          "("
                                          )
                                        limit t)
                                       (csharp-log 3 " - ctor with dependency?")

                                       (goto-char (match-end 0))
                                       (backward-char 1) ;; just left of the open paren
                                       (csharp-log 3 " - before paren at %d" (point))

                                       (if (c-safe (c-forward-sexp 1) t)
                                           (progn
                                             (c-forward-syntactic-ws)
                                             (csharp-log 3 " - skipped over paren pair %d" (point))
                                             (if (eq (char-after) ?{)
                                                 (setq found-it t)))))

                                      ;; open curly. no depedency on other ctor.
                                      ((eq (char-after) ?{)
                                       (csharp-log 3 " - no dependency, curly at %d" (point))
                                       (setq found-it t)))

                                     )))

                             (if found-it
                                 ;; fontify the constructor symbol
                                 (c-put-font-lock-face (match-beginning 3)
                                                       (match-end 3)
                                                       'font-lock-function-name-face))
                             (goto-char (match-end 0)))))
                       nil)))))


           ;; Case 4: using clause. Without this, using (..) gets fontified as a fn.
           ,@(when t
               `((,(byte-compile
                    `(lambda (limit)
                       (let ((parse-sexp-lookup-properties
                              (cc-eval-when-compile
                                (boundp 'parse-sexp-lookup-properties))))
                         (while (re-search-forward
                                 ,(concat "\\<\\(using\\)"
                                          "[ \t\n\r\f\v]*"
                                          "(")
                                 limit t)

                           (csharp-log 3 "using clause p(%d)" (match-beginning 0))

                           (unless
                               (progn
                                 (goto-char (match-beginning 0))
                                 (c-skip-comments-and-strings limit))

                             (save-match-data
                               (c-put-font-lock-face (match-beginning 1)
                                                     (match-end 1)
                                                     'font-lock-keyword-face)
                               (goto-char (match-end 0))))))
                       nil))
                  )))

           ;; Case 5: attributes
           ,`((lambda (limit)
                (let ((parse-sexp-lookup-properties
                       (cc-eval-when-compile
                         (boundp 'parse-sexp-lookup-properties))))

                  (while (re-search-forward
                          ,(concat "[ \t\n\r\f\v]+"
                                   "\\(\\["
                                   "[ \t\n\r\f\v]*"
                                   "\\(?:\\(?:return\\|assembly\\)[ \t]*:[ \t]*\\)?"
                                   "\\("
                                   "\\(?:[A-Za-z_][[:alnum:]]*\\.\\)*"
                                   "[A-Za-z_][[:alnum:]]*"
                                   "\\)"
                                   "[^]]*\\]\\)"
                                   )
                          limit t)

                    (csharp-log 3 "attribute? - %d limit(%d)" (match-beginning 1)
                                limit)

                    (unless
                        (progn
                          (goto-char (match-beginning 1))
                          (c-skip-comments-and-strings limit))

                      (let ((b2 (match-beginning 2))
                            (e2 (match-end 2))
                            (is-attr nil))
                        (csharp-log 3 " - type match: %d - %d"
                                    b2 e2)
                        (save-match-data
                          (c-backward-syntactic-ws)
                          (setq is-attr (or
                                         (eq (char-before) 59) ;; semicolon
                                         (eq (char-before) 93) ;; close square brace
                                         (eq (char-before) 123) ;; open curly
                                         (eq (char-before) 125) ;; close curly
                                         (save-excursion
                                           (c-beginning-of-statement-1)
                                           (looking-at
                                            "#\\(pragma\\|endregion\\|region\\|if\\|else\\|endif\\)"))
                                         )))

                        (if is-attr
                            (progn
                              (if (<= 3 csharp-log-level)
                                  (csharp-log 3 " - attribute: '%s'"
                                              (buffer-substring-no-properties b2 e2)))
                              (c-put-font-lock-face b2 e2 'font-lock-type-face)))))
                    (goto-char (match-end 0))
                    ))
                nil))


           ;; Case 6: directive blocks for .aspx/.ashx/.ascx
           ,`((lambda (limit)
                (let ((parse-sexp-lookup-properties
                       (cc-eval-when-compile
                         (boundp 'parse-sexp-lookup-properties))))

                  (while (re-search-forward csharp-aspnet-directive-re limit t)
                    (csharp-log 3 "aspnet template? - %d limit(%d)" (match-beginning 1)
                                limit)

                    (unless
                        (progn
                          (goto-char (match-beginning 0))
                          (c-skip-comments-and-strings limit))

                        (save-match-data
                          (let ((end-open (+ (match-beginning 0) 3))
                                (beg-close (- (match-end 0) 2)))
                            (c-put-font-lock-face (match-beginning 0)
                                                  end-open
                                                  'font-lock-preprocessor-face)

                            (c-put-font-lock-face beg-close
                                                  (match-end 0)
                                                  'font-lock-preprocessor-face)

                            ;; fontify within the directive
                            (while (re-search-forward
                                    ,(concat
                                      "\\("
                                      (c-lang-const c-symbol-key)
                                      "\\)"
                                      "=?"
                                      )
                                    beg-close t)

                            (c-put-font-lock-face (match-beginning 1)
                                                  (match-end 1)
                                                  'font-lock-keyword-face)
                            (c-skip-comments-and-strings beg-close))
                            ))
                        (goto-char (match-end 0)))))
                nil))


;;            ;; Case 5: #if
;;            ,@(when t
;;                `((,(byte-compile
;;                     `(lambda (limit)
;;                        (let ((parse-sexp-lookup-properties
;;                               (cc-eval-when-compile
;;                                 (boundp 'parse-sexp-lookup-properties))))
;;                          (while (re-search-forward
;;                                  "\\<\\(#if\\)[ \t\n\r\f\v]+\\([A-Za-z_][[:alnum:]]*\\)"
;;                                  limit t)
;;
;;                            (csharp-log 3 "#if directive - %d" (match-beginning 1))
;;
;;                            (unless
;;                                (progn
;;                                  (goto-char (match-beginning 0))
;;                                  (c-skip-comments-and-strings limit))
;;
;;                              (save-match-data
;;                                (c-put-font-lock-face (match-beginning 2)
;;                                                      (match-end 2)
;;                                                      'font-lock-variable-name-face)
;;                                (goto-char (match-end 0))))))
;;                        nil))
;;                   )))


 ;;           ,`(,(c-make-font-lock-search-function
 ;;                (concat "\\<new"
 ;;                        "[ \t\n\r\f\v]+"
 ;;                        "\\(\\(?:"
 ;;                        (c-lang-const c-symbol-key)
 ;;                        "\\.\\)*"
 ;;                        (c-lang-const c-symbol-key)
 ;;                        "\\)"
 ;;                        "[ \t\n\r\f\v]*"
 ;;                        "\\(?:"
 ;;                        "( *)[ \t\n\r\f\v]*"          ;; optional ()
 ;;                        "\\)?"
 ;;                        "{")
 ;;                '((c-font-lock-declarators limit t nil)
 ;;                  (save-match-data
 ;;                    (goto-char (match-end 0))
 ;;                    (c-put-char-property (1- (point)) 'c-type
 ;;                                         'c-decl-id-start)
 ;;                    (c-forward-syntactic-ws))
 ;;                  (goto-char (match-end 0)))))




           ;; Fontify labels after goto etc.
           ,@(when (c-lang-const c-before-label-kwds)
               `( ;; (Got three different interpretation levels here,
                 ;; which makes it a bit complicated: 1) The backquote
                 ;; stuff is expanded when compiled or loaded, 2) the
                 ;; eval form is evaluated at font-lock setup (to
                 ;; substitute c-label-face-name correctly), and 3) the
                 ;; resulting structure is interpreted during
                 ;; fontification.)
                 (eval
                  . ,(let* ((c-before-label-re
                             (c-make-keywords-re nil
                               (c-lang-const c-before-label-kwds))))
                       `(list
                         ,(concat "\\<\\(" c-before-label-re "\\)\\>"
                                  "\\s *"
                                  "\\(" ; identifier-offset
                                  (c-lang-const c-symbol-key)
                                  "\\)")
                         (list ,(+ (regexp-opt-depth c-before-label-re) 2)
                               c-label-face-name nil t))))))



           ;; Fontify the clauses after various keywords.
           ,@(when (or (c-lang-const c-type-list-kwds)
                       (c-lang-const c-ref-list-kwds)
                       (c-lang-const c-colon-type-list-kwds)
                       (c-lang-const c-paren-type-kwds))
               `((,(c-make-font-lock-search-function
                    (concat "\\<\\("
                            (c-make-keywords-re nil
                              (append (c-lang-const c-type-list-kwds)
                                      (c-lang-const c-ref-list-kwds)
                                      (c-lang-const c-colon-type-list-kwds)
                                      (c-lang-const c-paren-type-kwds)))
                            "\\)\\>")
                    '((c-fontify-types-and-refs ((c-promote-possible-types t))
                        (c-forward-keyword-clause 1)
                        (if (> (point) limit) (goto-char limit))))))))


           ;; Fontify the name that follows each namespace declaration
           ;; this needs to be done in the matchers-after because
           ;; otherwise the namespace names get the font-lock-type-face,
           ;; due to the energetic efforts of c-forward-type.
           ,`("\\<\\(namespace\\)[ \t\n\r\f\v]+\\(\\(?:[A-Za-z_][[:alnum:]]*\\.\\)*[A-Za-z_][[:alnum:]]*\\)"
              2 font-lock-constant-face t)


           ))



;; C# does generics.  Setting this to t tells the parser to put
;; parenthesis syntax on angle braces that surround a comma-separated
;; list.
(c-lang-defconst c-recognize-<>-arglists
  csharp t)


(c-lang-defconst c-identifier-key
  csharp (concat "\\([[:alpha:]_][[:alnum:]_]*\\)" ; 1
                 "\\("
                 "[ \t\n\r\f\v]*"
                 "\\(\\.\\)"             ;;(c-lang-const c-opt-identifier-concat-key)
                 "[ \t\n\r\f\v]*"
                 "\\(\\([[:alpha:]_][[:alnum:]_]*\\)\\)"
                 "\\)*"))

;; C# has a few rules that are slightly different than Java for
;; operators. This also removed the Java's "super" and replaces it
;; with the C#'s "base".
(c-lang-defconst c-operators
  csharp `((prefix "base")))


;; C# uses CPP-like prefixes to mark #define, #region/endregion,
;; #if/else/endif, and #pragma.  This regexp matches the prefix, not
;; including the beginning-of-line (BOL), and not including the term
;; after the prefix (define, pragma, region, etc).  This regexp says
;; whitespace, followed by the prefix, followed by maybe more
;; whitespace.

(c-lang-defconst c-opt-cpp-prefix
  csharp "\\s *#\\s *")


;; there are no message directives in C#
(c-lang-defconst c-cpp-message-directives
  csharp nil)

(c-lang-defconst c-cpp-expr-directives
  csharp '("if"))

(c-lang-defconst c-opt-cpp-macro-define
  csharp "define")

;; $ is not a legal char in an identifier in C#.  So we need to
;; create a csharp-specific definition of this constant.
(c-lang-defconst c-symbol-chars
  csharp (concat c-alnum "_"))

;; c-identifier-syntax-modifications by default defines $ as a word
;; syntax, which is not legal in C#.  So, define our own lang-specific
;; value.
(c-lang-defconst c-identifier-syntax-modifications
  csharp '((?_ . "w")))



(c-lang-defconst c-colon-type-list-kwds
  csharp '("class"))

(c-lang-defconst c-block-prefix-disallowed-chars

  ;; Allow ':' for inherit list starters.
  csharp (set-difference (c-lang-const c-block-prefix-disallowed-chars)
                         '(?: ?,)))


(c-lang-defconst c-assignment-operators
  csharp '("=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|="))

(c-lang-defconst c-primitive-type-kwds
  ;; ECMA-344, S8
  csharp '("object" "string" "sbyte" "short" "int" "long" "byte"
           "ushort" "uint" "ulong" "float" "double" "bool" "char"
           "decimal" "void"))

;; The keywords that define that the following is a type, such as a
;; class definition.
(c-lang-defconst c-type-prefix-kwds
  ;; ECMA-344, S?
  csharp '("class" "interface" "struct"))  ;; no enum here.
                                           ;; we want enum to be a brace list.


;; Type modifier keywords. They appear anywhere in types, but modify
;; instead of create one.
(c-lang-defconst c-type-modifier-kwds
  ;; EMCA-344, S?
  csharp '("readonly" "const"))


;; Tue, 20 Apr 2010  16:02
;; need to verify that this works for lambdas...
(c-lang-defconst c-special-brace-lists
  csharp '((?{ . ?}) ))



;; dinoch
;; Thu, 22 Apr 2010  18:54
;;
;; No idea why this isn't getting set properly in the first place.
;; In cc-langs.el, it is set to the union of a bunch of things, none
;; of which include "new", or "enum".
;;
;; But somehow both of those show up in the resulting derived regexp.
;; This breaks indentation of instance initializers, such as
;;
;;         var x = new Foo { ... };
;;
;; Based on my inspection, the existing c-lang-defconst should work!
;; I don't know how to fix this c-lang-defconst, so I am re-setting this
;; variable here, to provide the regex explicitly.
;;
(c-lang-defconst c-decl-block-key
  csharp '"\\(namespace\\)\\([^[:alnum:]_]\\|$\\)\\|\\(class\\|interface\\|struct\\)\\([^[:alnum:]_]\\|$\\)" )


;; Thu, 22 Apr 2010  14:29
;; I want this to handle    var x = new Foo[] { ... };
;; not sure if necessary.
(c-lang-defconst c-inexpr-brace-list-kwds
  csharp '("new"))


;; ;;(c-lang-defconst c-inexpr-class-kwds
;; ;; csharp '("new"))



(c-lang-defconst c-class-decl-kwds
  ;; EMCA-344, S?
  ;; don't include enum here, because we want it to be fontified as a brace
  ;; list, with commas delimiting the values. see c-brace-list-decl-kwds
  ;; below.
  csharp '("class" "interface" "struct" ))  ;; no "enum"!!


;; The various modifiers used for class and method descriptions.
(c-lang-defconst c-modifier-kwds
  csharp '("public" "partial" "private" "const" "abstract" "sealed"
           "protected" "ref" "out" "static" "virtual"
           "override" "params" "internal"))


;; Thu, 22 Apr 2010  23:02
;; Based on inspection of the cc-mode code, the c-protection-kwds
;; c-lang-const is used only for objective-c.  So the value is
;; irrelevant for csharp.
(c-lang-defconst c-protection-kwds
  csharp nil
  ;; csharp '("private" "protected" "public" "internal")
)


;; Define the keywords that can have something following after them.
(c-lang-defconst c-type-list-kwds
  csharp '("struct" "class" "interface" "is" "as"
           "delegate" "event" "set" "get" "add" "remove"))


;; This allows the classes after the : in the class declartion to be
;; fontified.
(c-lang-defconst c-typeless-decl-kwds
  csharp '(":"))

;; Sets up the enum to handle the list properly, and also the new
;; keyword to handle object initializers.  This requires a modified
;; c-basic-matchers-after (see above) in order to correctly fontify C#
;; 3.0 object initializers.
(c-lang-defconst c-brace-list-decl-kwds
  csharp '("enum" "new"))


;; Statement keywords followed directly by a substatement.
;; catch is not one of them, because catch has a paren (typically).
(c-lang-defconst c-block-stmt-1-kwds
  csharp '("do" "try" "finally" "unsafe"))


;; Statement keywords followed by a paren sexp and then by a substatement.
(c-lang-defconst c-block-stmt-2-kwds
  csharp '("for" "if" "switch" "while" "catch" "foreach" "using"
           "checked" "unchecked" "lock"))


;; Statements that break out of braces
(c-lang-defconst c-simple-stmt-kwds
  csharp '("return" "continue" "break" "throw" "goto" ))

;; Statements that allow a label
;; TODO?
(c-lang-defconst c-before-label-kwds
  csharp nil)

;; Constant keywords
(c-lang-defconst c-constant-kwds
  csharp '("true" "false" "null"))

;; Keywords that start "primary expressions."
(c-lang-defconst c-primary-expr-kwds
  csharp '("this" "base"))

;; Treat namespace as an outer block so class indenting
;; works properly.
(c-lang-defconst c-other-block-decl-kwds
  csharp '("namespace"))

(c-lang-defconst c-other-kwds
  csharp '("sizeof" "typeof" "is" "as" "yield"
           "where" "select" "in" "from"))

(c-lang-defconst c-overloadable-operators
  ;; EMCA-344, S14.2.1
  csharp '("+" "-" "*" "/" "%" "&" "|" "^"
           "<<" ">>" "==" "!=" ">" "<" ">=" "<="))


;; This c-cpp-matchers stuff is used for fontification.
;; see cc-font.el
;;

;; There's no preprocessor in C#, but there are still compiler
;; directives to fontify: "#pragma", #region/endregion, #define, #undef,
;; #if/else/endif.  (The definitions for the extra keywords above are
;; enough to incorporate them into the fontification regexps for types
;; and keywords, so no additional font-lock patterns are required for
;; keywords.)

(c-lang-defconst c-cpp-matchers
  csharp (cons
      ;; Use the eval form for `font-lock-keywords' to be able to use
      ;; the `c-preprocessor-face-name' variable that maps to a
      ;; suitable face depending on the (X)Emacs version.
      '(eval . (list "^\\s *\\(#pragma\\|undef\\|define\\)\\>\\(.*\\)"
                     (list 1 c-preprocessor-face-name)
                     '(2 font-lock-string-face)))
      ;; There are some other things in `c-cpp-matchers' besides the
      ;; preprocessor support, so include it.
      (c-lang-const c-cpp-matchers)))



;; Custom variables
;;;###autoload
(defcustom csharp-mode-hook nil
    "*Hook called by `csharp-mode'."
    :type 'hook
    :group 'csharp)

  ;; The following fn allows this:
  ;;    (csharp-log 3 "scan result...'%s'" state)

(defcustom csharp-log-level 0
    "The current log level for CSharp-mode-specific operations.
This is used in particular by the verbatim-literal
string scanning.

Most other csharp functions are not instrumented.
0 = NONE, 1 = Info, 2 = VERBOSE, 3 = DEBUG, 4 = SHUTUP ALREADY. "
    :type 'integer
    :group 'csharp)


;;;###autoload
(defcustom csharp-want-flymake-fixup t
  "*Whether to enable the builtin C# support for flymake. This is meaningful
only if flymake is loaded."
  :type 'boolean :group 'csharp)

;;;###autoload
(defcustom csharp-want-yasnippet-fixup t
  "*Whether to enable the builtin C# support for yasnippet. This is meaningful
only if flymake is loaded."
  :type 'boolean :group 'csharp)


;;;###autoload
(defcustom csharp-want-imenu t
  "*Whether to generate a buffer index via imenu for C# buffers."
  :type 'boolean :group 'csharp)


;;;###autoload
(defcustom csharp-make-tool "nmake.exe"
  "*The make tool to use. Defaults to nmake, found on path. Specify
a full path or alternative program name, to tell csharp-mode to use
a different make tool in compile commands.

See also, `csharp-msbuild-tool'.

"
  :type 'string :group 'csharp)


;;;###autoload
(defcustom csharp-msbuild-tool "msbuild.exe"
  "*The tool to use to build .csproj files. Defaults to msbuild, found on
path. Specify a full path or alternative program name, to tell csharp-mode
to use a different make tool in compile commands.

See also, `csharp-make-tool'.

"
  :type 'string :group 'csharp)


;;;###autoload
(defcustom csharp-cmd-line-limit 28
  "The number of lines at the top of the file to look in, to find
the command that csharp-mode will use to compile the current
buffer, or the command \"stub\" that csharp-mode will use to
check the syntax of the current buffer via flymake.

If the value of this variable is zero, then csharp-mode looks
everywhere in the file.  If the value is positive, then only in
the first N lines. If negative, then only in the final N lines.

The line should appear in a comment inside the C# buffer.


Compile
--------

In the case of compile, the compile command must be prefixed with
\"compile:\".  For example,

 // compile: csc.exe /r:Hallo.dll Arfie.cs


This command will be suggested as the compile command when the
user invokes `compile' for the first time.


Flymake
--------

In the case of flymake, the command \"stub\" string must be
prefixed with \"flymake:\".  For example,

 // flymake: DOTNETDIR\csc.exe /target:netmodule /r:foo.dll @@FILE@@

In the case of flymake, the string should NOT include the name of
the file for the buffer being checked. Instead, use the token
@@FILE@@ .  csharp-mode will replace this token with the name of
the source file to compile, before passing the command to flymake
to run it.

If for some reason the command is invalid or illegal, flymake
will report an error and disable itself.

It might be handy to run fxcop, for example, via flymake.

 // flymake: fxcopcmd.exe /c  /f:MyLibrary.dll



In all cases
------------

Be sure to specify the proper path for your csc.exe, whatever
version that might be, or no path if you want to use the system
PATH search.

If the buffer depends on external libraries, then you will want
to include /R arguments to that csc.exe command.

To be clear, this variable sets the number of lines to search for
the command.  This cariable is an integer.

If the marker string (either \"compile:\" or \"flymake:\"
is present in the given set of lines, csharp-mode will take
anything after the marker string as the command to run.

"
  :type 'integer   :group 'csharp)



(defconst csharp-font-lock-keywords-1 (c-lang-const c-matchers-1 csharp)
  "Minimal highlighting for C# mode.")

(defconst csharp-font-lock-keywords-2 (c-lang-const c-matchers-2 csharp)
  "Fast normal highlighting for C# mode.")

(defconst csharp-font-lock-keywords-3 (c-lang-const c-matchers-3 csharp)
  "Accurate normal highlighting for C# mode.")

(defvar csharp-font-lock-keywords csharp-font-lock-keywords-3
  "Default expressions to highlight in C# mode.")


(defvar csharp-mode-syntax-table nil
  "Syntax table used in csharp-mode buffers.")
(or csharp-mode-syntax-table
    (setq csharp-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table csharp))))

(defvar csharp-mode-abbrev-table nil
  "Abbreviation table used in csharp-mode buffers.")
(c-define-abbrev-table 'csharp-mode-abbrev-table
