;;; scala-mode-fontlock.el - Major mode for editing scala, font-lock
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(require 'scala-mode2-syntax)

(defface scala-font-lock:var-face
  '((t (:inherit font-lock-warning-face)))
  "Font Lock mode face used to highlight scala variable names."
  :group 'scala)

(defvar scala-font-lock:var-face 'scala-font-lock:var-face
  "Face for scala variable names.")

(defcustom scala-font-lock:constant-list '()
  "A list of strigs that should be fontified in constant
face. This customization property takes effect only after the
scala-mode has been reloaded."
  :type '(repeat string)
  :group 'scala)

(defconst scala-font-lock:mustBeContinued-keywords-re
  (regexp-opt '("case" "class" "def" "extends" "new" "object" "package" "trait" 
                "type" "val" "var" "with") 'words))

(defconst scala-font-lock:mustBeContinued-line-end-re
  (concat "\\(" scala-font-lock:mustBeContinued-keywords-re
          "\\|:" scala-syntax:end-of-code-line-re "\\)")
  "All keywords and symbols that cannot terminate a expression
and are infact a sign of run-on. Reserved-symbols not included.")

(defconst scala-font-lock:string-interpolation-start-re
  (concat "\\$\\(?:\\(" scala-syntax:alphaid-re "\\)\\|\\({\\)\\)")
  "Start of a string interpolation variable or block")

(defun scala-font-lock:extend-after-change-region-function (beg end old-len)
  "Find a safe region to font-lock" nil
  (save-excursion
    (save-match-data
      (goto-char beg)
      (forward-line 0)
      (while (scala-syntax:looking-back-token scala-font-lock:mustBeContinued-line-end-re)
        (forward-line -1))
      (setq beg (point))
      (goto-char end)
      (end-of-line)
      (while (and (/= (point) (point-max))
                  (scala-syntax:looking-back-token scala-font-lock:mustBeContinued-line-end-re))
        (forward-line 1)
        (end-of-line))
      (cons beg (point)))))

(defun scala-font-lock:create-user-constant-re ()
  (regexp-opt scala-font-lock:constant-list 'symbols))

(defun scala-font-lock:mark-reserved-symbols (limit)
  (when (re-search-forward scala-syntax:reserved-symbols-re limit t)
      (goto-char (match-end 1)))) ;; step back to the match (re matches futher)

(defun scala-font-lock:mark-underscore (limit)
  (when (re-search-forward scala-syntax:reserved-symbol-underscore-re limit t)
      (goto-char (match-end 1)))) ;; step back to the match (re matches futher)

(defun scala-font-lock:limit-pattern2 (&optional start)
  (save-excursion
    (when start (goto-char start))
    (scala-syntax:skip-forward-ignorable)
    (ignore-errors
      (while (and (not (or (eobp)
                           (nth 8 (syntax-ppss))
                           (looking-at scala-syntax:other-keywords-unsafe-re)
                           (scala-syntax:looking-at-reserved-symbol nil)))
                  (scala-syntax:looking-at-simplePattern-beginning))
;        (message "- now at %d" (point))
        (if (= (char-after) ?\()
            (forward-list)
          ;; else
          (goto-char (match-end 0))
          (scala-syntax:skip-forward-ignorable)
;          (message "+ now at %d" (point))
          (cond ((looking-at "(")
                 (forward-list))
                ((looking-at "@")
                 (goto-char (match-end 0)))
                ((or (scala-syntax:looking-at-reserved-symbol nil)
                     (looking-at scala-syntax:other-keywords-unsafe-re))
;                 (messssage "saw reserved symbol or keyword")
                 nil)
                ((looking-at scala-syntax:id-re)
;                 (message "saw id-re %d" (match-beginning 0))
                 (goto-char (match-end 0)))
;                (t
;                 (message "nothing special here %s" (point)))
                ))
        (scala-syntax:skip-forward-ignorable)))
;    (message "limit at %s" (point))
    (point)))

(defun scala-font-lock:limit-pattern2-list (&optional start)
  (unless (nth 8 (syntax-ppss))
    (let ((limit (scala-font-lock:limit-pattern2 start)))
      (while (= (char-after limit) ?,)
        (setq limit (scala-font-lock:limit-pattern2 (1+ limit))))
;    (message "list limit at %s" limit)
      limit)))

(defun scala-font-lock:mark-pattern1-part (&optional limit pattern-p)
  "Parses a part of val, var and case pattern (or id). Always
parses a variable or constant name first and then type, leaving
the pointer at the next variablename, constnat name, list or
Pattern3, if any, and setting up match data 1 (variable),
2 (constant) and 3 (type) acordingly. If there is no variable
name before the first type, then the match data for the variable
name is nil. Returns t if something was matched or nil if nothing
was found.

If pattern-p is defined, then only varid is matched as variable
and everything else is constant.

Does not continue past limit.
"
;  (message "will stop at %d" limit)
  (cond
   ;; quit if we are past limit or in a comment or string
   ((or (and limit (>= (point) limit))
        (eobp)
        (nth 8 (syntax-ppss)))
;    (message "at limit %s" (point))
    nil)
   ;; Type pattern, just skip the whole thing. It will end at ',' or ')'.
   ;; Note: forms starting with ':' are handled by a completely separete
   ;; font-lock matcher.
   ((scala-syntax:looking-at-reserved-symbol ":")
;    (message ":")
    (while (not (or (eobp)
                    (scala-syntax:looking-at "[,);]")
                    (scala-syntax:looking-at-reserved-symbol "|")
                    (scala-syntax:looking-at-reserved-symbol
                     scala-syntax:double-arrow-unsafe-re)
                    (scala-syntax:looking-at-empty-line-p)))
      (scala-syntax:forward-sexp)
      (scala-syntax:skip-forward-ignorable))
    (set-match-data nil)
    t)
   ;; Binding part cannot start with reserved symbols. If they
   ;; are seen, we must quit.
   ((scala-syntax:looking-at-reserved-symbol nil)
;    (message "symbol")
    nil)
   ((scala-syntax:looking-at-path 'stableId)
;    (message "stableId")
    (let ((beg (match-beginning 0))
          (end (match-end 0))
          (varid (scala-syntax:looking-at-path 'varid)))
      (goto-char end)
      (let ((new-match-data
             (cond
              ((= (char-after end) ?\()
               ;; matched type
;               (message "it's a type")
               `(,beg ,end nil nil nil nil ,beg ,end))
              ((progn (scala-syntax:backward-sexp)
                      (= (char-before) ?.))
               ;; matched constant
               `(,beg ,end nil nil ,(point) ,end nil nil))
              ((or varid (not pattern-p))
               ;; matched variable name or we can't be sure
               `(,beg ,end ,beg ,end nil nil nil nil))
              (t
               ;; matched constant
               `(,beg ,end nil nil ,beg ,end nil nil)))))
        (goto-char end)
        (scala-syntax:skip-forward-ignorable)
        (cond
         ((and (not (or (scala-syntax:looking-at-reserved-symbol nil)
                        (scala-syntax:looking-at-reserved-symbol "|")))
               (scala-syntax:looking-at-path 'stableId))
          (setq new-match-data
                (append (butlast new-match-data 2)
                        `(,(match-beginning 0)
                          ,(match-end 0))))
          (goto-char (match-end 0))
          (scala-syntax:skip-forward-ignorable))
         ((= (char-after) ?@)
          (forward-char)
          (scala-syntax:skip-forward-ignorable)))
        (set-match-data new-match-data)))
    t)
   ;; Pattern3 can be a literal. Just skip them.
   ((looking-at scala-syntax:literal-re)
;    (message "literal")
    (goto-char (match-end 0))
    (scala-syntax:skip-forward-ignorable)
    (set-match-data nil)
    t)
   ;; Start of a patterns list or alternatives. Skip if alternatives or
   ;; else leave point at start of first element.
   ((= (char-after) ?\()
;    (message "(")
    (let ((alternatives-p
           (save-excursion
             (forward-char)
             (ignore-errors
               ;; forward-sexp will terminate the loop with error
               ;; if '|' is not found before end of list ')'
               (while (not (or (eobp)
                               (= (char-before) ?|)
                               (scala-syntax:looking-at-empty-line-p)))
                 (scala-syntax:forward-sexp))
               t))))
      (if alternatives-p
          (forward-list)
        (forward-char)))
    (scala-syntax:skip-forward-ignorable)
    (set-match-data nil)
    t)
   ;; continuation or end of list, just skip and position at the
   ;; next element
   ((or (= (char-after) ?,)
        (= (char-after) ?\)))
;    (message ", or )")
    (forward-char)
    (scala-syntax:skip-forward-ignorable)
    (set-match-data nil)
    t)
   ;; none of the above, just stop
   (t
;    (message "Cannot continue Pattern1 at %d" (point))
    nil)
))

(defun scala-font-lock:limit-pattern (&optional start)
  (unless (nth 8 (syntax-ppss))
    (save-excursion
      (goto-char (scala-font-lock:limit-pattern2 start))
      (when (scala-syntax:looking-at-reserved-symbol ":")
        (while (not (or (eobp)
                        (scala-syntax:looking-at-reserved-symbol "|")
                        (scala-syntax:looking-at-reserved-symbol
                         scala-syntax:double-arrow-unsafe-re)
                        (scala-syntax:looking-at-empty-line-p)))
          (scala-syntax:forward-sexp)
          (scala-syntax:skip-forward-ignorable)))
      (if (or (/= (char-after) ?|)
              (scala-syntax:looking-at-reserved-symbol
               scala-syntax:double-arrow-unsafe-re))
          (point)
        (forward-char)
        (scala-font-lock:limit-pattern)))))

(defun scala-font-lock:mark-pattern-part (&optional limit)
  (when (scala-syntax:looking-at-reserved-symbol "|")
;    (message "skipping |")
    (forward-char)
    (scala-syntax:skip-forward-ignorable))
  (scala-font-lock:mark-pattern1-part limit t))


;; (defun scala-font-lock:limit-simpleType (&optional start)
;;   (when start (goto-char start))
;;   (scala-syntax:skip-forward-ignorable)
;;   (setq start (point))

;;   (if (= (char-after) ?\()
;;       (ignore-errors (forward-list))
;;     (scala-font-lock:mark-simpleType))
;;   (when (and (not (eobp)) (= (char-after) ?#))
;;     (scala-font-lock:mark-simpleType))
;;   (when (and (not (eobp)) (= (char-after) ?\[))
;;     (ignore-errors (forward-list))
;;     (scala-syntax:skip-forward-ignorable))
;;   (let ((limit (point)))
;;     (goto-char start)
;; ;    (message "simpeType limit at %d" limit)
;;     limit))

;; (defun scala-font-lock:mark-simpleType (&optional limit)
;; ;  (message "looking for simpleType at %d" (point))
;;   (cond
;;    ;; stop at limit
;;    ((and limit (>= (point) limit))
;;     nil)
;;    ;; just dive into lists
;;    ((> (skip-chars-forward "[(,)]") 0)
;; ;    (message "skipping list-marks")
;;     (scala-syntax:skip-forward-ignorable)
;;     (set-match-data nil)
;;     t)
;;    ;; jump over blocks
;;    ((= (char-after) ?\{)
;;     (ignore-errors
;;       (forward-list)
;;       (set-match-data nil)
;;       t))
;;    ;; ignore arrows and reserved words and symbols
;;    ((or (scala-syntax:looking-at-reserved-symbol
;;          scala-syntax:double-arrow-unsafe-re)
;;         (scala-syntax:looking-at-reserved-symbol
;;          "<[:%]\\|>?:")
;;         (looking-at "\\_<forSome\\>"))
;; ;    (message "skipping reserved")
;;     (goto-char (match-end 0))
;;     (scala-syntax:skip-forward-ignorable)
;;     (set-match-data nil)
;;     t)
;;    ;; color id after '#'
;;    ((= (char-after) ?#)
;; ;    (message "at #")
;;     (forward-char)
;;     (if (and (not (or (looking-at scala-syntax:keywords-unsafe-re)
;;                       (scala-syntax:looking-at-reserved-symbol nil)))
;;              (looking-at scala-syntax:id-re))
;;         (goto-char (match-end 0)) nil))
;;    ;; color paths (including stableid)
;;    ((scala-syntax:looking-at-path)
;; ;    (message "at path")
;;     (let ((end (match-end 0)))
;;       (goto-char end)
;;       (while (scala-syntax:looking-back-token "this\\|type")
;;         (goto-char (match-beginning 0))
;;         (skip-chars-backward "."))
;;       (unless (scala-syntax:looking-back-token scala-syntax:id-re)
;;         (set-match-data nil))
;;       (goto-char end))
;;     (scala-syntax:skip-forward-ignorable)
;;     t)
;;    (t
;; ;    (message "Cannot continue simpleType at %d" (point))
;;     nil)))

(defun scala-font-lock:mark-string-escapes (limit)
  (when (re-search-forward scala-syntax:string-escape-re limit t)
    (or (eq (nth 3 (save-excursion (syntax-ppss (match-beginning 0)))) ?\")
        (scala-font-lock:mark-string-escapes limit))))

(defun scala-font-lock:mark-string-interpolation (limit)
  (when (re-search-forward scala-font-lock:string-interpolation-start-re limit t)
;    (message "found $ %s %s" (match-beginning 0) (point))
    ;; now we have to consider the following:
    ;; - are we in a string (nth 3 syntax-ppps) is non-nill, if not continue search
    ;; - are we in a ${} block, if so, mark to end of block (can be nested)
    (let ((syntax (syntax-ppss))
          (beg (match-beginning 0)))
      (if (or (null (nth 3 syntax))
              (not (save-excursion 
                     (goto-char (nth 8 syntax))
                     (skip-syntax-backward "w_")
                     (looking-at-p scala-syntax:alphaid-re))))
          (scala-font-lock:mark-string-interpolation limit) ;; not in string-interpolation
        (or (match-beginning 1) ;; alphaId
            (progn              ;; block
              ;; first thing: decide where to stop searching for the end of the block
              (save-excursion
                (setq limit 
                      (if (characterp (nth 3 syntax)) 
                          (min limit (point-at-eol))
                        (if (re-search-forward "\"\"\"" limit t)
                            (min limit (point))
                          limit))))
              (parse-partial-sexp beg limit 0)
              (set-match-data (list beg (point)))
              t))))))


(defun scala-font-lock:mark-numberLiteral (re limit)
  (when (re-search-forward re limit t)
    (if (string-match-p scala-syntax:number-safe-start-re
                        ;; get char-before match or a magic ',', which is safe
                        (string (or (char-before (match-beginning 0)) ?,)))
        t
      (scala-font-lock:mark-numberLiteral re limit))))

(defun scala-font-lock:mark-floatingPointLiteral (limit)
  (scala-font-lock:mark-numberLiteral
   scala-syntax:floatingPointLiteral-re
   limit))

(defun scala-font-lock:mark-integerLiteral (limit)
  (scala-font-lock:mark-numberLiteral
   scala-syntax:integerLiteral-re
   limit))

(defun scala-font-lock:keywords ()
  ;; chars, string, comments are handled acording to syntax and
  ;; syntax propertize

  `(;; keywords
    (,scala-syntax:other-keywords-re 1 font-lock-keyword-face)
    (,scala-syntax:value-keywords-re 1 font-lock-constant-face)
    (,scala-syntax:path-keywords-re 1 font-lock-keyword-face)

    ;; User defined constants
    (,(scala-font-lock:create-user-constant-re) . font-lock-constant-face)

    ;; Annotations
    (,(rx (and "@" (in "a-zA-Z_.") (0+ (in "a-zA-Z0-9_."))))
     . font-lock-preprocessor-face)

    ;; reserved symbols
    (scala-font-lock:mark-reserved-symbols 1 font-lock-keyword-face)

    ;; 'Symbols
    (,scala-syntax:symbolLiteral-re 1 font-lock-string-face)

    ;; underscore
    (scala-font-lock:mark-underscore 1 font-lock-keyword-face)

    ;; escapes inside strings
    (scala-font-lock:mark-string-escapes (0 font-lock-keyword-face prepend nil))

    ;; string interpolation
    (scala-font-lock:mark-string-interpolation (0 font-lock-preprocessor-face prepend nil))

    ;; `object'
    (,(concat "\\_<object[ \t\n]+\\(" scala-syntax:id-re "\\)")
     1 font-lock-constant-face)

    ;; `type', `class', `trait', `extends', `new' or `with' followed by type
    (,(concat "\\_<\\(?:type\\|class\\|trait\\|extends\\|with\\|new\\)[ \t\n]+"
              "\\(" scala-syntax:id-re "\\)")
     1 font-lock-type-face)

    ;; `package' name
    (,(concat "\\_<package[ \t\n]+\\(\\" 
              "(?:" scala-syntax:varid-re ".\\)*" scala-syntax:varid-re "\\)")
     1 font-lock-string-face)

    ;; `def'
    (,(concat "\\_<def[ \t\n]+\\(" scala-syntax:id-re "\\)") 
     1 font-lock-function-name-face)

    ;; colon followed by id-re. 
    ;; TODO: we need to extend the font-lock region to next line when the line ends with :
    (,(concat "\\(?:^\\|[^" scala-syntax:opchar-group "]\\)"
              "\\(?::\\)[ \t\n]*"
              "\\(\\_<" scala-syntax:id-re "\\_>\\)")
     1 font-lock-type-face)

    ;; ;; extends, with, new
    ;; (,(concat "\\_<\\(extends\\|with\\|new\\)[ \t]+\\([("
    ;;           scala-syntax:id-first-char-group "]\\)")
    ;;  (scala-font-lock:mark-simpleType (scala-font-lock:limit-simpleType
    ;;                                    (goto-char (match-beginning 2)))
    ;;                                   nil
    ;;                                   (0 font-lock-type-face nil t)))

    ;; ;; ':'
    ;; (,scala-syntax:colon-re
    ;;  (scala-font-lock:mark-simpleType (scala-font-lock:limit-simpleType
    ;;                                    (goto-char (match-end 2)))
    ;;                                   nil
    ;;                                   (0 font-lock-type-face nil t)))


    ;; VarDcl
    ("\\_<val[ \t]+\\([^:]\\)"
     (scala-font-lock:mark-pattern1-part (scala-font-lock:limit-pattern2-list
                                          (goto-char (match-beginning 1)))
                                         nil
                                         (1 font-lock-variable-name-face nil t)
                                         (2 font-lock-constant-face nil t)
                                         (3 font-lock-type-face nil t)))

    ("\\_<var[ \t]+\\([^:]\\)"
     (scala-font-lock:mark-pattern1-part (scala-font-lock:limit-pattern2-list
                                          (goto-char (match-beginning 1)))
                                         nil
                                         (1 scala-font-lock:var-face nil t)
                                         (2 font-lock-constant-face nil t)
                                         (3 font-lock-type-face nil t)
                                         ))

    ;; case (but not case class|object)
    ("\\_<case[ \t]+\\([^:]\\)"
     (scala-font-lock:mark-pattern-part (scala-font-lock:limit-pattern
                                         (goto-char (match-beginning 1)))
                                        nil
                                        (1 font-lock-variable-name-face nil t)
                                        (2 font-lock-constant-face nil t)
                                        (3 font-lock-type-face nil t)))

    ;; all other uppercase names means a constant (object, val or type parameter)
    (,(concat "\\_<" scala-syntax:capitalid-re "\\_>")
     . font-lock-constant-face)

    ;; number literals (have to be here so that other rules take precedence)
    (scala-font-lock:mark-floatingPointLiteral . font-lock-constant-face)
    (scala-font-lock:mark-integerLiteral . font-lock-constant-face)

))

(defun scala-font-lock:syntactic-face-function (state)
  "Return correct face for string or comment"
  (if (and (integerp (nth 4 state))
           (save-excursion
             (goto-char (nth 8 state))
             (looking-at "/\\*\\*\\($\\|[^*]\\)")))
      ;; scaladoc (starts with /** only)
      font-lock-doc-face
    (if (nth 3 state) font-lock-string-face font-lock-comment-face)))

(provide 'scala-mode2-fontlock)
