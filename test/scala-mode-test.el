(defun smt:test (line exps expf)
  "line - line of scala code
exps - expected codes of syntax class
expf - expected font-locks"
  (let ((line-length (length line)))
    (with-temp-buffer
      (insert (format "package ensime

object Ensime {
  %s
}" line))
      (scala-mode)
      (font-lock-ensure)
      (re-search-backward (regexp-opt `(,line)) nil t)
      (let ((end-point (+ (point) line-length))
            (acc-syntax "")
            (acc-font ""))
        (while (< (point) end-point)
          (setq acc-syntax (concat acc-syntax (number-to-string (syntax-class (syntax-after (point))))))
          (setq acc-font (concat acc-font (font-lock-to-string (get-text-property (point) 'face))))
          (forward-char))
        (should (equal acc-syntax exps))
        (should (equal acc-font expf))))))

(defun font-lock-to-string (font-lock)
  (pcase font-lock
    (`nil "-")
    ('font-lock-constant-face "C")
    ('font-lock-variable-name-face "V")
    ('font-lock-keyword-face "K")
    ('font-lock-comment-face "O")
    ('font-lock-comment-delimiter-face "D")
    ('font-lock-doc-face "U")
    ('font-lock-type-face "T")
    (_ "?")))

(ert-deftest smt:syntax-class-and-font-lock-test-1 ()
  (smt:test
   "val `tvw xyz/*` = `abc def/*` + 123 /* comment `abc` abc */ + 456"
   "22203333333333301033333333333010222011022222220333330222011010222"
   "KKK-VVVVVVVVVVV-K---------------CCC-DDDOOOOOOOOOOOOOOOOOOOO---CCC"))

(ert-deftest smt:syntax-class-and-font-lock-test-2 ()
  (smt:test
   "val |+| = 123"
   "2220333010222"
   "KKK-VVV-K-CCC"))

(ert-deftest smt:syntax-class-and-font-lock-test-3 ()
  (smt:test
   "val a_|+| = 123"
   "222023333010222"
   "KKK-VVVVV-K-CCC"))

(ert-deftest smt:syntax-class-and-font-lock-test-4 ()
  (smt:test
   "val a = 123 /** hello */"
   "222020102220111022222011"
   "KKK-V-K-CCC-UUUUUUUUUUUU"))

(ert-deftest smt:syntax-class-and-font-lock-test-5 ()
  (smt:test
   "val a = <td>hello</td>"
   "2220201012212222211221"
   "KKK-V-K---------------"))

(ert-deftest smt:syntax-class-and-font-lock-test-6 ()
  (smt:test
   "// val |--| = 123"
   "11022203333010222"
   "DDDOOOOOOOOOOOOOO"))

(ert-deftest smt:syntax-class-and-font-lock-test-7 ()
  (smt:test
   "val xs = 1 :: 2 :: Nil"
   "2220220102033020330222"
   "KKK-VV-K-C----C----CCC"))

(ert-deftest smt:syntax-class-and-font-lock-test-8 ()
  (smt:test
   "val xs = 1:: 2 :: Nil"
   "222022010211020330222"
   "KKK-VV-K-C---C----CCC"))

(ert-deftest smt:syntax-class-and-font-lock-test-9 ()
  (smt:test
   "val xs = 1 ::2 :: Nil"
   "222022010201120330222"
   "KKK-VV-K-C---C----CCC"))

(ert-deftest smt:syntax-class-and-font-lock-test-10 ()
  (smt:test
   "case a :: (2) :: Nil"
   "22220203304250330222"
   "KKKK-V-TT--C--CC-TTT"))

(ert-deftest smt:syntax-class-and-font-lock-test-11 ()
  (smt:test
   "abc :<: def"
   "22203330222"
   "--------KKK"))

(ert-deftest smt:syntax-class-and-font-lock-test-12 ()
  (smt:test
   "Foo<T>"
   "222121"
   "CCC-C-"))

(ert-deftest smt:syntax-class-and-font-lock-test-13 ()
  (smt:test
   "class X[T<:Mapper[T]](t: T){}"
   "22222024211222222425542102545"
   "KKKKK-T-CKKCCCCCC-C----K-T---"))

(ert-deftest smt:syntax-class-and-font-lock-test-14 ()
  (smt:test
   "class X[T <: Mapper[T]](t: T){}"
   "2222202420330222222425542102545"
   "KKKKK-T-C-KK-CCCCCC-C----K-T---"))
