;;; scala-mode-map.el - Major mode for editing scala, keyboard map
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

(provide 'scala-mode-map)

(defmacro scala-mode-map:define-keys (key-map key-funcs)
  (cons 'progn (mapcar 
   #'(lambda (key-func)
       `(define-key ,key-map ,(car key-func) ,(cadr key-func)))
   key-funcs)))

(defvar scala-mode-map nil
  "Local key map used for scala mode")

(when (not scala-mode-map)
  (let ((keymap (make-sparse-keymap)))
    (scala-mode-map:define-keys 
     keymap
     (([backspace]                'backward-delete-char-untabify)
      ([(control c)(control r)]   'scala-syntax:runonp) ; TODO remove
      ;;       ("\r"                       'scala-newline)
      ([(control c)(control c)]   'comment-region)
      ;;       ("}"                        'scala-electric-brace)
      ))
     (setq scala-mode-map keymap)))
  
