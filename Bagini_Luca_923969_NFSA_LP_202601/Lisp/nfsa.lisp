;;;; Bagini    Luca    923969

;;;; nfsa.lisp
;;;; Compilatore da RE a nfsa in Lisp

;;; Gestisco i casi base di RE

;;; Creo una funzione per stabilire se una Sexp utilizzi un simbolo
;;; riservato per non considerarle poi come simboli dell'alfabeto
(defvar *reserved* '(c a z o))
(defun valid-sexp (re)
    "Ritorna vero se re è una Sexp non riservata"
    (and (listp re) (not (member (car re) *reserved*))))

(defun is-regex (re)
    "Ritorna vero se re è una regex"
    (cond ((null re) t)
        ((atom re) t)
        ((valid-sexp re) t)
        ((eq (car re) 'c) 
            (if (null (cdr re))
                t
                (and (is-regex (cadr re))
                    (is-regex (append '(c) (cddr re))))))
        ((eq (car re) 'a) 
            (if (null (cdr re))
                t
                (and (is-regex (cadr re))
                    (is-regex (append '(a) (cddr re))))))
        ((eq (car re) 'z) (is-regex (cadr re)))
        ((eq (car re) 'o) (is-regex (cadr re)))
        (t nil)))


;;; Funzione di ausilio per creare le transizioni dell'automa
(defun compile-transitions (re in fin)
    "Funzione ricorsiva per generare le transizioni dell'automa"
    (cond
        ;; Caso base: considero la RE vuota come epsilon
        ((null re) (list (list in '() fin)))
        ;; Caso base: simbolo atomico
        ((atom re) (list (list in re fin)))
        ;; Caso base: simbolo Sexp non riservato
        ((valid-sexp re) (list (list in re fin)))
        ;; Caso ricorsivo: concatenazione di RE
        ((eq (car re) 'c)
            (if (null (cdr re))
                (list (list in '() fin))
                (let ((medio (gensym "Q")))
                    (append (compile-transitions (cadr re) in medio)
                        (compile-transitions (append '(c) (cddr re)) medio fin)))))
        ;; Caso ricorsivo: somma di RE
        ((eq (car re) 'a)
            (if (null (cdr re))
                '()
                (let ((med1 (gensym "Q"))
                    (med2 (gensym "Q")))
                    (append (list (list in '() med1))
                        (list (list med2 '() fin))
                        (compile-transitions (cadr re) med1 med2)
                        (compile-transitions (append '(a) (cddr re)) in fin)))))
        ;; Caso ricorsivo: chiusura di Kleene
        ((and (eq (car re) 'z) (not (null (cdr re))))
            (let ((med1 (gensym "Q"))
                (med2 (gensym "Q")))
                (append (list (list in '() med1) 
                    (list med2 '() med1) 
                    (list med2 '() fin) 
                    (list in '() fin))
                    (compile-transitions (cadr re) med1 med2))))
        ;; Caso ricorsivo:
        ((and (eq (car re) 'o) (not (null (cdr re))))
            (let ((med1 (gensym "Q"))
                (med2 (gensym "Q")))
                (append (list (list in '() med1)
                    (list med2 '() med1)
                    (list med2 '() fin))
                    (compile-transitions (cadr re) med1 med2))))

        ;; S-exp generica (trattata come simbolo atomico composto)
        (t nil)))


;;; Funzione effettivamente utilizzata nella REPL
;;; L'automa ritornato è in forma (:NFSA iniziale finale transitions), dove
;;; iniziale e finale sono lo stato iniziale e finale dell'automa ottenuti con gensym
;;; transitions è una lista che contiene le transizioni dell'automa viste come
;;; liste (stato-partenza simbolo-di-transizione stato-destinazione)
(defun nfsa-compile-regex (re)
    "Compila una regex in un automa NFSA. Ritorna NIL se non valida."
    (if (not (is-regex re))
        nil
        (let* ((iniziale (gensym "Q"))
                (finale (gensym "Q"))
                (transitions (compile-transitions re iniziale finale)))
            (list :NFSA iniziale finale transitions))))


;;; Funzione per controllare l'accettazione di un input da parte di un automa
(defun nfsa-recognize (fa input)
    "Ritorna vero quando l'input è accettato dall'fa"
    )


;;;; End of file nfsa.lisp