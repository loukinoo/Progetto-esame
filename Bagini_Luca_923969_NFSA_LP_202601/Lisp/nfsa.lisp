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

;;; Predicato effettivamente utilizzato nella REPL
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
                (list (list in '() fin)))
                (let ((medio (gensym "Q")))
                    (append (compile-transitions (cadr re) in medio)
                        (compile-transitions (append '(c) (cddr re)) medio fin)))
