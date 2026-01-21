;;;; Bagini    Luca    923969

;;;; nfsa.lisp
;;;; Compilatore da RE a nfsa in Lisp

;;; Gestisco i casi base di RE

;;; Creo una funzione per stabilire se una Sexp utilizzi un simbolo
;;; riservato per non considerarle poi come simboli dell'alfabeto
(defparameter *reserved* '(c a z o))
(defun valid-sexp (re)
    "Ritorna vero se re è una Sexp non riservata"
    (and (listp re) (not (member (car re) *reserved*))))

(defun is-regex (re)
    "Ritorna vero se re è una regex"
    (cond ((null re) t)
        ((atom re) t)
        ((valid-sexp re) t)
        ((equal (car re) 'c) 
            (if (null (cdr re))
                t
                (and (is-regex (cadr re))
                    (is-regex (cons 'c (cddr re))))))
        ((equal (car re) 'a) 
            (if (null (cdr re))
                t
                (and (is-regex (cadr re))
                    (is-regex (cons 'a (cddr re))))))
        ((equal (car re) 'z) (is-regex (cadr re)))
        ((equal (car re) 'o) (is-regex (cadr re)))
        (t '())))


;;; Compilazione in automa

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
        ((equal (car re) 'c)
            (if (null (cdr re))
                (list (list in '() fin))
                (let ((medio (gensym "Q")))
                    (append (compile-transitions (cadr re) in medio)
                        (compile-transitions (cons 'c (cddr re)) medio fin)))))
        ;; Caso ricorsivo: somma di RE
        ((equal (car re) 'a)
            (if (null (cdr re))
                '()
                (let ((med1 (gensym "Q"))
                    (med2 (gensym "Q")))
                    (append (list (list in '() med1))
                        (list (list med2 '() fin))
                        (compile-transitions (cadr re) med1 med2)
                        (compile-transitions (cons 'a (cddr re)) in fin)))))
        ;; Caso ricorsivo: chiusura di Kleene
        ((and (equal (car re) 'z) (not (null (cdr re))))
            (let ((med1 (gensym "Q"))
                (med2 (gensym "Q")))
                (append (list (list in '() med1) 
                    (list med2 '() med1) 
                    (list med2 '() fin) 
                    (list in '() fin))
                    (compile-transitions (cadr re) med1 med2))))
        ;; Caso ricorsivo: RE+
        ((and (equal (car re) 'o) (not (null (cdr re))))
            (let ((med1 (gensym "Q"))
                (med2 (gensym "Q")))
                (append (list (list in '() med1)
                    (list med2 '() med1)
                    (list med2 '() fin))
                    (compile-transitions (cadr re) med1 med2))))
        (t '())))

;;; Funzione effettivamente utilizzata nella REPL
;;; L'automa ritornato è in forma (:NFSA iniziale finale transitions),
;;; dove iniziale e finale sono lo stato iniziale e finale dell'automa ottenuti
;;; con gensym e transitions è una lista che contiene le transizioni
;;; dell'automa viste come liste (stato-partenza simbolo-di-transizione 
;;; stato-destinazione)
(defun nfsa-compile-regex (re)
    "Compila una regex in un automa NFSA e lo ritorna come quadrupla"
    (if (not (is-regex re))
        '()
        (let ((iniziale (gensym "Q"))
            (finale (gensym "Q")))
            (list :NFSA iniziale finale 
                (compile-transitions re iniziale finale)))))


;;; Riconoscimento di un input

;;; Funzione di ausilio per trovare il prossimo stato
;;; dato uno stato di partenza e un simbolo
(defun prossimi-stati (iniziale simbolo delta)
    "Funzione ricorsiva per trovare i prossimi stati"
    (if (null delta) 
        '()
        (let ((transizione (car delta)))
            (if (and
                    (equal (car transizione) iniziale)
                    (equal (cadr transizione) simbolo))
                (cons (caddr transizione)
                    (prossimi-stati iniziale simbolo (cdr delta)))
                (prossimi-stati iniziale simbolo (cdr delta))))))

;;; Funzione di ausilio per portare avanti la computazione
;;; data una lista di stati iniziali, uno finale e una lista di transizioni
(defun computa-lista (iniziali input finale delta)
    "Funzione ricorsiva su lista per stabilire l'accettazione di un input"
    (if (null iniziali)
        '()
        (or (computa (car iniziali) input finale delta)
            (computa-lista (cdr iniziali) input finale delta))))


;;; Funzione di ausilio per portare avanti la computazione
;;; dati un singolo stato iniziale, uno finale e una lista di transizioni
(defun computa (iniziale input finale delta)
    "Funzione ricorsiva su uno stato per stabilire l'accettazione di un input"
    (if (and (null input) (null (prossimi-stati iniziale '() delta)))
        (equal iniziale finale)
        (or (computa-lista 
                (prossimi-stati iniziale (car input) delta)
                (cdr input)
                finale
                delta)
            (computa-lista 
                (prossimi-stati iniziale '() delta)
                input
                finale
                delta))))

;;; Funzione effettivamente utilizzata nella REPL
;;; per controllare l'accettazione di un input da parte di un automa
(defun nfsa-recognize (fa input)
    "Ritorna vero quando l'input è accettato da fa"
    (cond 
        ((not (and (listp fa) (equal (car fa) :NFSA)))
            (error "~S non è un automa NFSA valido" fa))
        ((not (and (listp input) (is-regex input)))
            (error "~S non è una regex valida" input))
        (t (computa (second fa) input (third fa) (fourth fa)))))

;;;; Fine del file nfsa.lisp