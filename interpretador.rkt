#lang eopl
;******************************************************************************************
;;;;; Interpretador para lenguaje para el taller 3 ;;;;;;;;;;;

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>       ::= <expresion>
;;                      <un-programa (exp)>
;;  <expresion>    ::= <numero>
;;                      <numero-lit (num)>
;;                  ::= "\" "<texto>" "\"
;;                       texto-lit (txt)
;;                  ::= <identificador>
;;                      <var-exp (id)>
;;                  ::= ( <expresion> <primitiva-binaria> <expresion>)
;;                      primapp-bin-exp (exp1 prim-binaria exp2)>
;;                  
;;                  ::= <primitiva-unaria> (<expresion>)
;;                       primapp-un-exp(prim-unaria exp)
;;
;;  <primitiva-binaria>    ::=  + | - | * | / | concat | > | < | >= | <= | != | ==
;;
;;  <primitiva-unaria>     ::=  longitud | add1 | sub1 | neg 

;******************************************************************************************
 
;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identificador
   ("@" letter (arbno (or letter digit "?"))) symbol)
  (numero
   (digit (arbno digit)) number)
  (numero
   ("-" digit (arbno digit)) number)
   (texto
     ("\"" (arbno (not #\")) "\"") string)))

;; Definicion sintactica (Gramatica)

(define grammar-simple-interpreter
  '((programa (expresion) un-programa)
    (expresion (numero) numero-lit)
    (expresion (texto) texto-lit)
    (expresion (identificador) var-exp)
    (expresion
      ("(" expresion primitiva-binaria expresion ")")
           primapp-bin-exp)
    (expresion
      ( primitiva-unaria "("expresion ")")
           primapp-un-exp)
    (expresion
      ("Si" expresion "{" expresion "}" "sino" "{" expresion "}")
        condicional-exp)

    (expresion
      ("declarar" "(" (arbno identificador "=" expresion ";") ")" "{" expresion "}")
        variableLocal-exp)

    (expresion
     ("procedimiento" "(" (arbno identificador "," ) ")" "{" expresion "}")
      procedimiento-ex)

    (expresion
      ("evaluar" expresion "(" (arbno expresion ",") ")" "finEval")
        app-exp)

    (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) 
                letrec-exp)
    
;; Primitivas binarias
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria ("!=") primitiva-diferente)
    (primitiva-binaria ("==") primitiva-comparador-igual)

;; Primitivas unarias
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-negacion-booleana)
    
    ))



;; Definicion de procVal
(define-datatype procVal procVal?
  (cerradura (lista-ID (list-of symbol?))
             (exp expresion?)
             (amb environment?)
             )
  )


;; Se usa sllgen para construir los datatypes

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;;front-end para interpretar los strings de entrada

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;; interpretador, el cual necesita la evalucacion definida posteriormente

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-programa  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;; interprete

(define eval-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (eval-expresion body (init-env))))))

;; Ambiente Inicial:

(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))

;; eval-expresion:
(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (datum) datum)
      (texto-lit (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-bin-exp (rand1 prim rand2)
                       (apply-primitiva-bin (eval-rand rand1 env) prim (eval-rand rand2 env)))
      (primapp-un-exp (prim exp)
                     (apply-primitiva-un prim (eval-rand exp env)))
      
      (condicional-exp (test true false)
                       (if (true-value? (eval-expresion test env))
                           (eval-expresion true env)
                           (eval-expresion false env)))

      (variableLocal-exp (ids exps cuerpo)
                         (let ((args (eval-rands exps env)))
                           (eval-expresion cuerpo
                                           (extend-env ids args env))))

      (procedimiento-ex (ids cuerpo)
                        (cerradura ids cuerpo env))

      (app-exp (proc-expr arg-exprs)
               (let* ((proc-val (eval-expresion proc-expr env))  
                      (evaluated-args (eval-rands arg-exprs env)))
                 (cases procVal proc-val
                   (cerradura (ids cuerpo closure-env)
                             
                              (let ((extended-env (extend-env ids evaluated-args closure-env)))
                                (eval-expresion cuerpo extended-env))))))

      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expresion letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      )))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expresion rand env)))

(define apply-primitiva-bin
  (lambda (rand1 prim rand2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ rand1 rand2))
      (primitiva-resta () (- rand1 rand2))
      (primitiva-div () (/ rand1 rand2))
      (primitiva-multi () (* rand1 rand2))
      (primitiva-concat () (string-append rand1 rand2))
      (primitiva-mayor () (if (> rand1 rand2) 1 0))
      (primitiva-menor () (if (< rand1 rand2) 1 0))
      (primitiva-mayor-igual () (if (>= rand1 rand2) 1 0))
      (primitiva-menor-igual () (if (<= rand1 rand2) 1 0))
      (primitiva-diferente () (if (not (eqv? rand1 rand2)) 1 0))
      (primitiva-comparador-igual () (if (eqv? rand1 rand2) 1 0))
      )))

(define apply-primitiva-un
  (lambda (prim exp)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length exp))
      (primitiva-add1 () (+ exp 1))
      (primitiva-sub1 () (- exp 1))
      (primitiva-negacion-booleana () (not exp))
      )))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))


;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))



;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

      
 ;;;Pruebas:

;; declarar(@x=2;@y=3;@a=procedimiento (@x,@y,@z,) {((@x+@y)+@z)};) { evaluar @a (1,2,@x,) finEval}

      