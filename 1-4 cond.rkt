#lang eopl
#| INTEGRANTES

Jhon Erik Avila Ortiz          - 1210209
Brayan Lara Vargas             - 1510322
Sebastian Guacheta Salazar     - 0938596

|#
;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales y ligadura local

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expresion>
;;                  ::= cond {Expression ==> Expression}* end
;;                  ::= let (identifier = expression)* in expression
;;                  ::= 
;;  <primitive>     ::= + | - | * | add1 | sub1 

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
   
    ; características adicionales
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    ;;punto 1.4 del taller 2 FLP
    (expression ("cond" expression "==>" expression (arbno expression "==>" expression) "end")
                cond-exp)
    
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    ;;;;;;
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    
    ;============= LISTAS ===============
    (primitive("list")   list-prim)
    (primitive ("car")    car-prim)
    (primitive ("cdr")    cdr-prim)
    (primitive ("empty-list") empty-list)
    (primitive ("null?") null-prim)
    (primitive ("nth-list") nth-prim)
    (primitive ("element-list") element-prim)

    ))


;Tipos de datos para la sintaxis abstracta de la gramática

;Construidos manualmente:

(define-datatype program program? (a-program (a-program16 expression?)))
 (define-datatype expression expression?
  (lit-exp (lit-exp number?))
  (var-exp (var-exp symbol?))
  (primapp-exp (primapp-prim primitive?) (primapp-args (list-of expression?)))
  (if-exp (test-exp expression?) (true-exp expression?) (false-exp expression?))
  (cond-exp
   (test-exp expression?)
   (val-exp expression?)
   (list-test (list-of expression?))
   (list-val (list-of expression?)))
  (let-exp (id-exp (list-of symbol?)) (body-exp (list-of expression?)) (evl-exp expression?)))

 (define-datatype primitive primitive?
  (add-prim)
  (substract-prim)
  (mult-prim)
  (incr-prim)
  (decr-prim)
  (list-prim)
  (car-prim)
  (cdr-prim)
  (empty-list)
  (null-prim)
  (nth-prim)
  (element-prim))

;(define-datatype program program?
;  (a-program
;   (exp expression?)))
;
;(define-datatype expression expression?
;  (lit-exp
;   (datum number?))
;  (var-exp
;   (id symbol?))
;  (primapp-exp
;   (prim primitive?)
;   (rands (list-of expression?)))
;  (if-exp
;   (test-exp expression?)
;   (true-exp expression?)
;   (false-exp expression?))
;  (let-exp
;   (ids (list-of symbol?))
;   (rans (list-of expression?))
;   (body expression?)))
;
;(define-datatype primitive primitive?
;  (add-prim)
;  (substract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim))

;Construidos automáticamente:

;(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

;(define show-the-datatypes
;  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))
(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     '(3 7 1)
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))

      ;;Punto 1.4 taller de FLP
      (cond-exp (test-exp val-exp list-test list-val)
                  (if (> 0 (eval-expression test-exp env))
                  (eval-expression val-exp env)
                  (if (true-value? (eval-expression (car list-test) env))
                                   (eval-expression (car list-val) env)
                                   (eval-expression (cond-exp test-exp val-exp (cdr list-test) (cdr list-val)) env)
                  )
                )
      )

      
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))

      )))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      ;;=====================
      (list-prim()  args)
      (car-prim()  (caar args))
      (cdr-prim()  (cdar args))
      (empty-list() '())
      (null-prim()
                 (cond
                   [(null? (car args)) "Lista vacía"]
                   [else "Lista no vacía"])
      )
      (nth-prim()
               (if (> (cadr args) (length (car args)))
                   "error, el arreglo posee un numero menor de posiciones"
                   (list-ref (car args) (cadr args))
               )
      )
      (element-prim()
                   (list-search (car args) (cadr args))
      )
      
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

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))))))


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

;;función auxiliar que permite buscar un elemento en una lista y retorna un booleano
(define list-search
  (lambda (ls arg)
      (if (= 0 (length ls))
          #f
          (if (= (car ls) arg) #t
              (list-search (cdr ls) arg))
      )      
  )
)



;******************************************************************************************
;Pruebas

;(show-the-datatypes)
(interpretador)