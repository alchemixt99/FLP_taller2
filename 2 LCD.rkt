#lang eopl
#| INTEGRANTES

Jhon Erik Avila Ortiz          - 1210209
Brayan Lara Vargas             - 1510322
Sebastian Guacheta Salazar     - 0938596

|#
#|
Descripción del lenguaje LCD

<chip>         := <chip-prim>
                  <prim-chip(p chip)>
               := chip (--> {(port)}*)
                       (<-- {(port)}*)
                       <circuito>
                       <prim-chip(p chip)>

<circuito>     := circ_simple ({cable}*)
                              ({cable}*)
                       <simple-circuit(icl,ocl,chip)>

               := circ_comp <circuito> {<circuito>}+
                            input {cable}*
                            output {cable}*
                       <complex-circuit(circ,lcircs,icl,ocl)>

<chip-prim>    := ch_or
               := <chip-or()>

               := ch_not
               := <chip-not()>

               := ch_and
               := <chip-and()>

               := ch_xor
               := <chip-xor()>

               := ch_nand
               := <chip-nand()>

               := ch_nor
               := <chip-nor()>

               := ch_xnor
               := <chip-xnor()>
|#


;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (port
   (letter (arbno (or letter digit "?"))) symbol)
  (cable
   (letter (arbno (or letter digit "?"))) symbol)
  ))

;Especificación Sintáctica (gramática)
(define grammar-simple-interpreter
  '((program (chip) a-chip)
    #| ---------- Expresiones  -------------------|#
    (chip (port)
          port-exp)
    (chip (cable)
          cable-exp)
    (chip (chip-prim)
          prim-chip )
    (chip ((separated-list port " ") (separated-list port " ") circuito)
          comp-chip)
    (circuito ((separated-list cable " ") (separated-list cable " ") chip)
              simple-circuit)
    (circuito (circuito (separated-list circuito " ") (separated-list cable " ") (separated-list cable " "))
              complex-circuit)
    
    #| ---------- Primitivas (Compuertas)  -------------------|#
    (chip-prim ("or") chip-or)
    (chip-prim ("not") chip-not)
    (chip-prim ("and") chip-and)
    (chip-prim ("xor") chip-xor)
    (chip-prim ("nand") chip-nand)
    (chip-prim ("nor") chip-nor)
    (chip-prim ("xnor") chip-xnor)
    
   ))
;(crear_chip (simple-circuit '(a b) '(c) (prim-chip (chip-or))) '(INA INB) '(OUTC))
(define crear_chip
  (lambda (crc ipl opl)
    (comp-chip ipl opl crc)
  )
)
;(crear_chip_prim x) átomos:‘or, ‘and, ‘not, ‘xor, ‘nand, ‘nor, ‘xnor  
(define crear_chip_prim
  (lambda (x)
    (cond
    [(eqv? x 'or)(prim-chip (chip-or))]
    [(eqv? x 'and)(prim-chip (chip-and))]
    [(eqv? x 'not)(prim-chip (chip-not))]
    [(eqv? x 'xor)(prim-chip (chip-xor))]
    [(eqv? x 'nand)(prim-chip (chip-nand))]
    [(eqv? x 'nor)(prim-chip (chip-nor))]
    [(eqv? x 'xnor)(prim-chip (chip-xnor))]
    )
  )
)

;;(crear_circuito lcxs icl ocl)
(define crear_circuito
  (lambda (lcxs icl ocl)
    (if(chip? (car (car lcxs)))
       (complex-circuit
         (simple-circuit (cadr (car lcxs)) (caddr (car lcxs)) (car (car lcxs)))
         (list
           (simple-circuit (cadr (cadr lcxs)) (caddr (cadr lcxs)) (car (cadr lcxs)))       
         )
         icl
         ocl
       )
       "debe ingresar un chip en esta posición, ¿que le pasa?, ¿se quiere tirar flp o que?")
  )
)

   #| ---------- Datatypes  -------------------|#
;#|
(define-datatype program program?
  (a-chip (a-chip chip?)))
 (define-datatype chip chip?
  (port-exp (port-exp symbol?))
  (cable-exp (cable-exp symbol?))
  (prim-chip (prim-chip chip-prim?))
  (comp-chip
   (input-ports (list-of symbol?))
   (output-ports (list-of symbol?))
   (circuit circuito?)))
 (define-datatype circuito circuito?
  (simple-circuit
   (input-cables (list-of symbol?))
   (output-cables (list-of symbol?))
   (chip chip?))
  (complex-circuit
   (circuito circuito?)
   (circuitos
    (list-of circuito?))
   (input-cables (list-of symbol?))
   (output-cables (list-of symbol?))))
 (define-datatype chip-prim chip-prim?
  (chip-or)
  (chip-not)
  (chip-and)
  (chip-xor)
  (chip-nand)
  (chip-nor)
  (chip-xnor))
;|#

;============== PRUEBAS ============================
;Sumador
;#|
(define sumador
  (comp-chip
   '(INA INB INC IND)
   '(OUTA)
  (complex-circuit (simple-circuit '(a b) '(e) (prim-chip (chip-and))) 
                   (list (simple-circuit '(c d) '(f) (prim-chip (chip-and)))
                         (simple-circuit '(e f) '(g) (prim-chip (chip-or )))
                   )   
  '(a b c d)
  '(g)
  )
  )
)
;|#

;Prueba lcxs
(define lcxs
  (list
    (list
      (comp-chip
       '(INA INB INC IND)
       '(OUTE OUTF)
       (complex-circuit
          (simple-circuit '(a b) '(e) (prim-chip (chip-and)))
             (list
                (simple-circuit '(c d) '(f) (prim-chip (chip-and)))
              )
             '(a b c d)
             '(e f)
       )
      )
      '(m n o p)
      '(e f)
     )
    (list
      (comp-chip
       '(INE INF)
       '(OUTA)
       (simple-circuit '(e f) '(g) (prim-chip (chip-or)))
      )
      '(e f)
      '(z)
     )
    )
  )
(define icl '(m n o p))
(define ocl '(z))

(crear_circuito lcxs icl ocl)