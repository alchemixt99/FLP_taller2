#lang eopl
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
;;(lcxs '(chip i o))

#|(define lcxs
  (lambda (ls)
    (if(chip? (car (car ls)))
       "es un chip"
       "debe ingresar un chip en esta posición, ¿que le pasa?, ¿se quiere tirar flp o que?")
    (if(list? (cadr (car ls)))
       "si señor, es una lista"
       "¿usted viene de banderas cierto?, por aquí no hay listas")
    (if(list? (caddr (car ls)))
       "si señor, también es una lista"
       "¿usted viene de banderas cierto?, por aquí no hay listas")
  )
)|#

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
#|
(define-datatype program program? (a-chip (a-chip15 chip?)))
 (define-datatype
  chip
  chip?
  (port-exp (port-exp16 symbol?))
  (cable-exp (cable-exp17 symbol?))
  (prim-chip (prim-chip18 chip-prim?))
  (comp_chip (comp_chip19 (list-of symbol?)) (comp_chip20 (list-of symbol?)) (comp_chip21 circuito?)))
 (define-datatype
  circuito
  circuito?
  (simple-circuit (simple-circuit22 (list-of symbol?)) (simple-circuit23 (list-of symbol?)) (simple-circuit24 chip?))
  (complex-circuit
   (complex-circuit25 circuito?)
   (complex-circuit26 (list-of circuito?))
   (complex-circuit27 (list-of symbol?))
   (complex-circuit28 (list-of symbol?))))
 (define-datatype chip-prim chip-prim? (chip-or) (chip-not) (chip-and) (chip-xor) (chip-nand) (chip-nor) (chip-xnor))
|#
;#|
   (sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

   (define show-the-datatypes
     (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))
;|#

;============== PRUEBAS ============================
;Sumador
(define sumador (simple-circuit '(a b) '(e)  (prim-chip (chip-and))))

;complex-circuit
;#|
(define test_comp_chip
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