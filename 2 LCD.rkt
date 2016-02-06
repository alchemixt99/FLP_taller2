#lang eopl
#|
Descripción del lenguaje LCD

<chip>         := <chip-prim>
               := chip (--> {(port)}*)
                       (<-- {(port)}*)
                       <circuito>
 
<circuito>     := circ_simple ({cable}*)
                              ({cable}*)

               := circ_comp <circuito> {<circuito>}+
                            input {cable}*
                            output {cable}*
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
    (chip (port) port-exp)
    (chip (cable) cable-exp)
    (chip (chip-prim) prim-chip )
    (chip ((separated-list port " ") (separated-list port " ") circuito) comp_chip)

    (circuito ((separated-list cable " ") (separated-list cable " ") chip) simple-circuit)
    ;;;;;;
    (chip-prim ("and") chip-and)
    (chip-prim ("or") chip-or)
    
    ))

(define-datatype program program? (a-chip (a-chip11 chip?)))
 (define-datatype
  chip
  chip?
  (port-exp (port-exp12 symbol?))
  (cable-exp (cable-exp13 symbol?))
  (prim-chip (prim-chip14 chip-prim?))
  (comp_chip (comp_chip15 (list-of symbol?)) (comp_chip16 (list-of symbol?)) (comp_chip17 circuito?)))
 (define-datatype
  circuito
  circuito?
  (simple-circuit
   (simple-circuit18 (list-of symbol?))
   (simple-circuit19 (list-of symbol?))
   (simple-circuit20 chip?)))
 (define-datatype chip-prim chip-prim? (chip-and) (chip-or))

#|
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))|#
(define sumador (simple-circuit '(a b) '(e)  (prim-chip (chip-and ))))