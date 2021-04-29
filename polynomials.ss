#lang scheme
;Integrantes: Hernan Diaz y Gina Ozimisa

(define (split1 A) ;FUNCION QUE TRANSFORMA EXPRESION SYMBOL A STRING Y LOS INGRESA A UNA LISTA
  (if (number? A)(number->string A)
  (let
      (
   (B (string->list (symbol->string A)))
         )
   (map string B)
    )
  ))

(define (num A v);si la lista es ("1" "2" "x") devuelve ("1" "2")
  (if (null? A) A
      (if (string->number (car A)) (cons (car A) (num (cdr A) v))
          v
          )))

(define (saltarnum A);si la lista es ("1" "2" "x") devuelve ("x")
  (if (null? A) A
      (if (string->number (car A)) (saltarnum (cdr A))
      A)
   )
  )

(define (digitos A) ;si la lista es ("1" "2") devuelve ("12")
  (if (null? A) A
      (cond
        ((null? (cdr A)) (list(car A)))
        ((and (string->number (car A)) (string->number (cadr A))) (cons (string-append* (car A) (num(cdr A)(list))) (digitos (saltarnum A))))
        (else (cons (car A) (digitos (cdr A))))
      )
  ))

(define (frac A) ;si la lista es ("1" "/" "2") devuelve ("1/2")
  (if (null? A) A
      (cond
        ((null? (cdr A))(list (car A)))
        ((and (string->number (car A))(equal? "/" (cadr A))(string->number (caddr A))) (cons (string-append* (list (car A)(cadr A)(caddr A))) (frac (cdddr A))))
        (else(cons (car A)(frac (cdr A))))
          )
      )
  )

(define (split2 A) ;LE ASIGNA UN SIGNO NEGATIVO AL "1" EN DIFERENTES CASOS , DE LO CONTRARIO SI EL NUMERO ES DISTINTO DE 1 SOLO LO TRANSFORMA DE STRING A NUMBER
  (if
   (null?  A)
   A
   (cond
     ((and (equal? (car A) "^")(equal? (cadr A) "-")(equal? (caddr A) "1")) (cons "^"(cons "-1" (split2(cdddr A)))))
     ((and(equal? "-" (car A))(equal? "1" (cadr A))) (cons "-"(cons "-1" (split2 (cddr A)))))
     ((and(string->number (car A)) (not(equal? "1" (car A)))) (cons (string->number(car A))(split2 (cdr A))))
     (else (cons (car A)(split2 (cdr A)))))))

(define (split3 A) ;COLOCA SIGNO ANTES DEL VALOR NUMERICO O DE X (X toma el valor de 1)
  (if
   (null? A)
   A
   (cond
     ((and(equal? (car A)"-")(number? (cadr A)))(cons "-"(cons(* (cadr A) -1)(split3 (cddr A)))))
     ((and(equal? (car A)"-")(equal? (cadr A) "x"))(cons "-"(cons -1 (split3 (cddr A)))))
     ((and(equal? (car A)"+")(equal? (cadr A) "x"))(cons "+"(cons 1 (split3 (cddr A)))))
     ((and (equal? (car A) "^") (equal? (cadr A) "-")) (cons "^"(cons (*(caddr A) -1)(split3 (cdddr A)))))
     (else
      (cons (car A)(split3 (cdr A)))))))


(define (ext1 L) ;INGRESA LOS VALORES DEL POLINOMIO A UNA SUBLISTA CON LA ESTRUCTURA (coef, grade)
  (if
   (null? L)
   '()
   (cond
     ((equal? (car L) "-")(ext1 (cdr L)));empieza con negativo
     ((equal? (car L) "+")(ext1 (cdr L)));empieza con positivo
     ((and(equal? (car L)"x")(null? (cdr L)))(cons (list 1 1)(ext1 (cdr L)))) ; x (expt 1)
     ((and (equal? (car L)"x")(or (equal? (cadr L)"+")(equal?(cadr L)"-")))(cons (list 1 1)(ext1 (cdr L)))) ;caso x sin simbolo
     ((and (equal? (car L) "x") (equal? (cadr L) "^") (equal? (caddr L) "-1")) (cons (list 1 -1) (ext1 (cdddr L)))); x^-1 al princpio
     ((and(equal?(car L) "x")(equal? (cadr L)"^"))(cons (list 1 (caddr L))(ext1 (cdddr L)))) ;caso x expt num (esta solo sin signo)
     ((equal? (car L) "1") (cons (list 1 0) (ext1 (cdr L)))) ;1
     ((equal? (car L) "-1") (cons (list -1 0) (ext1 (cdr L)))) ;-1
     ((and (number? (car L))(not(= (car L) 1))(not(= (car L) -1)) (null? (cdr L))) (cons (list (car L) 0) (ext1(cdr L))));termino independiente al final
     ((and (number? (car L))(not(= (car L) 1))(not(= (car L) -1)) (not(equal? (cadr L) "x"))) (cons (list (car L) 0) (ext1(cdr L))));termino independiente
     ((and (negative? (car L))(null? (cdr L)))(cons (list (car L) 1)(ext1 (cdr L)))) ; -x (expt 1)
     ((and (positive? (car L))(null? (cdr L)))(cons (list (car L) 1)(ext1 (cdr L)))) ; +x (expt 1)
     ((and (= (car L) -1)(equal? (cadr L) "^")(equal? (caddr L) "-1")) (cons (list -1 -1) (ext1 (cdddr L))));-x^-1 para cualquier lado
     ((and (= (car L) 1)(equal? (cadr L) "^")(equal? (caddr L) "-1")) (cons (list 1 -1) (ext1 (cdddr L))));+x^-1 para cualquier lado
     ((and (number? (car L))(equal? (cadr L) "x") (null? (cddr L))(cons (list (car L) 1)(ext1 (cddr L)))));numx al final
     ((and (positive? (car L))(equal?(cadr L)"x")(or(equal?(caddr L)"+")(equal?(caddr L)"-")))(cons (list (car L) 1)(ext1 (cddr L)))) ;caso num x (elevado a 1) (inicio)
     ((and (negative? (car L))(equal?(cadr L)"x")(or(equal?(caddr L)"+")(equal?(caddr L)"-")))(cons (list (car L) 1)(ext1 (cddr L))));caso -num x (elevado a 1 (inicio) 
     ((and (negative? (car L)) (equal? (cadr L)"^"))(cons (list(car L)(caddr L))(ext1 (cdddr L)))) ; caso -x num expt num
     ((and (positive? (car L)) (null? (cdr L)))(cons (list 1 1)(ext1(cdr L))));+x al final (lleva + antes)
     ((and (positive? (car L))(or(equal?(cadr L)"-")(equal?(cadr L)"+")))(cons(list 1 1)(ext1 (cdr L)))); x al medio
     ((and (negative? (car L))(or(equal?(cadr L)"+")(equal?(cadr L)"-")))(cons(list -1 1)(ext1 (cdr L))));-x al medio
     ((and (positive? (car L)) (equal? (cadr L)"^"))(cons (list(car L)(caddr L))(ext1 (cdddr L)))) ; caso +x num expt num
     ((and (number? (car L))(equal? (cadr L) "x") (equal? (caddr L) "^")(equal? (cadddr L) "-1")) (cons (list (car L) -1) (ext1 (cddddr L))));num x ^-1
     ((and (number? (car L))(number? (cadddr L)))(cons (list (car L)(cadddr L)) (ext1 (cddddr L)))) ;caso -numero x expt -num
     (else
      (ext1 (cdr L))))))

(define (poly P) ;FUNCION QUE DEVUELVE EL POLINOMIO EN SUBLISTA
  (ext1(split3(split2(frac(digitos(split1 P))))))
  )

(define (maximum L) ;DE UNA LISTA DEVUELVE EL MAYOR VALOR
     (if (null? (cdr L)) 
         (car L) 
         (if (< (car L) (maximum (cdr L)))  
             (maximum (cdr L)) 
             (car L)
         )
    )
)



(define (poly-degree P) ;DEVUELVE EL GRADO DEL POLINOMIO
  (cadr(car(sort (poly P)#:key cadr >)))
    )

(define (suma  P Q v)  ; SUMA LOS COEFICIENTES DEL POLINOMIO , EN CASO CONTRARIO LOS CONCATENA EN LA LISTA
  (cond
    ((null? P) (append Q v))
    ((null? Q) (append P v))
    ((and (=(+ (car(car P))(car(car Q)))0)(=(cadr(car P))(cadr(car Q))))(suma (cdr P)(cdr Q)v))
    ((= (cadr(car P))(cadr (car Q)))(cons (list (+ (car (car P))(car(car Q)))(cadr(car P)))(suma (cdr P) (cdr Q) v)))
    ((< (cadr(car P))(cadr(car Q)))(cons (car P)(suma (cdr P) Q v)))
    ((< (cadr(car Q))(cadr(car P)))(cons (car Q)(suma (cdr Q) P v)))
   )
)

(define (poly-add P Q) ;DEVUELVE EL RESULTADO DE SUMAR 2 POLINOMIOS
  (poly-t2(suma  (sort (poly P)#:key cadr <) (sort (poly Q)#:key cadr <) (list)))
  )

(define (multp A B) ; CONSECUTIVAMENTE LOS MONOMIOS DE LA LISTA A MULTIPLICAN LOS COEFICIIENTES Y SUMA LOS GRADOS CON TODOS LAS SUBLISTAS DE B
  (map (lambda (x)
         (map (lambda(y)
                (list(* (car x)(car y))(+ (cadr x)(cadr y))))A)
         )B))

(define (sumamult L) ;DEL RESULTADO DE LA MULTIPLICACION DE SUBLISTAS LOS QUE CONTENGAN IGUAL GRADO SE SUMARAN
  (cond
     ((null? (cdr L))(sort(car L)#:key cadr <))
  (else (suma (sort(car L)#:key cadr <)(sumamult (cdr L))(list))))
  )

(define (poly-multiply P Q) ;RESULTADO DE MULTIPLICAR DOS POLINOMIOS
  (poly-t2(sort(sumamult (multp (poly P)(poly Q)))#:key cadr >))
  )


(define (eval x L) ; x  toma un valor que se reemplaza en un polinomio , el resultado de evaluar x en cada monomio se ira sumando 
   (if
    (null? L)
    0
    (let*
        (
         (C (+(*(expt x (cadr(car L)))(car(car L)))(eval x (cdr L))))
         )
      C
      )))

(define (poly-eval P x) ;RESULTADO DE EVALUAR X EN UN POLINOMIO
  (eval x (poly P))
  )


(define (div P Q) ;TOMA EL MONOMIO DE MAYOR GRADO DE P Y LO DIVIDE CON EL DE Q
  (list (/ (car(car P))(car(car Q)))(-(cadr(car P))(cadr(car Q))))
  )

(define (division1 P Q) ; Del resuitado de dividir el monomio de mayor grado de P con Q , este se multiplica con todos los elementos de Q
 (sort(map (lambda (X)
         (list(* (car X) -1)(cadr X)))(sumamult(multp(list(div P Q))(sort Q #:key cadr <))))#:key cadr >))

(define (division2 P Q) ; Del resultado de la multiplicacion en division1 , este se suma con P
  (suma P (division1 P Q)(list))
    )

(define (division3 P Q) ; GUARDA EL VALOR DEL RESULTADO DE LA DIVISION
  (if ( <= (-(cadr(car P))(cadr(car Q))) 0)
     (list(div P Q))
     (cons (div P Q) (division3 (division2 P Q) Q))
    ))


(define (division4 P Q) ; GUARDA EL VALOR DEL RESTO DE LA DIVISION
  (if ( <= (-(cadr(car P))(cadr(car Q))) 0)
     (division2 P Q)
     (division4 (division2 P Q) Q)
    ))

(define (poly-quotient P Q) ;RESULTADO DE DIVIDIR 2 POLINOMIOS
  (poly-t2(division3(sort (poly P)#:key cadr >) (sort (poly Q)#:key cadr >))))


(define (poly-remainder P Q) ;DEVUELVE EL RESTO ENTRE 2 POLINOMIOS
   (poly-t2(division4(sort(poly P)#:key cadr >) (sort (poly Q)#:key cadr >))))
  

(define (poly-t P) ;TRANSFORMA LAS SUBLISTAS CONFORMADAS POR NUMEROS EN STRINGS
  (if
   (null? P) P
  (cond
   ((and (=(car(car P)) 1)(=(cadr(car P)) 0)) (cons "+1" (poly-t (cdr P)))) ;1
   ((and (=(car(car P)) -1)(=(cadr(car P)) 0)) (cons "-1" (poly-t (cdr P))));-1
   ((and (=(car(car P)) 1)(=(cadr(car P)) 1)) (cons "+x" (poly-t (cdr P)))) ;x
   ((and (=(car(car P)) -1)(=(cadr(car P)) 1)) (cons "-x" (poly-t (cdr P))));-x
   ((and (positive? (car(car P)))(=(cadr(car P)) 0)) (cons (string-append "+" (number->string(car(car P))))(poly-t (cdr P))));+numero
   ((=(cadr(car P)) 0) (cons (number->string(car(car P)))(poly-t (cdr P))));numero
   ((and (positive? (car(car P))) (=(cadr(car P)) 1)) (cons (string-append "+" (number->string(car(car P))) "x")(poly-t (cdr P))));+numero x
   ((and (=(cadr(car P))1)) (cons (string-append (number->string(car(car P))) "x")(poly-t (cdr P))));-numero x
   ((=(car(car P)) 1) (cons (string-append "+x^" (number->string(cadr(car P)))) (poly-t (cdr P))));+x^grado
   ((=(car(car P)) -1) (cons (string-append "-x^" (number->string(cadr(car P)))) (poly-t (cdr P))));-x^grado
   ((and (positive? (car(car P))) (not(= (car(car P)) 1)) (not(=(cadr (car P)) 1))) (cons (string-append "+" (number->string(car(car P))) "x^" (number->string(cadr(car P)))) (poly-t (cdr P))));numero x^grado
   ((and (not(= (car(car P)) 1)) (not(=(cadr (car P)) 1))) (cons (string-append (number->string(car(car P))) "x^" (number->string(cadr(car P)))) (poly-t (cdr P))));numero x^grado
   )
  ))

(define (poly-t2 P) ; CONCATENA LOS VALORES DE STRINGS Y LUEGO LOS TRANSFORMA A SYMBOL
  (string->symbol(string-append* (poly-t P)))
  )

"EJEMPLOS TRABAJO"
"Ejemplo poly-degree 1"
(poly-degree 'x^2-3/2x+3)
"Ejemplo poly-degree 2"
(poly-degree '3x-5/4x^3-x^2+5)
"Ejemplo poly-degree 3"
(poly-degree 'x+4)
"Ejemplo poly-add 1"
(poly-add 'x+4 'x^2-x+1)
"Ejemplo poly-add 2"
(poly-add 'x^2+1/3x+4 '2/3x)
"Ejemplo poly-multiply 1"
(poly-multiply 'x-1 'x^2+x+1)
"Ejemplo poly-multiply 2"
(poly-multiply 'x^3+6x^2+5 '|2/3|)
"Ejemplo poly-quotient 1"
(poly-quotient 'x^3 'x-1)
"Ejemplo poly-remainder 1"
(poly-remainder 'x^3 'x-1)
"Ejemplo poly-eval 1"
(poly-eval 'x^2-x+1 1)
"Ejemplo poly-eval 2"
(poly-eval 'x^2-x+1 1/2)

"OTROS EJEMPLOS"
"poly add 1"
(poly-add '3x^3+4x^2+2x+6 '5x^3-2x^2+8x+7)
"poly add 2"
(poly-add '12x^5-17x^3+3x^2-10 '-6x^5-5x^4+7x^3+4x^2)
"poly-add 3"
(poly-add '10x^3+5x^2-3x-11 '8+3x-x^2+2x^3)
"poly-multiply 1"
(poly-multiply '6-3x 'x^2+8x-3)
"poly-multiply 2"
(poly-multiply '4x-1 '2x^2-5x+8)
"poly-multiply 3"
(poly-multiply '-3-5x+2x^2 '3x-9)
"poly-degree"
(poly-degree '3x^2-2x-8 )
"poly-eval"
(poly-eval '3x^2-2x-8 -2/3)
"poly quotient 1"
(poly-quotient 'x^4-2x^3-11x^2+30x-20 'x^2+3x-2)
"poly remainder 1"
(poly-remainder 'x^4-2x^3-11x^2+30x-20 'x^2+3x-2)
"poly-quotient2"
(poly-quotient '3x^2-2x-8 'x+2)
"poly-remainder2"
(poly-remainder '3x^2-2x-8 'x+2)