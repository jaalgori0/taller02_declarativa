#lang racket

;;Ejercicio 1 – Contar elementos positivos en una lista
(define (contar-positivos lst)
  (length (filter (lambda (x) (> x 0)) lst)))

;;Ejemplo
(contar-positivos '(3 -2 7 0 -5 9))
;; => 3


;;Ejercicio 2 – Generar lista de cuadrados pares
(define (cuadrados-pares lst)
  (map (lambda (x) (* x x))
       (filter (lambda (x) (even? x)) lst)))

;;Ejemplo
(cuadrados-pares '(1 2 3 4 5 6 7 8))
;; => '(4 16 36 64)


;;Ejercicio 3 – Calcular el factorial de un número
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;;Ejemplo
(factorial 5)
;; => 120


;;Ejercicio 4 – Elevar cada número al cubo
(define (cubos lst)
  (map (lambda (x) (* x x x)) lst))

;;Ejemplo
(cubos '(2 3 4))
;; => '(8 27 64)


;;Ejercicio 5 – Sumar todos los elementos impares
(define (sumar-impares lst)
  (foldl + 0
         (filter (lambda (x) (odd? x)) lst)))

;;Ejemplo
(sumar-impares '(1 2 3 4 5 6 7))
;; => 16


;;Ejercicio 6 – Determinar si una lista contiene números negativos
(define (hay-negativos? lst)
  (ormap (lambda (x) (< x 0)) lst))

;;Ejemplo
(hay-negativos? '(5 9 -3 2))
;; => #t


;;Ejercicio 7 – Calcular la suma acumulada de una lista
(define (suma-acumulada lst)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lst)))

;;Ejemplo
(suma-acumulada '(1 2 3 4))
;; => '(1 3 6 10)


;;Ejercicio 8 – Concatenar cadenas de texto en una lista
(define (concatenar-cadenas lst)
  (foldl string-append "" lst))

;;Ejemplo
(concatenar-cadenas '("Hola" " " "Mundo"))
;; => "Hola Mundo"


;;Ejercicio 9 – Generar lista con el doble de los números mayores que 5
(define (dobles-mayores-5 lst)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lst)))

;;Ejemplo
(dobles-mayores-5 '(3 6 8 2 10))
;; => '(12 16 20)


;;Ejercicio 10 – Invertir el orden de una lista
(define (invertir lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))

;;Ejemplo
(invertir '(1 2 3 4))
;; => '(4 3 2 1)


;;Ejercicio 11 – Crear una función que reciba una función como parámetro
(define (aplicar-funcion f lst)
  (map f lst))

;;Ejemplo
(aplicar-funcion (lambda (x) (* x x)) '(1 2 3 4))
;; => '(1 4 9 16)


;;Ejercicio 12 – Reto integrador: promedio de números mayores a 5
(define (promedio-mayores-5 lst)
  (let* ((mayores (filter (lambda (x) (> x 5)) lst))
         (suma (foldl + 0 mayores)))
    (/ suma (length mayores))))

;;Ejemplo
(promedio-mayores-5 '(3 8 10 4 9 2 7))
;; => 8.5
