;;; Calcula el factorial de un número de forma recursiva.
;;;
;;; @param n El número entero del cual se calculará el factorial.
;;; @return El factorial de n.
(defun factorial (n)
  "Calcula el factorial de un número de forma recursiva."
  (cond ((= n 0) 1)               ; Caso base: factorial de 0 es 1
        (t (* n (factorial (- n 1))))))  ; Caso recursivo: n * factorial(n-1)

;;; Envuelve la función factorial para realizar validaciones de entrada.
;;;
;;; @param n El número entero del cual se calculará el factorial.
;;; @return El factorial de n, si n es un número entero no negativo.
;;; @error Si n es una lista, no es un número entero, o es negativo.
(defun factorial-wrapper (n)
  "Envuelve la función factorial para realizar validaciones de entrada."
  (cond ((listp n)               ; Comprobación si n es una lista
         (error "El argumento no puede ser una lista"))
        ((not (integerp n))      ; Comprobación si n no es un entero
         (error "El argumento debe ser un entero"))
        ((< n 0)                 ; Comprobación si n es negativo
         (error "El argumento debe ser un entero no negativo"))
        (t (factorial n))))      ; Llamar a la función factorial si n es válido

;;; Solicita al usuario un número y muestra su factorial.
(format t "Ingrese el numero: ")
(setq n (read))
(format t "El factorial de ~d es: ~d~%" n (factorial-wrapper n))