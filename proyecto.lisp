;; Definición de la función factorial usando DEFUN
(defun factorial (n)
  (cond ((= n 0) 1)               ; Caso base: factorial de 0 es 1
        (t (* n (factorial (- n 1))))))  ; Caso recursivo: n * factorial(n-1)

;; Uso de predicados para validar el argumento de entrada
(defun factorial-wrapper (n)
  (cond ((listp n)               ; Comprobación si n es una lista
         (error "El argumento no puede ser una lista"))
        ((not (integerp n))      ; Comprobación si n no es un entero
         (error "El argumento debe ser un entero"))
        ((< n 0)                 ; Comprobación si n es negativo
         (error "El argumento debe ser un entero no negativo"))
        (t (factorial n))))      ; Llamar a la función factorial si n es válido

;; Ejemplo de uso
(print "Ingrese el numero: ")
(setq n (read))

(format t "El factorial de ~d es: ~d~%" 'n (factorial-wrapper n))