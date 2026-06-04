;;(load "C:\\Lisp\\TPI-Funcional-2026-Grupo12\\TPI-Funcional-2026-Grupo12\\Lisp\\core")

;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura (no posee efectos secundarios, solo crea la lista segun el estado actual y la accion siguiente del ciclo del semaforo)
;; ESTRATEGIA: No posee una una estrategia especfica, se trata de una funcion simple que selecciona un resultado en base a la evaluacion 
;; de funciones predicados (equal/and)
;; IMPACTO: Es una funcion no destructiva
;; ========================================================

(defun transicion (colorActual cambiarA)

	(cond 
		((and (equal colorActual 'en-rojo) (equal cambiarA 'amarillo))  (list colorActual (format nil "Cambiar-a-~a" cambiarA)))
		((and (equal colorActual 'en-amarillo) (equal cambiarA 'verde))  (list colorActual (format nil "Cambiar-a-~a" cambiarA)))
		((and (equal colorActual 'en-verde) (equal cambiarA 'rojo))  (list colorActual (format nil "'Cambiar-a-~a" cambiarA)))
		(t (list colorActual 'accion-por-defecto))
		)
	)

;; ========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (no posee efectos secundarios, devuelve el valor esperado de la funcion auxiliar a la que llama)
;; ESTRATEGIA: Es una funciones que prepara las variables para ser evaluadas en la funcion que se llama dentro de esta 
;;misma, la procedemos a clasificar como una funcion iniciadora/preparadora.
;; IMPACTO: Es una funcion no destructiva.
;; ========================================================

(defun timer (n)
	(let ((duraciones '((rojo 90) (amarillo 6) (verde 120))))
				(timerAux (mod n 216) duraciones)
	)
)

;; ========================================================
;; FUNCIÓN: timerAux
;; NATURALEZA: Pura (Devuelve los mismos valores esperados segun las condiciones que se analizan en n
;; junto con el parametro lista que se le envia)
;; ESTRATEGIA: Recursividad de cola (la llamada recursiva es la ultima operacion) - recorre la lista de duraciones y se compara el elemento numerico con el entero "n"
;; pasado como parametro, si el entero "n" es menor se encontro el color de esa posicion de tiempo, caso contrario de n > "al elemento
;; numerico de la lista duraciones" se procede a seguir recorriendo la lista repitiendo el proceso hasta que "n es menor" devuelva verdadero.
;; IMPACTO: Es una funcion no destructiva.
;; ========================================================


(defun timerAux (n duraciones)
		(cond 	((< n (cadar duraciones)) (caar duraciones))
				(t (timerAux (- n (cadar duraciones)) (cdr duraciones)))
	)
)