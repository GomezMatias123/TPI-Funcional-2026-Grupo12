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
;; misma, la procedemos a clasificar como una funcion iniciadora/preparadora, utiliza funciones de orden superior (reduce y mapcar) para lograr
;; adaptatividad, sin importar la duracion de cada color siempre logra ocupar la duracion total correspondiente para identificar que segundo correspone
;; a cada color.
;; IMPACTO: Es una funcion no destructiva.
;; ========================================================

(defun timer (n)
	(let ((duraciones '((rojo 90) (amarillo 6) (verde 120))))
				(timerAux (mod n (reduce '+ (mapcar 'cadr duraciones))) duraciones)
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



;; ========================================================
;; FUNCIÓN: registrarCambio 
;; NATURALEZA: Impura (posee efectos secundarios: imprime el cambio de
;; estado en la terminal y consulta la hora del sistema mediante 
;; get-universal-time, que depende del estado externo del entorno)
;; ESTRATEGIA: No conlleva ninguna estrategia, es una funcion simple que consulta el tiempo actual trancurrido hasta hoy desde los origenes de Lisp.
;; IMPACTO: Es una funcion no destructiva, brinda informacion pero no modifica ningun tipo de dato
;; ========================================================

(defun registrarCambiosEstado (colorAnterior siguienteColor)
	(let ((epoch (get-universal-time)))
		(format t "Tiempo ~a: la luz ha cambiado de ~a a ~a" epoch colorAnterior siguienteColor)
		)
)

;; ========================================================
;; FUNCIÓN: duracion-Ciclo
;; NATURALEZA: Pura realiza calculos y devuelve un valor numerico que representa el total de segundos de 1 ciclo sin importar el segundo que se le brinde.
;; Se podria pasar como parametero segundos mayores a 216 que corresponden a un segundo ciclo segun la duraciones del requerimento 2 e igualmente el calculo
;; seguiria dando la duracion de 1 ciclo
;; ESTRATEGIA: Se trata de una estragia que permite iniciar los valores para ser usados en otras funciones, se utiliza un let para definir el fin del ciclo correspondiente al segundo
;; del tiempo del ciclo que pasamos por parametro, luego se vuelve a llamar a la funcion auxiliar buscar-Fin-Ciclo para obtener el final del ciclo del siguiente ciclo para 
;; hacer la diferencia con finCiclo, como siempre es la diferencia de un ciclo siguiente con el anterior, sin importar en cual numero n ciclo nos encontremos, obtenemos el
;; valor correspondiente a 1 ciclo.
;; IMPACTO: Es no destructivo
;; ========================================================


(defun duracion-Ciclo(segundos)
	(let ((finCiclo (buscar-Fin-Ciclo segundos)))
			(- (buscar-Fin-Ciclo (+ finCiclo 1)) finCiclo)
	)
)
;; ========================================================
;; FUNCIÓN: buscar-Fin-Ciclo
;; NATURALEZA: Pura devuelve un valor numerico basado en la logica de la funcion timer, sirve para buscar el segundo donde termina un ciclo.
;; ESTRATEGIA: Recursividad de cola (la llamada recursiva (buscar-Fin-Ciclo (+ segundos 1)) es la última operación en la rama. Avanza segundo 
;; a segundo hasta detectar la transición final de 1 ciclo
;; IMPACTO: Es no destructiva
;; ========================================================


(defun buscar-Fin-Ciclo(segundos)
	(cond
		((and (equal (timer segundos) 'verde) (equal (timer (+ segundos 1)) 'rojo)) 
			(+ segundos 1))
		(t (buscar-Fin-Ciclo (+ segundos 1)))
	)
)
	



;; ========================================================
;; FUNCIÓN: recomendacion-Ciclo
;; NATURALEZA: Pura devuelve un string basado únicamente en la comparación del parámetro
;; ESTRATEGIA: Evalua con predicados el valor numéreico que se le pasa por parametro para imprimir la leyenda correspondiente 
;; IMPACTO: Es no destructiva
;; ========================================================

(defun recomendacion-Ciclo (duracion)

	(cond 
		((> duracion 150) "Ciclo demasiado largo: difícil de adaptar a la mentalidad del conductor. Se recomienda reducir las duraciones.")
		((< duracion 35) "Ciclo demasiado corto: difícil de adaptar a la mentalidad del conductor. Se recomienda aumentar las duraciones.")
		(t "Ciclo dentro del rango óptimo (35-150 segundos).") 
	)

)


;; ========================================================
;; FUNCIÓN: ciclos-Por-Minuto
;; NATURALEZA: Pura, devuelve un valor numérico calculado a partir de operaciones aritméticas.
;; ESTRATEGIA: convierte los minutos recibidos a segundos multiplicando por 60, para luego realizar una división
;; mediante floor entre los segundos totales y la duración del ciclo obtenida en segundos por la funcion duracion-Ciclo.
;; obteniendo al final la cantidad de ciclos completos en el minuto pasado por parametro.
;; IMPACTO: No destructivo.
;; ========================================================

(defun ciclos-Por-Minuto (minutos)
	(floor (* minutos 60) (duracion-Ciclo 0))
)

;; ========================================================
;; FUNCIÓN:  
;; NATURALEZA: 
;; ESTRATEGIA: 
;; IMPACTO: 
;; ========================================================

