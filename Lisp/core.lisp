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
		((and (equal colorActual 'en-verde) (equal cambiarA 'rojo))  (list colorActual (format nil "Cambiar-a-~a" cambiarA)))
		(t (list colorActual 'accion-por-defecto))
		)
	)

;; ========================================================
;; FUNCIÓN: semaforo-timer
;; NATURALEZA: Pura (no posee efectos secundarios, devuelve el valor esperado de la funcion auxiliar a la que llama)
;; ESTRATEGIA: Es una funciones que prepara las variables para ser evaluadas en la funcion que se llama dentro de esta 
;; misma, la procedemos a clasificar como una funcion iniciadora/preparadora, utiliza funciones de orden superior (reduce y mapcar) para lograr
;; adaptatividad, sin importar la duracion de cada color siempre logra ocupar la duracion total correspondiente para identificar que segundo correspone
;; a cada color.
;; IMPACTO: Es una funcion no destructiva.
;; ========================================================

(defun semaforo-timer (n)
	(let ((duraciones '((rojo 90) (amarillo 6) (verde 120))))
				(timerAux (mod n (reduce '+ (mapcar 'cadr duraciones))) duraciones)
	)
)

;; ========================================================
;; FUNCIÓN: semaforo-timer-aux
;; NATURALEZA: Pura (Devuelve los mismos valores esperados segun las condiciones que se analizan en n
;; junto con el parametro lista que se le envia)
;; ESTRATEGIA: Recursividad de cola (la llamada recursiva es la ultima operacion) - recorre la lista de duraciones
;; y compara el entero "n" con la duracion del color actual; si n < duracion, retorna ese color.
;; Si no, descuenta la duracion y se llama a si misma con el resto de la lista (tail call).
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
;; FUNCIÓN: contar-Color
;; NATURALEZA: Pura (devuelve un valor numérico sin efectos secundarios).
;; ESTRATEGIA: Recursividad de cola con acumulador — recorre cada segundo del
;; intervalo (segundo, fin) incrementa el acumulador cuando semaforo-timer
;; devuelve el color. La llamada recursiva es siempre la última
;; operación en cada rama, garantizando tail call.
;; IMPACTO: No destructivo.
;; ========================================================

(defun contar-color(color segundo fin acum)

	(cond
		((>= segundo fin) acum)
		((equal (semaforo-timer segundo) color))
		(contar-Color color (+ segundo 1) fin (+ acum 1))
		(t (contar-Color color (+ segundo 1) fin acum))
	)
)

;; ========================================================
;; FUNCIÓN: distribucion-Temporal
;; NATURALEZA: Pura — calcula porcentajes basándose únicamente en funciones
;; puras auxiliares (semaforo-timer vía contar-Color). No produce efectos
;; secundarios ni depende de estado externo.
;; ESTRATEGIA: Utiliza funciones de orden superior (mapcar con lambda) para
;; calcular declarativamente el conteo de segundos de cada color dentro de
;; 1 hora (3600 segundos); luego un segundo mapcar transforma esos conteos
;; en porcentajes con precisión flotante.
;; IMPACTO: No destructivo.
;; ========================================================

(defun distribucion-Temporal ()
	(let* ((total-segundos 3600)
		   (colores '(rojo amarillo verde))
		   (conteos (mapcar (lambda (color)
								(contar-color color 0 total-segundos 0))
							colores)))
		(mapcar (lambda (color conteo)
					(list color (float (* (/ conteo total-segundos) 100))))
				colores conteos))
)


;;casos de prueba

;;funcionamiento normal
;;-> (transicion 'en-rojo 'amarillo)

#|(EN-ROJO "Cambiar-a-AMARILLO")|#

;;caminos alternativos (si los hubiere) 
;;-> (transicion 'en-rojo 'verde)

#|(EN-ROJO ACCION-POR-DEFECTO)|#

;;errores
;;-> (transicion "en-rojo" 'verde)

#|("en-rojo" ACCION-POR-DEFECTO)|#

;;casos de prueba

;;funcionamiento normal
;;-> (semaforo-timer  90)

#|AMARILLO|#

;;caminos alternativos (si los hubiere) 
;;-> (semaforo-timer  444)

#|ROJO|#

;;errores
;;-> (semaforo-timer  "54")


#|MOD: "54" is not a real number|#

	;; casos de prueba requerimiento 3
	#|
* ; [Normal] Registra el cambio con timestamp real del sistema
(registrarCambiosEstado 'rojo 'amarillo)
Tiempo 3990373261: la luz ha cambiado de ROJO a AMARILLO
NIL
* (registrarCambiosEstado 'amarillo 'verde)
Tiempo 3990373261: la luz ha cambiado de AMARILLO a VERDE
NIL
* (registrarCambiosEstado 'verde 'rojo)
Tiempo 3990373263: la luz ha cambiado de VERDE a ROJO
NIL
* ; [Alternativo] Acepta cualquier simbolo, no valida colores
(registrarCambiosEstado 'apagado 'rojo)
Tiempo 3990373316: la luz ha cambiado de APAGADO a ROJO
NIL
* ; [Error] Sin argumentos
(registrarCambiosEstado)

debugger invoked on a SB-INT:SIMPLE-PROGRAM-ERROR @1000B5AF43 in thread
#<THREAD tid=9888 "main thread" RUNNING {1100C38003}>:
  invalid number of arguments: 0

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [REPLACE-FUNCTION] Call a different function with the same arguments
  1: [CALL-FORM       ] Call a different form
  2: [ABORT           ] Exit debugger, returning to top level.

(REGISTRARCAMBIOSESTADO) [external]
   source: (DEFUN REGISTRARCAMBIOSESTADO (COLORANTERIOR SIGUIENTECOLOR)
             (LET ((EPOCH (GET-UNIVERSAL-TIME)))
               (FORMAT T "Tiempo ~a: la luz ha cambiado de ~a a ~a" EPOCH
                       COLORANTERIOR SIGUIENTECOLOR)))
0]
|#


;; casos de prueba requerimiento 4

#|
;;
(duracion-Ciclo 0)
216
* ;; Esperado: 216
(duracion-Ciclo 100)
216
* ;; [Normal]
(duracion-Ciclo 0)
216
* (duracion-Ciclo 100)
216
*

0] ;; [Error] (Intentar calcular con un símbolo en lugar de número)
(duracion-Ciclo 'rojo)


debugger invoked on a TYPE-ERROR @10002743C2 in thread
#<THREAD tid=6456 "main thread" RUNNING {1100C38003}>:
  The value
    ROJO
  is not of type
    REAL

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [ABORT] Reduce debugger level (to debug level 1).
  1:         Exit debugger, returning to top level.

(FLOOR ROJO 216)
0[2]
|#



	;; Casos de prueba Requerimiento 5
	 
	;funcionamiento normal
	;(ciclos-Por-Minuto 5)
  ; 1 ;
	; 84

	;caminos alternativos
	; (ciclos-Por-Minuto 5.2)
	; 1 ;
	; 95.99999

	;errores
	;->(ciclos-Por-Minuto -5)
	; -2 ;
	; 132

	;; Casos de prueba Requerimiento 4
	#|
;;
(duracion-Ciclo 0)
216
* ;; Esperado: 216
(duracion-Ciclo 100)
216
* ;; [Normal]
(duracion-Ciclo 0)
216
* (duracion-Ciclo 100)
216
*

0] ;; [Error] (Intentar calcular con un símbolo en lugar de número)
(duracion-Ciclo 'rojo)


debugger invoked on a TYPE-ERROR @10002743C2 in thread
#<THREAD tid=6456 "main thread" RUNNING {1100C38003}>:
  The value
    ROJO
  is not of type
    REAL

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [ABORT] Reduce debugger level (to debug level 1).
  1:         Exit debugger, returning to top level.

(FLOOR ROJO 216)
0[2]
|#