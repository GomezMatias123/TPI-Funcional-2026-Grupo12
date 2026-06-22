;;CLIPS-> (load "C:\\Lisp\\TPI-Funcional-2026-Grupo12\\TPI-Funcional-2026-Grupo12\\Lisp\\core")
;;SBCL-> (load "C:/Lisp/TPI-Funcional-2026-Grupo12/TPI-Funcional-2026-Grupo12/Lisp/core")

;; ========================================================
;; Carga de Quicklisp y dependencias externas
;; ========================================================
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :local-time)





;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura (no posee efectos secundarios, solo crea la lista segun el estado actual y la accion siguiente del ciclo del semaforo)
;; ESTRATEGIA: No posee una una estrategia especfica, se trata de una funcion simple que selecciona un resultado en base a la evaluacion 
;; de funciones predicados (equal/and)
;; IMPACTO: Es una funcion no destructiva
;; ========================================================

(defun transicion (color-actual cambiarA)
  (if (or (and (eql color-actual 'en-rojo) (eql cambiarA 'rojo-intermitente))
          (and (eql color-actual 'en-rojo-intermitente) (eql cambiarA 'amarillo))
          (and (eql color-actual 'en-verde) (eql cambiarA 'verde-intermitente))
          (and (eql color-actual 'en-verde-intermitente) (eql cambiarA 'rojo)))
          (and (eql color-actual 'en-amarillo) (eql cambiarA 'amarillo-intermitente))
          (and (eql color-actual 'en-amarillo-intermitente) (eql cambiarA 'verde))
      (list color-actual (format nil "Cambiar-a-~a" cambiarA))
      (list color-actual 'accion-por-defecto)))

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
	(let ((duraciones '((rojo 90) (rojo-intermitente 3) (verde 120) (verde-intermitente 3) (amarillo 6) (amarillo-intermitente 3))))
				(timer-aux (mod n (reduce '+ (mapcar 'cadr duraciones))) duraciones)))

;; ========================================================
;; FUNCIÓN: timer-aux
;; NATURALEZA: Pura (Devuelve los mismos valores esperados segun las condiciones que se analizan en n
;; junto con el parametro lista que se le envia)
;; ESTRATEGIA: Recursividad de cola (la llamada recursiva es la ultima operacion) - recorre la lista de duraciones
;; y compara el entero "n" con la duracion del color actual; si n < duracion, retorna ese color.
;; Si no, descuenta la duracion y se llama a si misma con el resto de la lista (tail call).
;; IMPACTO: Es una funcion no destructiva.
;; ========================================================


(defun timer-aux (n duraciones)
		(cond 	((< n (cadar duraciones)) (caar duraciones))
				(t (timer-aux (- n (cadar duraciones)) (cdr duraciones)))))



;; ========================================================
;; FUNCIÓN: registrar-cambios-estado 
;; NATURALEZA: Impura (posee efectos secundarios: imprime el cambio de estado en la terminal y consulta la hora del sistema mediante 
;; local-time:now de la libreria quicklisp
;; ESTRATEGIA: No conlleva ninguna estrategia, es una funcion simple que consulta el tiempo actual trancurrido hasta hoy desde los origenes de Lisp.
;; IMPACTO: Es una funcion no destructiva, brinda informacion pero no modifica ningun tipo de dato
;; ========================================================

(defun registrar-cambios-estado (colorAnterior siguienteColor)
	(format t "Tiempo [~a]: la luz ha cambiado de ~a a ~a~%"
          (local-time:format-timestring 
            nil 
            (local-time:timestamp- (local-time:now) 3 :hour)
            :format '((:year 4) "-" (:month 2) "-" (:day 2) " "
                      (:hour 2) ":" (:min 2) ":" (:sec 2)))
          colorAnterior siguienteColor))

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura realiza calculos y devuelve un valor numerico que representa el total de segundos de 1 ciclo sin importar el segundo que se le brinde.
;; Se podria pasar como parametero segundos mayores a 225 que corresponden a un segundo ciclo segun la duraciones del requerimento 2 e igualmente el calculo
;; seguiria dando la duracion de 1 ciclo
;; ESTRATEGIA: Se trata de una estragia que permite iniciar los valores para ser usados en otras funciones, se utiliza un let para definir el fin del ciclo correspondiente al segundo
;; del tiempo del ciclo que pasamos por parametro, luego se vuelve a llamar a la funcion auxiliar buscar-Fin-Ciclo para obtener el final del ciclo del siguiente ciclo para 
;; hacer la diferencia con finCiclo, como siempre es la diferencia de un ciclo siguiente con el anterior, sin importar en cual numero n ciclo nos encontremos, obtenemos el
;; valor correspondiente a 1 ciclo.
;; IMPACTO: Es no destructivo
;; ========================================================


(defun duracion-ciclo(segundos)
	(let ((finCiclo (buscar-Fin-Ciclo segundos)))
			(- (buscar-Fin-Ciclo (+ finCiclo 1)) finCiclo)))

;; ========================================================
;; FUNCIÓN: buscar-Fin-Ciclo
;; NATURALEZA: Pura devuelve un valor numerico basado en la logica de la funcion timer, sirve para buscar el segundo donde termina un ciclo.
;; ESTRATEGIA: Recursividad de cola (la llamada recursiva (buscar-Fin-Ciclo (+ segundos 1)) es la última operación en la rama. Avanza segundo 
;; a segundo hasta detectar la transición final de 1 ciclo
;; IMPACTO: Es no destructiva
;; ========================================================


(defun buscar-Fin-Ciclo(segundos)
	(cond
		((and (equal (timer segundos) 'verde-intermitente) (equal (timer (+ segundos 1)) 'rojo)) 
			(+ segundos 1))
		(t (buscar-Fin-Ciclo (+ segundos 1)))))
	

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
		(t "Ciclo dentro del rango óptimo (35-150 segundos).")))


;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura, devuelve un valor numérico calculado a partir de operaciones aritméticas.
;; ESTRATEGIA: convierte los minutos recibidos a segundos multiplicando por 60, para luego realizar una división
;; mediante floor entre los segundos totales y la duración del ciclo obtenida en segundos por la funcion duracion-ciclo.
;; obteniendo al final la cantidad de ciclos completos en el minuto pasado por parametro.
;; IMPACTO: No destructivo.
;; ========================================================

(defun ciclos-por-tiempo (minutos)
	(floor (* minutos 60) (duracion-ciclo 0)))

;; ========================================================
;; FUNCIÓN: contar-porcentaje-color
;; NATURALEZA: Pura (devuelve un valor numérico sin efectos secundarios).
;; ESTRATEGIA: Recursividad de cola con acumulador — recorre cada segundo del
;; intervalo (segundo, fin) incrementa el acumulador cuando timer
;; devuelve el color. La llamada recursiva es siempre la última
;; operación en cada rama, garantizando tail call.
;; IMPACTO: No destructivo.
;; ========================================================

(defun contar-porcentaje-color (color segundo fin acum)
  (cond
    ((>= segundo fin) acum)
    ((equal (timer segundo) color) 
     (contar-porcentaje-color color (+ segundo 1) fin (+ acum 1)))
    (t (contar-porcentaje-color color (+ segundo 1) fin acum))))

;; ========================================================
;; FUNCIÓN: distribucion-Temporal
;; NATURALEZA: Pura — calcula porcentajes basándose únicamente en funciones
;; puras auxiliares (timer vía contar-porcentaje-color). No produce efectos
;; secundarios ni depende de estado externo.
;; ESTRATEGIA: Utiliza funciones de orden superior (mapcar con lambda) para
;; calcular declarativamente el conteo de segundos de cada color dentro de
;; 1 hora (3600 segundos); luego un segundo mapcar transforma esos conteos
;; en porcentajes con precisión flotante.
;; IMPACTO: No destructivo.
;; ========================================================

(defun distribucion-Temporal ()
	(let* ((total-segundos 3600)
		   (colores '(rojo rojo-intermitente amarillo amarillo-intermitente verde verde-intermitente))
		   (conteos (mapcar (lambda (color)
								(contar-porcentaje-color color 0 total-segundos 0))
							colores)))
		(mapcar (lambda (color conteo)
					(list color (float (* (/ conteo total-segundos) 100))))
				colores conteos)))

;; ========================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura (Escribe en un archivo de texto plano, modificando el estado externo).
;; ESTRATEGIA: Función de orden superior — utiliza mapc con una lambda para 
;; recorrer la lista de datos de transiciones y coloca la informacion dentro
;; de un texto de manera iterada.
;; IMPACTO: No destructiva
;; ========================================================

(defun informe (datos)
  (with-open-file (stream "informe-ejecucion-semaforo.txt" 
                          :direction :output
                          :if-exists :supersede)
    (format stream "Informe de Ejecución del Sistema Semafórico~%")
    (format stream "=========================================~%")
    (mapc (lambda (entrada)
            (format stream "~a - Transición: ~a → ~a~%"
                    (first entrada) (second entrada) (third entrada)))
          datos)
    (format stream "~% --- Fin del Informe ---")))


;;casos de prueba

;;funcionamiento normal
;;-> (transicion 'en-rojo 'rojo-intermitente)

#|(EN-ROJO "Cambiar-a-ROJO-INTERMITENTE")|#

;;caminos alternativos (si los hubiere) 
;;-> (transicion 'en-rojo 'verde)

#|(EN-ROJO ACCION-POR-DEFECTO)|#

;;errores
;;-> (transicion "en-rojo" 'verde)

#|("en-rojo" ACCION-POR-DEFECTO)|#

;;casos de prueba requerimiento 2

;;funcionamiento normal
;;-> (timer  90)

#|ROJO-INTERMITENTE|#

;;caminos alternativos (si los hubiere) 
;;-> (timer  444)

#|VERDE|#

;;errores
;;-> (timer  "54")


#|MOD: "54" is not a real number|#

	;; casos de prueba requerimiento 3
	#|
* ; [Normal] Registra el cambio con timestamp real del sistema
(registrar-cambios-estado 'rojo 'amarillo)
Tiempo 3990373261: la luz ha cambiado de ROJO a AMARILLO
NIL
* (registrar-cambios-estado 'amarillo 'verde)
Tiempo 3990373261: la luz ha cambiado de AMARILLO a VERDE
NIL
* (registrar-cambios-estado 'verde 'rojo)
Tiempo 3990373263: la luz ha cambiado de VERDE a ROJO
NIL
* ; [Alternativo] Acepta cualquier simbolo, no valida colores
(registrar-cambios-estado 'apagado 'rojo)
Tiempo 3990373316: la luz ha cambiado de APAGADO a ROJO
NIL
* ; [Error] Sin argumentos
(registrar-cambios-estado)

debugger invoked on a SB-INT:SIMPLE-PROGRAM-ERROR @1000B5AF43 in thread
#<THREAD tid=9888 "main thread" RUNNING {1100C38003}>:
  invalid number of arguments: 0

Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

restarts (invokable by number or by possibly-abbreviated name):
  0: [REPLACE-FUNCTION] Call a different function with the same arguments
  1: [CALL-FORM       ] Call a different form
  2: [ABORT           ] Exit debugger, returning to top level.

(registrar-cambios-estado) [external]
   source: (DEFUN registrar-cambios-estado (COLORANTERIOR SIGUIENTECOLOR)
             (LET ((EPOCH (GET-UNIVERSAL-TIME)))
               (FORMAT T "Tiempo ~a: la luz ha cambiado de ~a a ~a" EPOCH
                       COLORANTERIOR SIGUIENTECOLOR)))
0]
|#


;; casos de prueba requerimiento 4

#|
;;
(duracion-ciclo 0)
225
* ;; Esperado: 225
(duracion-ciclo 100)
225

;; [Error] (Intentar calcular con un símbolo en lugar de número)
(duracion-ciclo 'rojo)


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
	;(ciclos-por-tiempo 5)
  ; 1 ;
	; 75

	;caminos alternativos
	; (ciclos-por-tiempo 5.2)
	; 1 ;
	; 87.0

	;errores
	;->(ciclos-por-tiempo -5)
	; -2 ;
	; 150

	;; Casos de prueba Requerimiento 6

	; funcionamiento normal
	; (distribucion-temporal)
	; ((rojo 40.0) (rojo-intermitente 1.33) (amarillo 2.67) 
	;(amarillo-intermitente 1.33) (verde 53.33) (verde-intermitente 1.33)) 

	;errores
	; (distribucion-temporal)
	; ((rojo 33.333332) (amarillo 33.333332) (verde 33.333332))

;; Casos de prueba Requerimiento 7 (Extensión 2)
#|
; [Normal] Genera el archivo con 3 transiciones
(informe '(("2024-06-04 14:30:15" rojo amarillo)
           ("2024-06-04 14:30:21" amarillo verde)
           ("2024-06-04 14:30:30" verde rojo)))
NIL
; El archivo "informe-ejecucion-semaforo.txt" contiene:
; Informe de Ejecución del Sistema Semafórico
; =========================================
; 2024-06-04 14:30:15 - Transición: ROJO → AMARILLO
; 2024-06-04 14:30:21 - Transición: AMARILLO → VERDE
; 2024-06-04 14:30:30 - Transición: VERDE → ROJO
;
;  --- Fin del Informe ---

; [Alternativo] Lista vacía → solo encabezado y cierre
(informe '())
NIL

; [Error] Datos mal formados (elemento sin los 3 campos)
(informe '((rojo amarillo)))
; → ERROR: Tira error al intentar (third entrada) sobre lista de 2 elementos
|#