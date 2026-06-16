-module(solucion).
   -export([transicion/2,semaforo_timer/1]).
 

%% ========================================================
%% FUNCIÓN: transicion
%% NATURALEZA: Pura (la salida queda totalmente determinada por las 
%% dos entradas; no produce efectos secundarios).
%% ESTRATEGIA: Se selecciona una salida en base a los valores de los parametros, existe una salida para caso de entrada de parametros permitida
%% IMPACTO: No destructiva.
%% ========================================================  
transicion (ColorActual, CambiarA) ->
  case {ColorActual, CambiarA} of

         {en_rojo, rojo_intermitente} ->
             {ColorActual, "Cambiar-a-rojo-intermitente"};
             
         {en_rojo_intermitente, verde} ->
             {ColorActual, "Cambiar-a-verde"};

         {en_verde, verde_intermitente} ->
             {ColorActual, "Cambiar-a-verde-intermitente"};

         {en_verde_intermitente, amarillo} ->
             {ColorActual, "Cambiar-a-amarillo"};

         {en_amarillo, amarillo_intermitente} ->
             {ColorActual, "Cambiar-a-amarillo-intermitente"};

        {en_amarillo_intermitente, rojo} ->
             {ColorActual, "Cambiar-a-rojo"};

          _ ->
             {ColorActual, accion_por_defecto}
     end.


%% ========================================================
%% FUNCIÓN: semaforo_timer/1
%% NATURALEZA: Pura (Sin efectos secundarios, retorna el color correspondiente)
%% ESTRATEGIA: Iniciadora con funciones de orden superior (lists:map, lists:foldl)
%% IMPACTO: No destructiva
%% ========================================================
semaforo_timer(N) ->
    Duraciones = [
        {rojo, 90}, 
        {rojo_intermitente, 3}, 
        {amarillo, 6}, 
        {amarillo_intermitente, 3}, 
        {verde, 120}, 
        {verdeintermitente, 3}
    ],
    Tiempos = lists:map(fun({_, Tiempo}) -> Tiempo end, Duraciones),
    TotalCiclo = lists:foldl(fun(X, Sum) -> X + Sum end, 0, Tiempos),
    semaforo_timer_aux(N rem TotalCiclo, Duraciones).

%% ========================================================
%% FUNCIÓN: semaforo_timer_aux/2
%% NATURALEZA: Pura 
%% ESTRATEGIA: Recursividad de cola (Tail recursion) sobre listas
%% IMPACTO: No destructiva
%% ========================================================
semaforo_timer_aux(N, [{Color, Duracion} | Resto]) ->
    if
        N < Duracion -> 
            Color;
        true -> 
            semaforo_timer_aux(N - Duracion, Resto)
    end.