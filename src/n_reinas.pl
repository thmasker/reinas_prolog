:- use_module(library(pce)).


/**************************/
/**** INTERFAZ GRÁFICA ****/
/**************************/

/* VENTANA DE CONTROL */

n_reinas :-
        new(Dialog, dialog('Problema de N Reinas')),
        free(@txtStep),
        send_list(Dialog, append,
                  [ new(TxtReinas, int_item(número_de_reinas, 4, low := 4, high := 100)),
                    new(@txtStep, int_item(solución, 1, low:=1, high:=10000)),
                    button(calcular_solución,
                           message(@prolog, resolver,
                                   TxtReinas?selection, @txtStep?selection)),
                    button(siguiente_solución,
                           message(@prolog, siguienteSolucion,
                                   TxtReinas?selection, @txtStep?selection))
                  ]),
        send(Dialog, open),
        resolver(4,1).

siguienteSolucion(NReinas, SolutionIndex) :-
        NextIndex is SolutionIndex + 1,
        send(@txtStep, selection(NextIndex)),
        resolver(NReinas, NextIndex).

/* VENTANA DE RESULTADOS */

resolver(NReinas, SolutionIndex) :-
        /* Comprobamos si existe la ventana para los resultados */
        (   not(object(@resultWindow)) ->
            /* ResultWindow no existe, la creamos y la abrimos */
            new(@resultWindow, window('Solución', size(NReinas*32, NReinas*32))),
            send(@resultWindow, open);

            /* ResultWindow existe, reajustamos las dimensiones por si NReinas ha cambiado */
            send(@resultWindow, width(NReinas*32)),
            send(@resultWindow, height(NReinas*32))
        ),
        /* Limpiamos el tablero*/
        free(@cb),
        make_chess_board(@cb, NReinas),
        send(@resultWindow, display, @cb),
        /* Buscamos la solución número SolutionIndex */
        findall(Sol, vector(Sol, NReinas), Solutions),
        length(Solutions, AllSolutionsCount),
        resolver(NReinas, SolutionIndex, Solutions, AllSolutionsCount).

%  resolver(NReinas, SolutionIndex, Solutions, NSolutions)
%         NReinas: Número de reinas (tamaño del tablero) a resolver
%         SolutionIndex: Solución que queremos mostrar
%         Solutions: Lista de soluciones que quedan
%         NSolutions: Número total de soluciones posibles
resolver(_, _, [], _) :- mensaje_error.
resolver(NReinas, SolutionIndex, [Sol|Sr], NSolutions) :-
        length(Sr, NSolsRestantes),
        Index is NSolutions - NSolsRestantes,
        (   Index = SolutionIndex ->
            showSolution(NReinas, Sol);
            resolver(NReinas, SolutionIndex, Sr, NSolutions)
        ).


/* VENTANA DE ERROR: Índice de solución mayor que número de soluciones */

mensaje_error :-
        new(Dialog, dialog('Solución no encontrada')),
        send_list(Dialog, append, [
                   label(error_label, "El índice de solución es superior a la cantidad de soluciones encontradas.")
                ]),
        send(Dialog, open).


/* DIBUJO DE TABLERO */

make_chess_board(Board, N) :-
    new(Board, device),
    (   between(1, N, X),
        between(1, N, Y),
            GX is (X-1) * 32,
            GY is (N-Y) * 32,
            send(Board, display, new(Cell, box(32,32)), point(GX,GY)),
            /* Definimos el color de relleno de esta casilla dependiendo de su posición */
            square_colour(X, Y, Colour),
            send(Cell, fill_pattern, colour(Colour)),
        fail ; true
    ).

%square_colour: Colour es el color correspondiente a la casilla (X,Y)
square_colour(X, Y, Colour) :-
    (X+Y) mod 2 =:= 0 ->
        Colour = black;
        Colour = white.

/* DIBUJO DE LA SOLUCIÓN */

showSolution(NReinas, Sol) :-
        free(@cb),
        make_chess_board(@cb, NReinas),
        send(@resultWindow, display, @cb),
        pinta(Sol).

pinta([c(X, Y)|R]) :-
        put(@cb, [X,Y]),
        pinta(R).

put(Board, [X,Y]) :-
    GX is (X-1)*32+3,
    GY is (Y-1)*32+3,
    send(Board, display, new(Circle, circle(26)), point(GX,GY)),
    send(Circle, fill_pattern, colour(gray)).



/************************/
/**** LÓGICA INTERNA ****/
/************************/

/* CÁLCULO DE SOLUCIONES AL PROBLEMA DE LAS N-REINAS */

% vector(Sol, N), Sol es una lista con las posiciones de las N reinas
vector(Sol, N) :-
        creaListaC(N, L),
        generar(L, N),
        reverse(L, Sol).

% generar(L, N), L es una lista con N posiciones desde las que las
% reinas no se atacan
generar([], _).
generar([c(X, Y) | Resto], N) :-
        generar(Resto, N),
        creaLista(N, L),
        member(Y, L),
        not(ataca(c(X, Y), Resto)).

% ataca(c(X, Y), L), la reina en c(X, Y) ataca a alguna de las situadas
% en las posiciones de L
ataca(c(_, Y), [c(_, Y1) | _]) :-
        Y =:= Y1, !.
ataca(c(X, Y), [c(X1, Y1) | _]) :-
        Y1 is X1 + (Y - X), !.
ataca(c(X, Y), [c(X1, Y1) | _]) :-
        Y1 is (Y + X) - X1, !.
ataca(c(X, Y), [c(_, _) | Resto]) :-
        ataca(c(X, Y), Resto).

% creaLista(N, L), la lista L tiene N elementos numerados de N a 1
creaLista(1, [1]).
creaLista(N, [N|Ns]) :-
        N > 1,
        N1 is N - 1,
        creaLista(N1, Ns).

% creaListaC(N, L), la lista L tiene N elementos con el siguiente
% formato: c(X, Y), que expresa la casilla donde se ubica una reina
% concreta
creaListaC(1, [c(1, _)]).
creaListaC(N, [c(N, _)| Ns]) :-
        N > 1,
        N1 is N - 1,
        creaListaC(N1, Ns).
