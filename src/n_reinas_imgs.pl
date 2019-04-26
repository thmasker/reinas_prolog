:- use_module(library(pce)).
:- dynamic
    computed_image/4.
:- pce_global(@square_colour, new(get_method(colour, name, new(vector),
                                            @receiver?image?colour))).
:- pce_global(@square_piece, new(get_method(piece, name, new(vector),
                                           @receiver?image?piece))).


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


/* CARGA DE IMÁGENES */

%       square_image(+PieceName, +PieceColour, +SquareColour, -Image)
square_image(Piece, PieceColour, SquareColour, Image) :-
    computed_image(Piece, PieceColour, SquareColour, Image),
    !.
square_image(Piece, PieceColour, SquareColour, Image) :-
    image_name(Piece, ImageName),
    new(TotalImage, image(ImageName)),
    sub_area(PieceColour, SquareColour, Area),
    !,
    get(TotalImage, clip, Area, Image),
    send(Image, lock_object, @on),
    send(Image, attribute, attribute(piece, Piece)),
    send(Image, attribute, attribute(colour, PieceColour)),
    asserta(computed_image(Piece, PieceColour, SquareColour, Image)).

%       image_name(?PieceName, ?ChessProgram Id, ?ImageName)
image_name(empty, 'chesssquare.bm').
image_name(queen, 'queen.bm').

%       sub_area(+PieceColour, +SquareColour, -AreaTerm)
sub_area(white, white, area(32,  0, 32, 32)).
sub_area(white, black, area(0,   0, 32, 32)).
sub_area(black, white, area(32, 32, 32, 32)).
sub_area(black, black, area(0,  32, 32, 32)).

/* DIBUJO DE TABLERO */

make_chess_board(Board, N) :-
    new(Board, device),
    (   between(1, N, X),
        between(1, N, Y),
            GX is (X-1) * 32,
            GY is (N-Y) * 32,
            square_colour(X, Y, Colour),
            square_image(empty, _, Colour, Image),
            send(Board, display,
                 new(Bitmap, bitmap(Image)),
                 point(GX, GY)),
            send(Bitmap, attribute, attribute(square_colour, Colour)),
            send(Bitmap, get_method, @square_piece),
            send(Bitmap, get_method, @square_colour),

            name(CellName, [X,Y]),
            send(Bitmap, name, CellName),
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
    /* Obtenemos la casilla en la que poner la reina */
    name(CellName, [X,Y]),
    get(Board, member, CellName, Bitmap),

    get(Bitmap, square_colour, SquareColour),
    square_image(queen, black, SquareColour, Image),
    send(Bitmap, image, Image).


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
