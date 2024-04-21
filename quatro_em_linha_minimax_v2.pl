%next_player(Player1, Player2) - permite saber qual é o próximo jogador
next_player(1,2).
next_player(2,1).

%play_game/0
% Este predicado começa por criar um tabuleiro com a dimensao 6x6 e
% validar a jogada do computador (player 1).
play_game():-
    initial_board(6, 6, B0),
    play(2,B0).

initial_board(NumRows, NumColumns, Board) :-
    length(Board, NumRows),
    length(Row, NumColumns),
    maplist(=(0),Row),
    maplist(=(Row), Board).

position(Board, X, Y, Value) :-
    nth0(X, Board, Row),
    nth0(Y, Row, Value).
valid_move(X,Y,Board):-
    position(Board,X,Y,0).

game_over(Board,Winner):-
     append(_, [C|_],Board),
     append(_,[Winner,Winner,Winner,Winner|_],C),
     Winner \= 0.

game_over(Board,Winner):-
    \+ valid_move(_,_,Board),
    Winner = 0,
    !.

game_over(Board,Winner):-
     append(_,[C1,C2,C3,C4|_],Board),
     append(I1,[Winner|_],C1),
     append(I2,[Winner|_],C2),
     append(I3,[Winner|_],C3),
     append(I4,[Winner|_],C4),
     length(I1,M), length(I2,M), length(I3,M), length(I4,M),
     Winner \= 0.
game_over(Board,Winner):-
     append(_,[C1,C2,C3,C4|_],Board),
     append(I1,[Winner|_],C1),
     append(I2,[Winner|_],C2),
     append(I3,[Winner|_],C3),
     append(I4,[Winner|_],C4),
     length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
     M2 is M1+1, M3 is M2+1, M4 is M3+1,
     Winner \= 0.

game_over(Board,Winner):-
     append(_,[C1,C2,C3,C4|_],Board),
     append(I1,[Winner|_],C1),
     append(I2,[Winner|_],C2),
     append(I3,[Winner|_],C3),
     append(I4,[Winner|_],C4),
     length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
     M2 is M1-1, M3 is M2-1, M4 is M3-1,
     Winner \= 0.

calculate_board_value(Winner, Score):-
     (   Winner = 2 ->
         Score = -1;
         Score = Winner   ).

replace(X,Y,Z,B) :-
    nth0(X,Y,_,T),
    nth0(X,B,Z,T).

add_move(Player,X,Y,InitialBoard,FinalBoard):-
    nth0(X,InitialBoard,V1),
    replace(Y,V1,Player,V2),
    replace(X,InitialBoard,V2,FinalBoard).

generate_move(Player,[X|InitialBoard],[Y|FinalBoard]):-
    valid_move(V1,V2,[X|InitialBoard]),
    add_move(Player,V1,V2,[X|InitialBoard],[Y|FinalBoard]).

print_board([]).
print_board([H|Board]):- write(H), nl, print_board(Board).

%play/1
%O predicado play tem como argumentos o jogador que deve jogar e o tabuleiro sobre o qual deve fazer a sua jogada.
% Este predicado e' recursivo de modo a permitir a alternancia das
% jogadas. A sua implementaçao e constituida pela jogada do computador
% (play(1,B)) , pela jogada do jogador (play(2,B)) e pela verificaçao
% do fim do jogo.
play(_,B0):-
    game_over(B0,T),
    calculate_board_value(T,Value),
    print_board(B0),
    write('Game Over!'),
    nl,

    display(Value),
    !.

play(1,B0):-
    write('Player:'),nl,
    print_board(B0),
    alphabeta(B0,6,-100,100,B1,_,1),
    !,
    play(2,B1).

play(2,B0):-
    write('Computer:'),nl,
    print_board(B0),
    write('Where to play? (C,L)'),
    read(C),
    read(L),
    valid_move(C,L,B0),
    add_move(2,C,L,B0,B1),
    !,
    play(1,B1).

%alphabeta/7
%minimax-alpha-beta
% O predicado que implementa o minimax e' chamado alfabeta e tem como
% argumentos o tabuleiro, o valor de profundidade que ainda e' permitido
% explorar, o alfa, o beta, o tabuleiro resultado, o score da avaliaçao
% do resultado na o'tica do computador e o jogador que se esta' a
% avaliar (minimizar ou a maximizar).

alphabeta(Bi, 0, _, _, Bi, Value, P):-
    calculate_board_heuristic(P,Bi,Value),
    !.

alphabeta(Bi, _, _, _, Bi, Value, _):-
    game_over(Bi,T),
    calculate_board_value(T,Value),
    !.


alphabeta(Bi, D, Alfa, Beta, Bf, Value, Player):-
    next_player(Player,Other),
    possible_moves(Player,Bi,L),
    !,
    evaluate_child(Other, L, D, Alfa, Beta, Bf, Value).
%Funcao nao completa
calculate_board_heuristic(P,B1,Value):-
    generate_move(P,B1,B2),
    (  \+ game_over(B2,P) ->
        Value = 2;
        Value = 0      ).

%evaluate_child/7
evaluate_child(Player, [B], D, Alfa, Beta, B, Value):-
    D1 is D-1,
    !,
    alphabeta(B, D1, Alfa, Beta, _, Value, Player).


evaluate_child(2, [Bi|T], D, Alfa, Beta,Bf, Value):-
    D1 is D-1,
    alphabeta(Bi, D1, Alfa, Beta, _, Value1, 2),
    !,
    evaluate_next_child_max(Value1,Bi, T, D, Alfa, Beta, Value, Bf).

evaluate_child(1, [Bi|T], D, Alfa, Beta,Bf, Value):-
    D1 is D-1,
    alphabeta(Bi, D1, Alfa, Beta, _, Value1, 1),
    !,
    evaluate_next_child_min(Value1,Bi, T, D, Alfa, Beta, Value, Bf).

%evaluate_next_child_max/8
evaluate_next_child_max(Value1,Bi, T, D, Alfa, Beta, Value, Bf):-
    Value1 < Beta,
    max(Value1,Alfa,NewAlfa),
    !,
    evaluate_child(2, T, D, NewAlfa, Beta, B2, Value2),
    max_board(Value1,Bi,Value2,B2,Value,Bf).

evaluate_next_child_max(Value1,Bi, _, _, _, _, Value1, Bi):- !.

%evaluate_next_child_min/8
evaluate_next_child_min(Value1,Bi, T, D, Alfa, Beta, Value, Bf):-
     Value1 > Alfa,
     min(Value1,Beta,NewBeta),
     !,
     evaluate_child(1, T, D, Alfa, NewBeta, B2, Value2),
     min_board(Value1,Bi,Value2,B2,Value,Bf).

evaluate_next_child_min(Value1,Bi, _, _, _, _, Value1, Bi):- !.


%possible_moves/3
possible_moves(X,B,L):-
    bagof(BP,generate_move(X,B,BP),L).

%max_board/6
max_board(Value1,B1,Value2,_,Value1,B1):-
    Value1 >= Value2.

max_board(Value1,_,Value2,B2,Value2,B2):-
    Value1 < Value2.

%min_board/6
min_board(Value1,B1,Value2,_,Value1,B1):-
    Value1 =< Value2.

min_board(Value1,_,Value2,B2,Value2,B2):-
    Value1 > Value2.

%max/3
max(X,Y,X):-
    X>=Y,!.
max(X,Y,Y):-
    Y>X.
%min/3
min(X,Y,X):-
    X=<Y,!.
min(X,Y,Y):-
    Y<X.
