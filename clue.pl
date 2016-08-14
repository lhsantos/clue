% Author: Luciano Santos
% Date: 12/08/2016
% This script plays Clue (Hasbro, 2002). See doc.pdf for reference.


%%%%% Auxiliary predicates.

%% member(X, List) - X is in List
member(X, [X|_]).
member(X, [_|Tail]) :- member(X, Tail).

%% append(L1, L2, L3) - L3 is the result of appending L1 and L2.
append([], L, L).
append([H|T], L2, [H|L3]) :- append(T, L2, L3).

%% reverse(List, Reversed) - reverses List
reverse_aux([H|Tail], Stack, Reversed) :- reverse_aux(Tail, [H|Stack], Reversed).
reverse_aux([], Stack, Stack). 
reverse(List, Reversed) :- reverse_aux(List, [], Reversed).

%%%%% The static game data.

%%% The game map
% Origin of the map is lower left corner, growing to the right and
% upwards (duh). We are programmers, so we count from zero.

%% blocked((X, Y)) - point <X, Y> is inside a room or is a wall
% conservatory
blocked((X, Y)) :- Y =:= 0, X >= 0, X =< 8.
blocked((X, Y)) :- X >= 0, X =< 4, Y >= 1, Y =< 5.
blocked((X, Y)) :- X =:= 5, Y >= 1, Y =< 4.
blocked((6, 1)).
% ball room
blocked((X, Y)) :- X >= 10, X =< 13, Y >= 0, Y =< 1.
blocked((X, Y)) :- X >= 8, X =< 15, Y >= 2, Y =< 7.
% kitchen
blocked((X, Y)) :- Y =:= 0, X >= 15, X =< 23.
blocked((17, 1)).
blocked((X, Y)) :- X >= 18, X =< 23, Y >= 1, Y =< 6.
% dining room
blocked((X, Y)) :- X >= 16, X =< 18, Y >= 10, Y =< 15.
blocked((X, Y)) :- X >= 19, X =< 22, Y >= 9, Y =< 15.
blocked((X, Y)) :- X =:= 23, Y >= 8, Y =< 16.
% lounge
blocked((X, Y)) :- X >= 17, X =< 22, Y >= 19, Y =< 24.
blocked((X, Y)) :- X =:= 23, Y >= 18, Y =< 24.
% hall
blocked((X, Y)) :- X >= 9, X =< 14, Y >= 18, Y =< 23.
blocked((X, Y)) :- Y =:= 24, X >= 8, X =< 15.
% study
blocked((X, Y)) :- X =:= 0, Y >= 20, Y =< 24.
blocked((X, Y)) :- X >= 1, X =< 6, Y >= 21, Y =< 24.
% library
blocked((X, Y)) :- X >= 0, X =< 5, Y >= 14, Y =< 18.
blocked((X, Y)) :- X =:= 6, Y >= 15, Y =< 17.
% billiard room
blocked((X, Y)) :- X =:= 0, Y >= 7, Y =< 13.
blocked((X, Y)) :- X >= 1, X =< 5, Y >= 8, Y =< 12.
% center
blocked((X, Y)) :- X >= 9, X =< 13, Y >= 10, Y =< 16.

%% valid_room(R) - R is a valid room name
valid_room(R) :-
	member(
		R,
		[
			'conservatory',
			'ball room',
			'kitchen',
			'dining room',
			'lounge',
			'hall',
			'study',
			'library',
			'billiard room'
		]
	).

%% door((X, Y), R) - point <X, Y> is a door to room R
door((4, 5), 'conservatory').
door((8, 5), 'ball room').
door((15, 5), 'ball room').
door((9, 7), 'ball room').
door((14, 7), 'ball room').
door((19, 6), 'kitchen').
door((16, 12), 'dining room').
door((17, 15), 'dining room').
door((17, 19), 'lounge').
door((9, 20), 'hall').
door((11, 18), 'hall').
door((12, 18), 'hall').
door((6, 21), 'study').
door((3, 14), 'library').
door((6, 16), 'library').
door((1, 12), 'billiard room').
door((5, 9), 'billiard room').

%% passage(R1, R2) - there a passage from R1 to R2
passage_aux('conservatory', 'lounge').
passage_aux('kitchen', 'study').
passage(R1, R2) :- passage_aux(R1, R2) ; passage_aux(R2, R1).


%%% The game characters.

%% valid_char(C) - is C a valid character name?
valid_char(C) :-
	member(
		C,
		[
			'peacock',
			'plum',
			'green',
			'white',
			'scarlet',
			'mustard'
		]
	).

%%% The game weapons.

%% valid_weapon(W) - is W a valid weapon name?
valid_weapon(W) :-
	member(
		W,
		[
			'rope',
			'pipe',
			'knife',
			'wrench',
			'candlestick',
			'pistol'
		]
	).



%%%%% Movement logic.

%%% The position of each character.

%% position((X, Y), C) - the current position of character C is <X, Y>
%% here, it's initialized to the start position at the board.
%% this predicate will be "rewritten" whenever the script's own character moves
%% or it receives information that another character moved.
position((0, 6), 'peacock').
position((0, 19), 'plum').
position((9, 0), 'green').
position((14, 0), 'white').
position((16, 24), 'scarlet').
position((23, 17), 'mustard').

%% is_free((X, Y)) - the position <X, Y> can bee occupied by a character.
is_free((X, Y)) :-
		X >= 0, X =< 23, Y >= 0, Y =< 23, % inside the boundaries of the board
		\+ position((X, Y), _), % not currently occupied by anyone
		\+ blocked((X, Y)). % not a room or a wall

%% neighbor((Xs, Ys), (Xn, Yn)) - <Xn, Yn> is neighbor of <Xs, Ys>.
neighbor((Xs, Ys), (Right, Ys)) :- Right is Xs + 1.
neighbor((Xs, Ys), (Left, Ys)) :- Left is Xs - 1.
neighbor((Xs, Ys), (Xs, Up)) :- Up is Ys + 1.
neighbor((Xs, Ys), (Xs, Down)) :- Down is Ys - 1.

%% closest_door((Xs, Ys), Room, Path) - starting at <Xs, Ys>, finds
%% the closest Room door and the Path to it
can_move_to((X, Y)) :- door((X, Y), _); is_free((X, Y)).
valid_adjacent((X, Y), (Xd, Yd)) :-
		neighbor((X, Y), (Xd, Yd)),
		can_move_to((Xd, Yd)).
closest_door_aux(Room, [(X, Y)], [(X, Y)|_], _) :- door((X, Y), Room).
closest_door_aux(Room, [(X, Y)|PTail], [(X, Y)|QTail], Seen) :-
		print((X, Y)),
		print(QTail),nl,
		% enqueues all non-seen adjacents to which it's possible to move
		findall((Xd, Yd), (valid_adjacent((X, Y), (Xd, Yd)), \+ member((Xd, Yd), Seen)), ValidAdjacents),
		append(QTail, ValidAdjacents, Queue),
		% considers all adjacent nodes as seen, to cut the search recursion
		findall((Xa, Ya), neighbor((X, Y), (Xa, Ya)), Adjacents),
		append(Adjacents, Seen, NewSeen),
		closest_door_aux(Room, PTail, Queue, NewSeen). % recursive definition
closest_door((Xs, Ys), Room, Path) :-
		is_free((Xs, Ys)),
		closest_door_aux(Room, Path, [(Xs, Ys)], [(Xs, Ys)]).
