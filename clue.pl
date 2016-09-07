% Author: Luciano Santos
% Date: 12/08/2016
% This script plays Clue (Hasbro, 2002). See doc/doc.pdf for reference.

:- use_module(library(lists)).

%%%%% The game data.

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
% stairs
blocked((X, Y)) :- X >= 9, X =< 13, Y >= 10, Y =< 16.

%% valid_rooms(Rooms) - Rooms is the list of valid room names.
valid_rooms(
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

%% door((X, Y), R) - there's a door to room R from point <X, Y>
door((5, 5), 'conservatory').
door((7, 5), 'ball room').
door((16, 5), 'ball room').
door((9, 8), 'ball room').
door((14, 8), 'ball room').
door((19, 7), 'kitchen').
door((15, 12), 'dining room').
door((17, 16), 'dining room').
door((17, 18), 'lounge').
door((8, 20), 'hall').
door((11, 17), 'hall').
door((12, 17), 'hall').
door((6, 20), 'study').
door((3, 13), 'library').
door((7, 16), 'library').
door((1, 13), 'billiard room').
door((6, 9), 'billiard room').

%% passage(Src, Dst) - there's a passage from Src to Dst
passage_aux('conservatory', 'lounge').
passage_aux('kitchen', 'study').
passage(Src, Dst) :- passage_aux(Src, Dst) ; passage_aux(Dst, Src).

%%% The game characters.

%% valid_chars(Chars) - Chars is the list of valid characters,
%% in the order they must play.
valid_chars(
	[
		'scarlet',
		'mustard',
		'white',
		'green',
		'peacock',
		'plum'
	]
).

%% my_char(Char) - Char is the character of this agent.
:- dynamic my_char/1.

%% position((X, Y), C) - the current position of character C is <X, Y>.
%% here, it's initialized to the start position at the board.
%% this predicate will be "rewritten" whenever the script's own
%% character moves or it receives information that another character
%% moved.
:- dynamic position/2.
position((16, 24), 'scarlet').
position((23, 17), 'mustard').
position((14, 0), 'white').
position((9, 0), 'green').
position((0, 6), 'peacock').
position((0, 19), 'plum').

%% last_position((X, Y), C) - the position of character C was <X, Y> last turn.
%% it'll be updated as the script's own character moves or it receives
%% information that another character moved.
:- dynamic last_position/2.
last_position(nil, 'scarlet').
last_position(nil, 'mustard').
last_position(nil, 'white').
last_position(nil, 'green').
last_position(nil, 'peacock').
last_position(nil, 'plum').

%%% The game weapons.

%% valid_weapons(Weapons) - Weapons is the list of valid weapons.
valid_weapons(
	[
		'rope',
		'pipe',
		'knife',
		'wrench',
		'candlestick',
		'pistol'
	]
).

%%% Acquired information.

%% shown_char(Player, Char) - the player Player (could be myself)
%% has shown me the card for Char.
:- dynamic shown_char/2.

%% shown_room(Player, Room) - the player Player (could be myself)
%% has shown me the card for Room.
:- dynamic shown_room/2.

%% shown_weapon(Player, Weapon) - the player Player (could be myself)
%% has shown me the card for Weapon.
:- dynamic shown_weapon/2.



%%%%% Movement logic.

%% neighbor((Xs, Ys), (Xn, Yn)) - <Xn, Yn> is neighbor of <Xs, Ys>.
neighbor((Xs, Ys), (Right, Ys)) :- Right is Xs + 1.
neighbor((Xs, Ys), (Left, Ys)) :- Left is Xs - 1.
neighbor((Xs, Ys), (Xs, Up)) :- Up is Ys + 1.
neighbor((Xs, Ys), (Xs, Down)) :- Down is Ys - 1.

%% is_free((X, Y)) - the position <X, Y> can bee occupied by a character.
is_free((X, Y)) :-
		X >= 0, X =< 23, Y >= 0, Y =< 24, % inside the board
		\+ position((X, Y), _), % not currently occupied by anyone
		\+ blocked((X, Y)). % not a room or a wall

%% closest_door((Xs, Ys), Room, Path) - starting at <Xs, Ys>, finds
%% the closest Room door and the Path to it
unseen_neighbor((X, Y), (Xd, Yd), Seen) :-
		neighbor((X, Y), (Xd, Yd)),
		\+ member(((Xd, Yd), _), Seen).
valid_adjacent((X, Y), (Xd, Yd), Seen) :-
		unseen_neighbor((X, Y), (Xd, Yd), Seen),
		is_free((Xd, Yd)).
add_parent([], _, []).
add_parent([Node|NodesTail], Parent, [(Node, Parent)|LinkedTail]) :-
		add_parent(NodesTail, Parent, LinkedTail).
build_path(Head, Seen, [Head]) :- member((Head, nil), Seen).
build_path(Head, Seen, [Head|Tail]) :-
		member((Head, Parent), Seen),
		build_path(Parent, Seen, Tail).
closest_door_aux(Room, Path, [Head|_], Seen) :-
		door(Head, Room),
		build_path(Head, Seen, InversePath),
		reverse(InversePath, Path).
closest_door_aux(Room, Path, [(X, Y)|QTail], Seen) :-
		% enqueues all non-seen adjacents to which it's possible to move
		findall((Xd, Yd), valid_adjacent((X, Y), (Xd, Yd), Seen), ValidAdjacents),
		append(QTail, ValidAdjacents, NewQueue),
		% stores all adjacent nodes as seen, to cut the search recursion
		findall((Xa, Ya), unseen_neighbor((X, Y), (Xa, Ya), Seen), Neighbors),
		add_parent(Neighbors, (X, Y), LinkedNeighbors),
		append(LinkedNeighbors, Seen, NewSeen),
		closest_door_aux(Room, Path, NewQueue, NewSeen). % recursive definition
closest_door((Xs, Ys), Room, Path) :-
		%is_free((Xs, Ys)),
		closest_door_aux(Room, Path, [(Xs, Ys)], [((Xs, Ys), nil)]).
closest_door(SourceRoom, TargetRoom, Path) :-
		valid_rooms(ValidRooms), member(SourceRoom, ValidRooms),
		findall(Exit, door(Exit, SourceRoom), Exits),
		add_parent(Exits, nil, LinkedExits),
		closest_door_aux(TargetRoom, Path, Exits, LinkedExits).



%%%%% Decision logic.

%%% Accusation

%% the next predicates generate the known and unknown lists
%% of characters, rooms and weapons, i.e., those whose cards
%% have and have not yet been shown so far
known_chars(KnownChars) :- findall(C, shown_char(_, C), KnownChars).
unknown_chars(UnknownChars) :-
		valid_chars(ValidChars),
		known_chars(KnownChars),
		subtract(ValidChars, KnownChars, UnknownChars).
known_rooms(KnownRooms) :- findall(R, shown_room(_, R), KnownRooms).
unknown_rooms(UnknownRooms) :-
		valid_rooms(ValidRooms),
		known_rooms(KnownRooms),
		subtract(ValidRooms, KnownRooms, UnknownRooms).
known_weapons(KnownWeapons) :- findall(W, shown_weapon(_, W), KnownWeapons).		
unknown_weapons(UnknownWeapons) :-
		valid_weapons(ValidWeapons),
		known_weapons(KnownWeapons),
		subtract(ValidWeapons, KnownWeapons, UnknownWeapons).

can_accuse(Person, Room, Weapon) :-
		unknown_chars([Person]),
		unknown_rooms([Room]),
		unknown_weapons([Weapon]).


%%% Player rank.

%% pivot_split(List, Pivot, Left, Right) - Splits List in two parts:
%% - Left -> elements before Pivot
%% - Right -> Pivot plus the elements after it.
%% If Pivot is not in the list, Left = List and Right = [].
pivot_split([], _, [], []).
pivot_split([Pivot|Tail], Pivot, [], [Pivot|Tail]) :- !.
pivot_split([Head|Tail], Pivot, [Head|Left], Right) :-
		pivot_split(Tail, Pivot, Left, Right).

%% char_rank(MyChar, Char, Rank) - gives a Rank to Char relative to MyChar,
%% that's the order in which Char would play, after MyChar, starting from 0.
char_pos(_, [], Pos, Pos).
char_pos(Char, [Char|_], Pos, Pos) :- !.
char_pos(Char, [_|Tail], Cur, Pos) :-
		Next is Cur + 1,
		char_pos(Char, Tail, Next, Pos).
char_rank(MyChar, Char, Rank) :-
		valid_chars(ValidChars),
		pivot_split(ValidChars, MyChar, Left, Right),
		append(Right, Left, Rotated),
		char_pos(Char, Rotated, 0, Rank).


%%% Room picking

%% remove_if_not_entered(Rooms, MyChar, MyPos, Result) - if i'm in a room and did
%% not enter it this turn, remove it from the list.
remove_if_not_entered(Rooms, MyChar, MyPos, Result) :-
		valid_rooms(ValidRooms), member(MyPos, ValidRooms), % I'm in a room
		last_position(MyPos, MyChar), % I didn't enter it this turn
		delete(Rooms, (MyPos, _), Result), !.
remove_if_not_entered(Rooms, _, _, Rooms).

%% check_secret_passage(Rooms, MyPos, Result) - if there's a secret passage
%% from position MyPos (a room) to a target room, adds (TargetRoom, 'secret passage')
%% to the beginning of Rooms (a list of (Room, Path)).
check_secret_passage(Rooms, MyPos, [(TargetRoom, 'secret passage')|Rooms]) :-
		passage(MyPos, TargetRoom), !.
check_secret_passage(Rooms, _, Rooms).

%% room_compare(Order, A, B) - unifies Order to the relative order (<, >, =)
%% between two elements A and B of a room list (Room, Path).
%% a path in the form of a secret passage is greater than a unitary path, i.e.,
%% a path that take zero steps to reach the goal (only the source), and less
%% than any other path. all other paths are compared by length.
room_compare(>, (_, 'secret passage'), (_, [_])) :- !.
room_compare(<, (_, 'secret passage'), _) :- !.
room_compare(Delta, (_, PathA), (_, PathB)) :-
		length(PathA, LenA), length(PathB, LenB),
		compare(Delta, LenA, LenB).

%% sort_rooms(Rooms, Sorted) - unifies the list of rooms, sorted by the
%% order defined on room_compare
sort_rooms(Rooms, Sorted) :- predsort(room_compare, Rooms, Sorted).

%% pick_room_not_shown_next(MyChar, Rooms, SeenRooms, Room, Path) - given Rooms, a list of (Room, Path)
%% sorted by path, picks the first room that was not shown by the next player or the
%% first element in the list, if no such room exists.
pick_room_not_shown_next(_, [], SeenRooms, Room, Path) :-
		append(_, [(Room, Path)], SeenRooms), !. % take the last element of SeenRooms
pick_room_not_shown_next(_, [(Room, Path)|_], _, Room, Path) :-
		\+ shown_room(_, Room), !.
pick_room_not_shown_next(MyChar, [(Room, Path)|_], _, Room, Path) :-
		shown_room(Char, Room),
		char_rank(MyChar, Char, Rank),
		Rank =\= 1, !.
pick_room_not_shown_next(MyChar, [(R, P)|Tail], SeenRooms, Room, Path) :-
		pick_room_not_shown_next(MyChar, Tail, [(R, P)|SeenRooms], Room, Path), !.

%% picks a suitable room to be used this turn (could be the room it's currently in)
pick_room(Room, Path) :-
		my_char(MyChar),
		position(MyPos, MyChar),
		findall((R, P), closest_door(MyPos, R, P), Rooms),
		remove_if_not_entered(Rooms, MyChar, MyPos, WithoutCurrent),
		check_secret_passage(WithoutCurrent, MyPos, WithPassage),
		sort_rooms(WithPassage, Sorted),
		pick_room_not_shown_next(MyChar, Sorted, [], Room, Path), !.
