# Clue

This is a SWI-Prolog script that poses as one of the players on a match of [Clue](http://www.hasbro.com/en-us/toys-games/hasbro-games:clue), by Hasbro. It was implemented based on the [rules](clue_hasbro_2002.pdf) of the 2002 version of the game and the board below.

The script basically provides predicates to start a game, inform another player's action and request the next action. The state of the game is stored as a set of facts in the database.

Please check de [documentation](doc/doc.pdf) for a detailed explanation of the implementation and its usage.

The code is licensed under the Apache License, Version 2.0. For details, check the [license file](LICENSE).

![Clue Game Board](doc/board.jpg)
