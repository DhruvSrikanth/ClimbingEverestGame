# functional-adventure

---

### Name - Dhruv Srikanth

### Date - 12/10/2021 (10th Decemeber, 2021)

---

<H3> Acknowledgments: </H3>

First, I'd like to thank the professor, **Matt** and the TA, **Jacob**. I have emailed both of them with doubts and they have always helped in clearing them. Furthermore, Matt's energy in class, as well as style of teaching is something different. I am never bored or tired in class and am always learning something interesting (albiet quite complex). Also, thanks so much for the spotify playlist! Im really emjoying it!!.

Finally, I would like to thank **Rohit Kumar** and **Minjeong Woo**, both of whom I have gotten to know over this past quarter and have helped understand assignments, work out code, or even just lighten the mood when we are stuck on a question. 

---

<H3> Instructions on the Game: </H3>

1. The goal is to get to the summit of everest.
2. To win the game, you must pick up the flag at the south summit and bring it to everest.
3. An extra feature is that if you do not pick up the snacks in Camp 3 and the oxygen at ABC, you will die if you make it to the south summit, everest summit or lhotse summit.

A map of the game can be seen below - 

![GameMap](/functional-adventure/game_map.PNG)

You will start at everest base camp. (As indicated by the star when you use the map command).

Commands that can be used are -
1. map - View where you are on the map (as indicated by the asterisk).
2. look - Look to see what items are in this room, what room you are in and where you can go from here.
3. take item/item1,item2... - Take an item (specify the name) or items (comma separated) into your inventory.
4. drop item/item1,item2... - Drop an item (specify the name) or items (comma separated) into your inventory.
5. north/south/east/west - Use any of these commands to move in that particular direction (if possible).
6. exit/quit - use either of these commands to exit the game.

Note - You can chain commands by separating each command by "and".

Items Present - 

I wont be divulging where I have placed each item (the important ones have been stated above), because I believe part of the fun of this game is in finding what items are placed around and where they are placed.

1. Summit Stone
2. Trekking Poles
3. Skis
4. Ice Axes
5. Ropes
6. Flag
7. Snacks
8. Ladders
9. Oxygen
10. Camera

---

<H3> Runtime Bugs: </H3>

I was not able to finish the following things - 
1. For some reason, the randomMap function works some times at at other times, the program seems to be stuck in a recursive loop. It is for this reason and the fact that I am using the initialState to start the game. Additionally, for my game (climbing everest), I thought starting with the initialState made more sense.
2. I was not able to create the random boxes whilst having fixed surrounding rooms. My thought process was to use this as a base and then alter it in such a way that only the spaces between camp1 and camp2 would be randomly generate but I did not have time to do so.

Other than this, I have not encountered any run time bugs but feel free to test it out to find some :).

---

<H3> Extra Credit Features: </H3>

1. I have added a few extra rooms and items.
2. I have implemented a feature where the player will die if he or she does not pick up the snacks and the oxygen present at two different camps before the South Summit, the summit of Everest or the summit of Lhotse. The implementation can be found at these functions - dieGame, repl, checkGameDie (in GameIO) and willDieGame (in GameState).

---

