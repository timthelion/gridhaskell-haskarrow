GPLV3.0 or later copyright brmlab.cz contact timothyhobbs@seznam.cz

Copyright 2012.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

>module GridExample where

>import Grid
>import Path
>import Cell
>import Scope
>import Super

This is an example program written in grid haskell.  It reads characters from input untill the user types n.

>grid :: Grid

>grid = Grid{

>        message           = "",
>        gridName          = "gridhaskell",
>        gridLicence       = 
>"{-GPLV3.0 or later copyright brmlab.cz contact timothyhobbs@seznam.cz\
>\\
>\Copyright 2012.\
>\\
>\This program is free software: you can redistribute it and/or modify\
>\it under the terms of the GNU General Public License as published by\
>\the Free Software Foundation, either version 3 of the License, or\
>\(at your option) any later version.\
>\\
>\This program is distributed in the hope that it will be useful,\
>\but WITHOUT ANY WARRANTY; without even the implied warranty of\
>\MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\
>\GNU General Public License for more details.\
>\\
>\You should have received a copy of the GNU General Public License\
>\along with this program.  If not, see <http://www.gnu.org/licenses/>.\
>\-}",
>        gridImports       = ["Control.Concurrent","System.Exit"],

        gridComments      = [((-1,1), "This is a comment."),
                             ((3,9), "These are some loose cells...")],

>        gridCells = (Start (CellCommon ((-1,0),smallRectangle) []) "" "main" [] False

Preform a group of actions, leaving the stack alone.

>   (Lambda (CellCommon ((0,0),smallRectangle) []) [] ((1,0),smallRectangle) False

Get a character from stdin and push it to the stack. 

>	(Action (CellCommon ((2,0),smallRectangle) []) "getChar" False True False Nothing Nothing 

Push 'n' to the stack.  We also carry this value down a path, just to sho we can...

>	(Action (CellCommon ((3,0),smallRectangle) []) "'n'" True True False Nothing Nothing

Pull the two stack items, compair them, and push the result to the stack.

>	(Action (CellCommon ((4,0),smallRectangle) []) "(==)" True True True Nothing Nothing

Return the value of the ==.

>   (Return (CellCommon ((5,0),smallRectangle) [])))))

Pull that value from the stack and pattern match it against True, or False, (an ulimited number of patters of course is possible :)

>	(Which (CellCommon ((0,3),smallRectangle) []) 
>	[(Pattern {patternLabel   = (((2,5),smallRectangle),"True"),

If the character typed was 'n' output the 'n' which we carried down the path from action (0,1)

>   action = (Action (CellCommon ((2,6),smallRectangle) []) "'n'" True True False Nothing Nothing
>	(Action (CellCommon ((2,7),smallRectangle) []) "putChar" False False True Nothing Nothing 

and exit.  Note, there IS an important difference between Exit and End.  End does NOTHING.  Exit kills everything in an indiscriminate, imediate, and genocidal fashion.

>   (Action (CellCommon ((2,8),smallRectangle) []) "exitSuccess" True True False Nothing Nothing 
>   (Exit (CellCommon ((2,9),smallRectangle) [])))))}),

Otherwise,

>	(Pattern {patternLabel   = (((-2,5),smallRectangle),"False") ,

Fork just to show we can...

>   action = 

Creating a new MVar.

>   (NewEmptyMVar (CellCommon ((-2,6),smallRectangle) []) (((-1,6),smallRectangle), "char")  

Fork into two threads.  Despite there being a concept of parent and child threads, the user of grid haskell see's no such destinction.  In fact, Fork forks to a list of threads. The UI for it is quite similar to that of switch.

>   (Fork (CellCommon ((-2,7),smallRectangle) [])

One thread gets a character and puts it in our MVar.

>	[(Action (CellCommon ((-4,9),smallRectangle) []) "getChar" False True False Nothing Nothing
>	(PutMVar (CellCommon ((-4,10),smallRectangle) []) (((-3,10),smallRectangle), "char") (End (CellCommon ((-4,11),smallRectangle) [])))),

The other thread gets that character out of the MVar and puts it on the stack.

>	(TakeMVar (CellCommon ((-1,11),smallRectangle) []) (((-2,11),smallRectangle), "char")

Then we put that to output.

>   (Action (CellCommon ((-1,12),smallRectangle) []) "putChar" False False True Nothing Nothing

And then "Jumps" back up to the start of our "loop" which began at the switch statement.  I know that GOTO statements are evil when it comes to textual programs.  Hopefully, I can prove to you, that in visual programming, this is very much not the case.

>	(Jump (CellCommon ((-1,13),smallRectangle) []) (Just (SteppingStone (0,12)
>                 (SteppingStone   (1,2)
>				  (PathDestination (0,0))))))))]))})]))),
>   gridLooseCells=[(Action (CellCommon ((3,10),smallRectangle) []) "getChar" False True False Nothing Nothing (End (CellCommon ((3,11),smallRectangle) [])))]}

\end{code}

This example precompiles to

\begin{scheme}%
--TODO TODO TODO--
\end{scheme}
