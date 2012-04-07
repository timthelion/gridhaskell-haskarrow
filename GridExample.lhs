>module GridExample where

>import Grid
>import Path
>import Cell
>import Scope

This is an example program written in grid haskell.  It reads characters from input untill the user types n.

>grid :: Grid

>grid = Grid{

>    message="This is a computer generated file.  Do not edit.",

>        gridName          = "gridhaskell",
>        gridLicence       = "{-GPLv3-}",
>        gridImports       = ["Control.Concurrent","System"],
>        gridPureFunctions = [("","")],
>        gridComments      = [((-1,1), "This is a comment.")],

>        gridCells = (Start (-1,0) "" "main" []

Get a character from stdin and push it to the stack. 

>	(Action (0,0) "getChar" False True False Nothing 

Push 'n' to the stack.  We also carry this value down a path, just to sho we can...

>	(Action (0,1) "'n'" True True False (Just (SteppingStone (3,1)
>                                       (SteppingStone   (3,6)
>										(PathDestination (2,6)))))

Pull the two stack items, compair them, and push the result to the stack.

>	(Action (0,2) "(==)" True True True Nothing

Pull that value from the stack and pattern match it against True, or False, (an ulimited number of patters of course is possible :)

>	(Switch (0,3) 
>	[(Pattern {patternPoint   = (2,5) ,
>              pattern = "True",

If the character typed was 'n' output the 'n' which we carried down the path from action (0,1)

>   action = (Destination (2,6) (0,1) Nothing
>	(Action (2,7) "putChar" False False True Nothing 

and exit.  Note, there IS an important difference between Exit and End.  End does NOTHING.  Exit kills everything in an indiscriminate, imediate, and genocidal fashion.

>   (Exit (2,8) "ExitSuccess")))}),

Otherwise,

>	(Pattern {patternPoint   = (-2,5) ,
>             pattern = "False",

Fork just to show we can...

>   action = 

Creating a new MVar.

>   (NewEmptyMVar (-2,6) (-1,6) "char"  

Fork into two threads.  Despite there being a concept of parent and child threads, the user of grid haskell see's no such destinction.  In fact, Fork forks to a list of threads. The UI for it is quite similar to that of switch.

>   (Fork (-2,7)

One thread gets a character and puts it in our MVar.

>	[(Action (-4,9) "getChar" False True False Nothing
>	(PutMVar (-4,10) (-3,10) "char" (End (-4,11)))),

The other thread gets that character out of the MVar and puts it on the stack.

>	(TakeMVar (-1,11) (-2,11) "char"

And then "Jumps" back up to the start of our "loop" which began at the switch statement.  I know that GOTO statements are evil when it comes to textual programs.  Hopefully, I can prove to you, that in visual programming, this is very much not the case.

>	(Jump (-1,12) (Just (SteppingStone (0,12)
>                (SteppingStone   (1,2)
>				 (PathDestination (0,1)))))))]))})])))))}

\end{code}

This example precompiles to

\begin{scheme}%
--TODO TODO TODO--
\end{scheme}
