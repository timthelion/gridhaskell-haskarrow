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

>module Cell where

>import qualified Path
>import qualified Super

>import Data.Maybe

Patterns are owned by which statements.

>data Pattern = Pattern {patternLabel   :: Label, 
>                        action         :: Cell}
>           deriving(Show,Read)

>type Label = (Super.Rectangle, String)

>data CellCommon = CellCommon {
>        rectangle :: Super.Rectangle,
>        comments  :: [Label]     
>   }
>   deriving(Show,Read)

| Cells are the main data type used.  All grid haskell commands are cells.  There is a tree of cells which is our program.  These cells get precompiled to haskell code.
             
>data Cell = 
>    Start {
>        common    :: CellCommon,
>        prototype :: Super.Prototype,

 | Name of grid

>        code      :: String,

>        arguments :: [Label],

>        pure      :: Bool,

>        next      :: Cell}


>  | Action {
>        common    :: CellCommon,

>        code      :: String,

| True if it is a pure function false if it is of type IO.  The grid haskell precompiler compiles to something that looks like 

<f0x0   = getChar  >>= \v0x0 ->f0x1  v0x0 
<f0x1  p1  = return ('n' ) >>= \v0x1 ->f1x1 v0x1  p1 v0x1 

The getChar function has type IO Char and thus does not need to be "returned." The 'n' however is not of the IO monad, and we need to return it in order for type checking to work.

>        return    :: Bool,

|  Push the return value of action to stack?  Grid haskell is a stack based language.  We use push and pull to mark stack actions.

>        push      :: Bool,

| Pull the stack and pass it as arguments to the action?

>        pull      :: Bool, 

|  Path to any far away place where return value of action is used.  Sometimes the stack isn't very usefull though, and we really need to carry a value to another place in the code.  We want to make this as bright visual and clear as possible, so we make a path or line on the screen that shows where each value goes(is used).

>        label     :: Maybe Label,

>        path      :: (Maybe Path.Path), 

| The next cell/action in on the grid in executable order.

>        next      :: Cell} 

>  | Lambda {
>        common    :: CellCommon,

>        arguments :: [Label],

>        arrow     :: Super.Rectangle,

>        pure      :: Bool,

>        body      :: Cell,

>        next      :: Cell}


|  Destination place where we take a value that was aquired a while ago and place it on the stack

>  | Destination {
>        common    :: CellCommon,

| Origin place where the value was aquired/returned.

>        origin    :: Super.Point,

>        value     :: String,

| Path to any further uses of the value.

>        path      :: (Maybe Path.Path),
>        next      :: Cell}

Which statement.  One example of how visual programming may be better than textual.

>  | Which {
>        common    :: CellCommon,
>        patterns  :: [Pattern]}

| Jump to an earlier point in the code(that point being the destination of the path).

We use Maybe for type consistency.  path should never be Nothing here though.

>  | Jump {
>        common    :: CellCommon,
>        path      :: (Maybe Path.Path)}  
 
Forking is a very important process in grid haskell.  It should be encouraged rather than discouraged.  Grid haskell is intended to make forking visual and intuitive...

>  | Fork {
>        common    :: CellCommon,
>        newThreads:: [Cell]}

These MVar cells have two points.  Why?  They are displayed on the screen like this:

  NewEmptyMVar   ->  [ nameOfMvar ]
\typical point/      \ labelPoint /

>  | NewEmptyMVar {
>        common    :: CellCommon,
>        mvarLabel :: Label, 
>        next      :: Cell}

 
>  | TakeMVar {
>        common    :: CellCommon,
>        mvarLabel :: Label, 
>        next      :: Cell}


>  | PutMVar {
>        common    :: CellCommon,
>        mvarLabel :: Label, 
>        next      :: Cell}

Return the last value in the stack.

>  | Return{common    :: CellCommon}

End of thread.  Stop, and wait for Exit. 

>  | End{common    :: CellCommon}


Exit program, sending signal to all other threads to exit as well.

>  | Exit{common    :: CellCommon}

As stated earlier, we use show and read to save this to a file.

>    deriving (Show, Read)

