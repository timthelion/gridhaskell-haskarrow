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

| This is the text that gets displayed on the screen.  It is not the haskell code to which these cells are precompiled.

>cellText :: Cell -> String
>cellText cell@Start{}        = code cell
>cellText cell@Action{}       = code cell
>cellText Lambda{}            = "λ"
>cellText Which{}             = "Which"
>cellText cell@Destination{}  = value cell
>cellText Jump{}              = "Jump"
>cellText Fork{}              = "Fork"
>cellText cell@NewEmptyMVar{} = "newEmptyMVar"
>cellText cell@PutMVar{}      = "putMVar"
>cellText cell@TakeMVar{}     = "takeMVar"
>cellText Return{}            = "Return"
>cellText End{}               = "End"
>cellText Exit{}              = "Exit"

We use this to build a list of cells for display on the screen.

>cellsNext :: Cell -> [Cell]
>cellsNext cell@Jump{}     = []
>cellsNext cell@Return{}   = []
>cellsNext cell@End{}      = []
>cellsNext cell@Exit{}     = []
>cellsNext cell@Fork{}     = newThreads cell
>cellsNext cell@Which{}    = map action (patterns cell)
>cellsNext cell@Lambda{}   = next cell : body cell : []
>cellsNext cell            = [next cell]

>cellComments :: Cell -> [Label]
>cellComments cell = comments (common cell)

>cellPoint :: Cell -> Super.Point
>cellPoint cell = fst (rectangle (common cell))

>cellPutCode :: Cell -> String -> Maybe Cell
>cellPutCode _ "Fork"        = Nothing
>cellPutCode _ "Jump"        = Nothing
>cellPutCode _ "End"         = Nothing
>cellPutCode _ "Which"       = Nothing
>cellPutCode _ "Destination" = Nothing

>cellPutCode cell@Start{}  code' = Just cell{code=code'}
>cellPutCode cell@Action{} code' = Just cell{code=code'}

>cellPutCode _ _ = Nothing

|Put the first cell into the seccond cell(tree), at the point of the first cell.  We return a tuple with our new tree of cells, plus the cells that used to come after the cell we just replaced.

>cellPutCell :: Cell -> Cell -> (Cell,[Cell])  
>cellPutCell whatToPut whereToPut@Which{} =
> (cells',strays)
> where (cells',strays) = if cellPoint whatToPut == cellPoint whereToPut
>                          then (whatToPut,cellsNext whereToPut)
>                          else (whereToPut
>                            {patterns=
>                              map (\pattern ->
>                                    pattern{ action=
>                                     fst (cellPutCell whatToPut (action pattern))}) 
>                                    (patterns whereToPut)},[])

>cellPutCell whatToPut whereToPut@Fork{} =
> (cells',strays)
> where (cells',strays) = if  cellPoint whatToPut == cellPoint whereToPut
>                          then (whatToPut,cellsNext whereToPut)
>                          else (whereToPut{newThreads=map (\nextCells -> fst (cellPutCell whatToPut nextCells)) (newThreads whereToPut)},[])

>cellPutCell whatToPut whereToPut@Jump{} =
> if  cellPoint whatToPut == cellPoint whereToPut
> then (whatToPut,[])
> else (whereToPut,[])

>cellPutCell whatToPut whereToPut@End{} =
> if  cellPoint whatToPut == cellPoint whereToPut
> then (whatToPut,[])
> else (whereToPut,[])

>cellPutCell whatToPut whereToPut@Exit{} =
> if  cellPoint whatToPut == cellPoint whereToPut
> then (whatToPut,[])
> else (whereToPut,[])

>cellPutCell whatToPut whereToPut@Return{} =
> if  cellPoint whatToPut == cellPoint whereToPut
> then (whatToPut,[])
> else (whereToPut,[])


>cellPutCell whatToPut whereToPut =
> (cells',strays)
> where (cells',strays) = if  cellPoint whatToPut == cellPoint whereToPut
>                          then (whatToPut,cellsNext whereToPut)
>                          else (whereToPut{next=fst(cellPutCell whatToPut (next whereToPut))},[])

| Returns a new cell with the list of point1s reloacted to their corresponding point2s in the (point1,point2) tuples.

>cellPointsRelocation :: Cell -> [(Super.Point,Super.Point)] -> Cell
>cellPointsRelocation originalCells@Start{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>  arguments = argumentsPointsRelocation (arguments originalCells) relocations,
>  next = cellPointsRelocation (next originalCells) relocations
>  }

>cellPointsRelocation originalCells@Action{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>  label = case (label originalCells) of
>            Just label -> Just $ labelRelocation label relocations
>            Nothing    -> Nothing,
>  path = case (path originalCells) of
>            Just path -> Just $ pathPointsRelocation path relocations
>            Nothing   -> Nothing,
>  next = cellPointsRelocation (next originalCells) relocations
>  }

>cellPointsRelocation originalCells@Lambda{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>  arguments = argumentsPointsRelocation (arguments originalCells) relocations,
>  arrow = rectangleRelocation (arrow originalCells) relocations,
>  body  = cellPointsRelocation (body originalCells) relocations,
>  next  = cellPointsRelocation (next originalCells) relocations
>  }


>cellPointsRelocation originalCells@Destination{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>  origin = relocatePoint (origin originalCells) relocations,
>  path = case (path originalCells) of
>            Just path -> Just $ pathPointsRelocation path relocations
>            Nothing   -> Nothing,
>  next = cellPointsRelocation (next originalCells) relocations
>  }

>cellPointsRelocation originalCells@Which{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>    patterns = map (\pattern -> patternPointsRelocation pattern relocations) (patterns originalCells)}

>cellPointsRelocation originalCells@Jump{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>  path = case (path originalCells) of
>            Just path -> Just $ pathPointsRelocation path relocations
>            Nothing   -> Nothing
>  }

>cellPointsRelocation originalCells@Fork{} relocations  =
>  (cellPointsRelocation' originalCells relocations){
>  newThreads = map (\thread -> cellPointsRelocation thread relocations) (newThreads originalCells)
>  }

>cellPointsRelocation originalCells@NewEmptyMVar{} relocations  =
> mvarPointsRelocation originalCells relocations
>cellPointsRelocation originalCells@PutMVar{} relocations  =
> mvarPointsRelocation originalCells relocations
>cellPointsRelocation originalCells@TakeMVar{} relocations  =
> mvarPointsRelocation originalCells relocations

>cellPointsRelocation originalCells relocations =
> cellPointsRelocation' originalCells relocations

>cellPointsRelocation' :: Cell -> [(Super.Point,Super.Point)] -> Cell
>cellPointsRelocation' originalCells relocations  =
> originalCells{
>   common = commonRelocation (common originalCells) relocations}

>argumentsPointsRelocation :: [Label] ->  [(Super.Point,Super.Point)] -> [Label]
>argumentsPointsRelocation arguments relocations =
> map
>   (\label -> labelRelocation label relocations)
>   arguments

>pathPointsRelocation :: Path.Path -> [(Super.Point,Super.Point)] -> Path.Path
>pathPointsRelocation path@Path.SteppingStone{} relocations =
>  path{Path.point = relocatePoint (Path.point path) relocations,
>       Path.next  = pathPointsRelocation (Path.next path) relocations}
>pathPointsRelocation path@Path.PathDestination{} relocations =
>  path{Path.point = relocatePoint (Path.point path) relocations}

>patternPointsRelocation :: Pattern -> [(Super.Point,Super.Point)] -> Pattern
>patternPointsRelocation pattern relocations =
>  pattern{patternLabel = labelRelocation (patternLabel pattern) relocations,
>  action = cellPointsRelocation (action pattern) relocations}

>mvarPointsRelocation :: Cell -> [(Super.Point,Super.Point)] -> Cell
>mvarPointsRelocation originalCells relocations =
>  (cellPointsRelocation' originalCells relocations){
>   mvarLabel = labelRelocation (mvarLabel originalCells) relocations,
>   next = cellPointsRelocation (next originalCells) relocations
>   }

>labelRelocation :: Label -> [(Super.Point,Super.Point)] -> Label
>labelRelocation (rectangle,string) relocations = (rectangleRelocation rectangle relocations,string)

>commonRelocation :: CellCommon -> [(Super.Point,Super.Point)] -> CellCommon
>commonRelocation common relocations = common{rectangle = rectangleRelocation (rectangle common) relocations}

>rectangleRelocation :: Super.Rectangle ->  [(Super.Point,Super.Point)] -> Super.Rectangle
>rectangleRelocation (point,size) relocations = (relocatePoint point relocations,size)

>relocatePoint :: Super.Point -> [(Super.Point,Super.Point)] -> Super.Point
>relocatePoint point relocations =
>  case
>     catMaybes $
>      map
>        (\tup -> if (fst tup) == point
>                 then Just (snd tup)
>                 else Nothing)
>        relocations
>     of
>  (p:ps) -> p
>  []     -> point

>cellPointFilled :: Cell -> Super.Point -> Bool
>cellPointFilled cell pointToCheck =
> if pointToCheck == cellPoint cell
> then True
> else (case cell of
>  Start{} ->
>   any (\label -> labelPointFilled label pointToCheck) (arguments cell)
>  Action{} -> (case (label cell) of
>               Just label -> labelPointFilled label pointToCheck
>               Nothing         -> False) ||
>              (case (path cell) of
>               Just path -> Path.pathPointFilled path pointToCheck
>               Nothing   -> False)
>  Lambda{} -> (any (\label -> labelPointFilled label pointToCheck) (arguments cell)) ||
>              (rectanglePointFilled (arrow cell) pointToCheck)
>  Destination{} -> case (path cell) of
>                    Just path -> Path.pathPointFilled path pointToCheck
>                    Nothing -> False
>  Which{} -> or $ map (\pattern -> labelPointFilled (patternLabel pattern) pointToCheck) (patterns cell)
>  Jump{}  -> case (path cell) of
>              Just path -> Path.pathPointFilled path pointToCheck
>              Nothing -> False
>  NewEmptyMVar{} -> labelPointFilled (mvarLabel cell) pointToCheck
>  TakeMVar{}     -> labelPointFilled (mvarLabel cell) pointToCheck
>  PutMVar{}      -> labelPointFilled (mvarLabel cell) pointToCheck
>  otherwise      -> False) ||
>  (or $ map (\cell->cellPointFilled cell pointToCheck) (cellsNext cell))

| Is the point covered by the label?

>labelPointFilled :: Label -> Super.Point -> Bool
>labelPointFilled (rectangle,_) point = rectanglePointFilled rectangle point

| Is the point inside the Rectangle?

>rectanglePointFilled :: Super.Rectangle -> Super.Point -> Bool
>rectanglePointFilled ((rx,ry),(rw,rh)) (px,py) = (px >= rx && px < rx + rw) && (py >= ry && py < ry + rh)

>labelPoint :: Label -> Super.Point
>labelPoint (rectangle,_) = Super.rectanglePoint rectangle

>labelText :: Label -> String
>labelText (_,string) = string

>commonPoint :: CellCommon -> Super.Point
>commonPoint common = Super.rectanglePoint $ rectangle common

>patternPoint :: Pattern -> Super.Point
>patternPoint pattern = labelPoint (patternLabel pattern)
