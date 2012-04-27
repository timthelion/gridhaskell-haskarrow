>module Cell where

>import qualified Path
>import qualified Super

>import Data.Maybe

Patterns are owned by which statements.

>data Pattern = Pattern {patternPoint   :: Super.Point,
>                        pattern        :: String, 
>                        action         :: Cell}
>           deriving(Show,Read)


| Cells are the main data type used.  All grid haskell commands are cells.  There is a tree of cells which is our program.  These cells get precompiled to haskell code.
             
>data Cell = 
>    Start {
>        point     :: Super.Point,
>        prototype :: Super.Prototype,

 | Name of grid

>        code      :: String,

>        arguments :: [(Super.Point,String)],

>        next      :: Cell}


>  | Action {
>        point     :: Super.Point,
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

>        label     :: Maybe (Super.Point, String),

>        path      :: (Maybe Path.Path), 

| The next cell/action in on the grid in executable order.

>        next      :: Cell} 

>  | Lambda {
>        point     :: Super.Point,

>        arguments :: [(Super.Point,String)],

>        arrow     :: Super.Point,
>        body      :: Cell,

>        next      :: Cell}


|  Destination place where we take a value that was aquired a while ago and place it on the stack

>  | Destination {
>        point     :: Super.Point, 

| Origin place where the value was aquired/returned.

>        origin    :: Super.Point,

>        value     :: String,

| Path to any further uses of the value.

>        path      :: (Maybe Path.Path),
>        next      :: Cell}

Which statement.  One example of how visual programming may be better than textual.

>  | Which {
>        point     :: Super.Point,
>        patterns  :: [Pattern]}

| Jump to an earlier point in the code(that point being the destination of the path).

We use Maybe for type consistency.  path should never be Nothing here though.

>  | Jump {
>        point     :: Super.Point,
>        path      :: (Maybe Path.Path)}  
 
Forking is a very important process in grid haskell.  It should be encouraged rather than discouraged.  Grid haskell is intended to make forking visual and intuitive...

>  | Fork {
>        point     :: Super.Point,
>        newThreads:: [Cell]}

These MVar cells have two points.  Why?  They are displayed on the screen like this:

  NewEmptyMVar   ->  [ nameOfMvar ]
\typical point/      \ labelPoint /

>  | NewEmptyMVar {
>        point     :: Super.Point,
>        labelPoint:: Super.Point,
>        mvar      :: String,
>        next      :: Cell}

 
>  | TakeMVar {
>        point     :: Super.Point,
>        labelPoint:: Super.Point,
>        mvar      :: String,
>        next      :: Cell}


>  | PutMVar {
>        point     :: Super.Point,
>        labelPoint:: Super.Point,
>        mvar      :: String,
>        next      :: Cell}

Return the last value in the stack.

>  | Return{point  :: Super.Point}

End of thread.  Stop, and wait for Exit. 

>  | End{point     :: Super.Point}


Exit program, sending signal to all other threads to exit as well.

>  | Exit{point     :: Super.Point,
>         signal    :: String}

As stated earlier, we use show and read to save this to a file.

>    deriving (Show, Read)

| This is the text that gets displayed on the screen.  It is not the haskell code to which these cells are precompiled.

>cellText :: Cell -> String
>cellText cell@Start{}        = code cell
>cellText cell@Action{}       = code cell
>cellText Lambda{}            = "Lambda"
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

>cellNext :: Cell -> [Cell]
>cellNext cell@Jump{}     = []
>cellNext cell@Return{}   = []
>cellNext cell@End{}      = []
>cellNext cell@Exit{}     = []
>cellNext cell@Fork{}     = newThreads cell
>cellNext cell@Which{}    = map action (patterns cell)
>cellNext cell@Lambda{}   = next cell : body cell : []
>cellNext cell            = [next cell]

>cellPutCode :: Cell -> String -> Maybe Cell
>cellPutCode _ "Fork"        = Nothing
>cellPutCode _ "Jump"        = Nothing
>cellPutCode _ "End"         = Nothing
>cellPutCode _ "Which"       = Nothing
>cellPutCode _ "Destination" = Nothing

>cellPutCode cell@Start{}  code' = Just cell{code=code'}
>cellPutCode cell@Action{} code' = Just cell{code=code'}

>cellPutCode cell@NewEmptyMVar{} mvar' = Just cell{mvar=mvar'}
>cellPutCode cell@TakeMVar{}     mvar' = Just cell{mvar=mvar'}
>cellPutCode cell@PutMVar{}      mvar' = Just cell{mvar=mvar'}

>cellPutCode cell@Exit{} signal' = Just cell{signal=signal'}

|Put the first cell into the list of cells, at the point of the cell.  We return a tuple with our new tree of cells, plus the cells that used to come after the cell we just replaced.

>cellPutCell :: Cell -> Cell -> (Cell,[Cell]) 

>cellPutCell cell cells@Which{} =
> (cells',strays)
> where (cells',strays) = if (point cell) == (point cells)
>                          then (cell,cellNext cells)
>                          else (cells{patterns=map (\pattern -> pattern{action=fst (cellPutCell cell (action pattern))}) (patterns cells)},[])

>cellPutCell cell cells@Fork{} =
> (cells',strays)
> where (cells',strays) = if (point cell) == (point cells)
>                          then (cell,cellNext cells)
>                          else (cells{newThreads=map (\nextCells -> fst (cellPutCell cell nextCells)) (newThreads cells)},[])

>cellPutCell cell cells@Jump{} =
> (cells,[])
>cellPutCell cell cells@End{} =
> (cells,[])
>cellPutCell cell cells@Exit{} =
> (cells,[])

>cellPutCell cell cells =
> (cells',strays)
> where (cells',strays) = if (point cell) == (point cells)
>                          then (cell,cellNext cells)
>                          else (cells{next=fst(cellPutCell cell (next cells))},[])

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
>            Just (p,s) -> Just (relocatePoint p relocations, s)
>            Nothing    -> Nothing,
>  path = case (path originalCells) of
>            Just path -> Just $ pathPointsRelocation path relocations
>            Nothing   -> Nothing,
>  next = cellPointsRelocation (next originalCells) relocations
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
>   point = relocatePoint (point originalCells) relocations}

>argumentsPointsRelocation :: [(Super.Point,String)] ->  [(Super.Point,Super.Point)] -> [(Super.Point,String)]
>argumentsPointsRelocation arguments relocations =
> map
>   (\argument ->
>     (relocatePoint (fst argument) relocations,snd argument))
>   arguments

>pathPointsRelocation :: Path.Path -> [(Super.Point,Super.Point)] -> Path.Path
>pathPointsRelocation path@Path.SteppingStone{} relocations =
>  path{Path.point = relocatePoint (Path.point path) relocations,
>       Path.next  = pathPointsRelocation (Path.next path) relocations}
>pathPointsRelocation path@Path.PathDestination{} relocations =
>  path{Path.point = relocatePoint (Path.point path) relocations}

>patternPointsRelocation :: Pattern -> [(Super.Point,Super.Point)] -> Pattern
>patternPointsRelocation pattern relocations =
>  pattern{
>   patternPoint = relocatePoint (patternPoint pattern) relocations,
>   action = cellPointsRelocation (action pattern) relocations}

>mvarPointsRelocation :: Cell ->  [(Super.Point,Super.Point)] -> Cell
>mvarPointsRelocation originalCells relocations =
>  (cellPointsRelocation' originalCells relocations){
>   labelPoint = relocatePoint (labelPoint originalCells) relocations,
>   next = cellPointsRelocation (next originalCells) relocations
>   }


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
