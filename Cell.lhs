>module Cell where

>import qualified Path
>import qualified Super

Patterns are owned by switch statements.

>data Pattern = Pattern {patternPoint   :: Super.Point,
>                        pattern :: String, 
>                        action  :: Cell}
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

>        path      :: (Maybe Path.Path), 

| The next cell/action in on the grid in executable order.

>        next      :: Cell} 

|  Destination place where we take a value that was aquired a while ago and place it on the stack

>  | Destination {
>        point     :: Super.Point, 

| Origin place where the value was aquired/returned.

>        origin    :: Super.Point, 

| Path to any further uses of the value.

>        path      :: (Maybe Path.Path),
>        next      :: Cell}

Switch statement.  One example of how visual programming may be better than textual.

>  | Switch {
>        point     :: Super.Point,
>        patterns  :: [Pattern]}

| The place at which the two branches of a switch join.  This is the end of scope for any values that became public within either of these values.

>  | Join {
>        point     :: Super.Point,
>        next      :: Cell}

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
>cellText Switch{}            = "Switch"
>cellText Destination{}       = "->"
>cellText Join{}              = "Join" 
>cellText Jump{}              = "Jump"
>cellText Fork{}              = "Fork"
>cellText cell@NewEmptyMVar{} = "newEmptyMVar" ++ (mvar cell)
>cellText cell@PutMVar{}      = "putMVar" ++ (mvar cell)
>cellText cell@TakeMVar{}     = "takeMVar" ++ (mvar cell)
>cellText End{}               = "End"
>cellText Exit{}              = "Exit"

We use this to build a list of cells for display on the screen.

>cellNext :: Cell -> [Cell]
>cellNext cell@Jump{}     = []
>cellNext cell@End{}      = []
>cellNext cell@Exit{}     = []
>cellNext cell@Fork{}     = newThreads cell
>cellNext cell@Switch{}   = map action (patterns cell)
>cellNext cell            = [next cell]

| Return the Points of the arguments passed to the 'Start'.

>argumentsPoints :: [(Super.Point,String)] -> [Super.Point]
>argumentsPoints arguments = map fst arguments
