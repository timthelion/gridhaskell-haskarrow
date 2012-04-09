GPLv3 (c) Timothy Hobbs

>module Grid where
>import qualified Cell
>import Super

>type Comment = (Point,String)

>commentPoint :: Comment -> Point
>commentPoint (p,_) = p

| Grid is our file type.  We read it and show it to load and unload the grid haskell files.

>data Grid = Grid{

| The 'message' is just to tell the user that this is not to be edited by hand after we save the whole thing to a file. 
 
>        message           :: String,
>        gridName          :: String,
>        gridLicence       :: String,
>        gridImports       :: [String],
>        gridPureFunctions :: [(Prototype, String)],
>        gridComments      :: [Comment],
>        gridCells         :: Cell.Cell}
> deriving (Read, Show)
  

>emptyGrid :: Grid
>emptyGrid = Grid {
>       message = "This is a computer generated file.  Do not edit.",

>       gridName          = "",
>       gridLicence       = "",
>       gridImports       = [""],
>       gridPureFunctions = [("","")],
>       gridComments      = [],
>       gridCells         = Cell.End (0,0)}

>gridPlusComment :: Point -> String -> Grid -> Grid
>gridPlusComment point comment (Grid message gridName gridLicence gridImports gridPureFunctions gridComments gridCells) =
>  Grid message gridName gridLicence gridImports gridPureFunctions ((point,comment):gridComments) gridCells
