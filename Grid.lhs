GPLv3 (c) Timothy Hobbs

>module Grid where
>import qualified Cell
>import Super

>type Comment = (Point,String)

>commentPoint :: Comment -> Point
>commentPoint (p,_) = p

>commentText :: Comment -> String
>commentText (_,t) = t

| Grid is our file type.  We read it and show it to load and unload the grid haskell files.

>data Grid = Grid{

| The 'message' is just to tell the user that this is not to be edited by hand after we save the whole thing to a file. 
 
>        message           :: String,
>        gridName          :: String,
>        gridLicence       :: String,
>        gridImports       :: [String],
>        gridPureFunctions :: [(Prototype, String)],
>        gridComments      :: [Comment],
>        gridCells         :: Cell.Cell,
>        gridLooseCells    :: [Cell.Cell]}
> deriving (Read, Show)
  

>emptyGrid :: Grid
>emptyGrid = Grid {
>       message = "This is a computer generated file.  Do not edit.",

>       gridName          = "",
>       gridLicence       = "",
>       gridImports       = [""],
>       gridPureFunctions = [("","")],
>       gridComments      = [],
>       gridCells         = Cell.End (0,0),
>       gridLooseCells    = []}

>gridPutComment :: Point -> String -> Grid -> Grid
>gridPutComment point comment (Grid message gridName gridLicence gridImports gridPureFunctions gridComments gridCells looseCells) =
>  Grid message gridName gridLicence gridImports gridPureFunctions ((point,comment):(filter (\comment -> not $ point == (fst comment)) gridComments)) gridCells looseCells

>gridPutCell :: Cell.Cell -> Grid -> Grid
>gridPutCell cell grid = 
>  grid{gridCells=fst (Cell.cellPutCell cell gridCells')}
>  where gridCells' = gridCells grid
