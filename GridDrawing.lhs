>module GridDrawing (buildTable) where

>import Graphics.UI.Gtk
>import Graphics.Rendering.Cairo
>import Data.Tuple

>import Grid
>import GridExtremes
>import qualified Super
>import qualified DisplayCell
>import qualified Cell

| Build the actual gtk table which shows our grid.

>buildTable :: ScrolledWindow -> Grid -> IO (Table, [VBox])
>buildTable scrwin mygrid = do

This is a list of cells to be displayed.

>     displayCells <- return (DisplayCell.displayCellList mygrid)

We use these extremes to find how big a table to make.

>     grid_maximum_x <- return (maximum_x displayCells)
>     grid_maximum_y <- return (maximum_y displayCells)
>     grid_minimum_x <- return (minimum_x displayCells)
>     grid_minimum_y <- return (minimum_y displayCells)

Y before X because tableNew goes Rows Collumns order.

>     table <- tableNew (lengthBetween grid_minimum_y grid_maximum_y) (lengthBetween grid_minimum_x grid_maximum_x) True
>     scrolledWindowAddWithViewport scrwin table

Now we draw the lines.

>     onExpose table  $
>            (\x -> do drawWin <- widgetGetDrawWindow table
>                      renderWithDrawable drawWin drawArrows
>                      return False)


A "filled in" list of display cells, to make a complete grid, including blank cells.

>     displayCellsFilled <- return (DisplayCell.filledinDisplayCellList displayCells (grid_minimum_x,grid_minimum_y) (grid_maximum_x,grid_maximum_y))
>     print "Raw display cells:"
>     print (map DisplayCell.displayCellPoint displayCells)
>     print "Final display cells:"
>     print (map DisplayCell.displayCellPoint displayCellsFilled)

Now we build gtk widgets for each displayCell.
     
>     cellFormList <- sequence (map cellForm displayCellsFilled)

Now we make a list of all the points that appear in our table to actually be drawn on the screen.

>     let places = map (\p -> Super.subtractPoint p (grid_minimum_x,grid_minimum_y))
>          (map DisplayCell.displayCellPoint displayCellsFilled)

and then we zip those points with the widgets we want to attach to that table using the function attachCellForm to attach them.

>     sequence_ (zipWith (attachCellForm table) cellFormList places)

>     return (table, cellFormList)


| 'drawArrows' or lines between cells. 

>drawArrows :: Render ()
>drawArrows = do
>    setSourceRGB 0 0 0
>    setLineWidth 5

>    moveTo 110 0
>    lineTo 100 100
>    lineTo 100 200
>    closePath

>    stroke


| 'cellForm' generates a gtk widget which represents the a cell in our grid.  The return type of this should be changed, so that we get something more usefull than a VBox.  This should return a data which includes all widgets who's contents might want to be updated or saved.

>cellForm :: DisplayCell.DisplayCell -> IO VBox
>cellForm dc@(DisplayCell.DisplayCellCode cell) = do
>        info <- labelNew (Just ((show (DisplayCell.displayCellPoint dc)) ++"  Code"))
>        contents <- entryNew
>        entrySetText contents (Cell.cellText cell)
>        box <- vBoxNew False 0
>        boxPackStart box info PackNatural 0
>        boxPackStart box contents PackNatural 0
>        return box

>cellForm (DisplayCell.DisplayCellComment (point,text)) = do
>        info <- labelNew (Just ((show point)++" Comment"))
>        contents <- entryNew
>        entrySetText contents text
>        box <- vBoxNew False 0
>        boxPackStart box info PackNatural 0
>        boxPackStart box contents PackNatural 0
>        return box

>cellForm (DisplayCell.DisplayCellPath  point) = do
>        info <- labelNew (Just ((show point)++" Path"))
>        box <- vBoxNew False 0
>        boxPackStart box info PackNatural 0
>        return box        

>cellForm (DisplayCell.DisplayCellArgument point argument) = do
>        info <- labelNew (Just ((show point)++" Argument"))
>        contents <- entryNew
>        entrySetText contents argument
>        box <- vBoxNew False 0
>        boxPackStart box info PackNatural 0
>        boxPackStart box contents PackNatural 0
>        return box
       
>cellForm (DisplayCell.DisplayCellPattern point pattern) = do
>        info <- labelNew (Just ((show point)++" Pattern"))
>        contents <- entryNew
>        entrySetText contents pattern
>        box <- vBoxNew False 0
>        boxPackStart box info PackNatural 0
>        boxPackStart box contents PackNatural 0
>        return box

>cellForm (DisplayCell.DisplayCellMVarLabel point mvar) = do
>        info <- labelNew (Just ((show point)++" MVarLabel"))
>        contents <- entryNew
>        entrySetText contents mvar
>        box <- vBoxNew False 0
>        boxPackStart box info PackNatural 0
>        boxPackStart box contents PackNatural 0
>        return box


>cellForm (DisplayCell.DisplayCellBlank point) = do
>        box <- vBoxNew False 0
>        enter <- buttonNew
>        boxPackStart box enter PackNatural 0

Now we draw the lines.

<        onExpose box  $
<            (\x -> do drawWin <- widgetGetDrawWindow box
<                      renderWithDrawable drawWin drawArrows
<                      return False)

>        return box


<        info <- labelNew (Just ((show point)++" Blank"))
<        contents <- entryNew
<        box <- vBoxNew False 0
<        boxPackStart box info PackNatural 0
<        boxPackStart box contents PackNatural 0
<        return box

>attachCellForm :: Table -> VBox -> (Int,Int) -> IO ()
>attachCellForm table form (x,y) = 
>              tableAttachDefaults table form x (x+1) y (y+1)
