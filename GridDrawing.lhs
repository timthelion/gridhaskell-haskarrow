>module GridDrawing (buildTable) where

>import Graphics.UI.Gtk
>import Graphics.Rendering.Cairo
>import Data.Tuple
>import Data.List

>import Grid
>import GridExtremes
>import qualified Super
>import qualified DisplayCell
>import qualified Cell
>import qualified Path

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

A "filled in" list of display cells, to make a complete grid, including blank cells.

>     displayCellsFilled <- return (DisplayCell.filledinDisplayCellList displayCells (grid_minimum_x,grid_minimum_y) (grid_maximum_x,grid_maximum_y))

<     print "Raw display cells:"
<     print (map DisplayCell.displayCellPoint displayCells)
<     print "Final display cells:"
<     print (map DisplayCell.displayCellPoint displayCellsFilled)

Now we build gtk widgets for each displayCell.
     
>     cellVBoxList <- sequence (map cellForm displayCellsFilled)

Now we make a list of all the points that appear in our table to actually be drawn on the screen.

>     let places = map 
>                 (\p -> Super.subtractPoint p 
>                       (grid_minimum_x,grid_minimum_y))
>          (map DisplayCell.displayCellPoint displayCellsFilled)

and then we zip those points with the widgets we want to attach to that table using the function attachCellForm to attach them.

>     sequence_ (zipWith (attachCellForm table) cellVBoxList places)

Now we draw the lines in our diagram.

>     onExpose table  $
>       (\x -> do drawWin <- widgetGetDrawWindow table
>                 allocations <- (mapM widgetGetAllocation  cellVBoxList)
>                 renderWithDrawable drawWin (drawArrows (zip allocations displayCellsFilled))
>                 return False)

>     return (table, cellVBoxList)



Gather a list of arrows that look like [(Rectangle, Rectangle)].  Then draw the lines.

| 'drawArrows' or lines between cells. 

>drawArrows :: [(Rectangle,DisplayCell.DisplayCell)] -> Render ()
>drawArrows cells = do

We go through the list both forwards and backwards, completing arrows.

>      (arrows',unfinished_arrows) <- return (arrowsOf cells [])
>      arrows <- return (arrows' ++ 
>                        (fst (arrowsOf (reverse cells) unfinished_arrows)))
>      mapM drawArrow arrows
>      return ()

| The 'arrowsOf' or between a list of cells.

 The first argument is the list of the display cells, and their geometric locations on the screen.  The seccond argument is the list of unfinished arrows.  When we come across a Fork, or Switch, or Code element we add a Rectangle and Point.  The Point is relative to the coordinate system of the Grid and not of the pixels on the screen :) .  We keep the tuple in the list of unfinished arrows untill we reach the Point of the other endo of the arrow.  Then we finish the arrow and add it to our output.

>arrowsOf :: [(Rectangle,DisplayCell.DisplayCell)] -> [(Rectangle,Super.Point)] -> ([(Rectangle,Rectangle)],[(Rectangle,Super.Point)])

Path Arrows

>arrowsOf ((r, (DisplayCell.DisplayCellPath path)):cells) unfinished_arrows = 
> arrowsOf' r (Path.point path) unfinished_arrows [(Path.point (Path.next path))] cells


The arrows going down to the branches of a switch.

>arrowsOf ((r, (DisplayCell.DisplayCellCode cell@Cell.Switch{})):cells) unfinished_arrows = 
> arrowsOf' r (Cell.point cell) unfinished_arrows (map Cell.patternPoint (Cell.patterns cell)) cells

>arrowsOf ((r,(DisplayCell.DisplayCellPattern pattern)):cells) unfinished_arrows =
> arrowsOf' r (Cell.patternPoint pattern) unfinished_arrows [Cell.point (Cell.action pattern)] cells

The arrows comming from any other type of cell.
    
>arrowsOf ((r, (DisplayCell.DisplayCellCode cell)):cells) unfinished_arrows =
>  arrowsOf' r (Cell.point cell) unfinished_arrows ((map Cell.point (Cell.cellNext cell))++path_points) cells
>   where path_points = 
>          if
>           case cell of

Types of cells which have paths:

>             Cell.Action{}      -> True
>             Cell.Destination{} -> True
>             Cell.Jump{}        -> True
>             _                  -> False
>          then maybe [] (\p -> [Path.point p]) (Cell.path cell)
>          else []

>arrowsOf (_:cells) unfinished_arrows =
>   arrowsOf cells unfinished_arrows

>arrowsOf [] unfinished_arrows = ([],unfinished_arrows)

>arrowsOf' :: Rectangle -> Super.Point -> [(Rectangle,Super.Point)] -> [Super.Point] -> [(Rectangle,DisplayCell.DisplayCell)] -> ([(Rectangle,Rectangle)],[(Rectangle,Super.Point)])
>arrowsOf' r p unfinished_arrows new_arrows cells =
>   (finished_arrows ++ finished_arrows',
>   unfinished_arrows')

>   where
>    (finished_arrows', unfinished_arrows') =
>      arrowsOf cells still_unfinished_arrows
 
>    (finished_arrows, still_unfinished_arrows) = 
>      finished_and_still_unfinished_arrows r p unfinished_arrows new_arrows


| 'finished_and_still_unfinished_arrows' given the Rectangle of the current Cell, the Point of the current cell, the list of previously unfinished arrows and the list of points which come next(this defines the newly unfinished arrows.)

Returns ([finished_arrows],[still_unfinished_arrows])

>finished_and_still_unfinished_arrows :: Rectangle -> Super.Point -> [(Rectangle,Super.Point)] -> [Super.Point] -> ([(Rectangle, Rectangle)] , [(Rectangle,Super.Point)])
>finished_and_still_unfinished_arrows r p unfinished_arrows new_arrow_destinations =
>   (zip (map fst matches) (repeat r),

Still unfinished arrows.

>   zip (repeat r) new_arrow_destinations ++
>   still_unmatched)
>   where (matches, still_unmatched) = partition (\unfinished_arrow -> snd unfinished_arrow == p) unfinished_arrows 


>drawArrow :: (Rectangle,Rectangle) -> Render ()
>drawArrow ((Rectangle x y w h), (Rectangle x1 y1 w1 h1)) = do
>    setSourceRGB 0 0 0
>    setLineWidth 2

>    moveTo (fromIntegral (x+(div w 2))) (fromIntegral (y+(div h 2)))
>    lineTo (fromIntegral (x1+(div w1 2))) (fromIntegral (y1+(div h1 2)))

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

>cellForm (DisplayCell.DisplayCellPath  path) = do
>        info <- labelNew (Just ((show (Path.point path))++" Path"))
>        box <- vBoxNew False 0
>        enter <- buttonNew
>        set enter [buttonRelief := ReliefNone]
>        boxPackStart box enter PackGrow 0
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
       
>cellForm (DisplayCell.DisplayCellPattern pattern) = do
>        info <- labelNew (Just ((show (Cell.patternPoint pattern))++" Pattern"))
>        contents <- entryNew
>        entrySetText contents (Cell.pattern pattern)
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
>        set enter [buttonRelief := ReliefNone]
>        boxPackStart box enter PackGrow 0

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
