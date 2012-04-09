>module GridDrawing (buildTable) where

>import Graphics.UI.Gtk
>import Data.Tuple
>import Data.List
>import Control.Concurrent


>import Grid
>import GridExtremes
>import qualified Super
>import qualified DisplayCell
>import qualified Cell
>import qualified Path
>import ThreadObject
>import ArrowDrawing

| Build the actual gtk table which shows our grid.

>buildTable :: ScrolledWindow -> ThreadObject Grid -> ThreadObject (Maybe DisplayCell.DisplayCell) -> Grid -> IO Button
>buildTable scrwin gridObject focusedCellObject mygrid = do{

>     focusedWidgetMVar <- newEmptyMVar;

>     focusedCellMVar <- newEmptyMVar;
>     updateIO focusedCellObject (\focusedCell -> do{
>      putMVar focusedCellMVar focusedCell;
>      return focusedCell;
>     });

>     focusedCell <- takeMVar focusedCellMVar;

This is a list of cells to be displayed.

>     displayCells <- return (DisplayCell.displayCellList mygrid);

We use these extremes to find how big a table to make.

>     grid_maximum_x <- return (maximum_x displayCells);
>     grid_maximum_y <- return (maximum_y displayCells);
>     grid_minimum_x <- return (minimum_x displayCells);
>     grid_minimum_y <- return (minimum_y displayCells);

Y before X because tableNew goes Rows Collumns order.

>     table <- tableNew (lengthBetween grid_minimum_y grid_maximum_y) (lengthBetween grid_minimum_x grid_maximum_x) True;
>     scrolledWindowAddWithViewport scrwin table;

A "filled in" list of display cells, to make a complete grid, including blank cells.

>     displayCellsFilled <- return (DisplayCell.filledinDisplayCellList displayCells (grid_minimum_x,grid_minimum_y) (grid_maximum_x,grid_maximum_y));

Now we build gtk widgets for each displayCell.

>     cellVBoxList <- sequence (map (cellForm gridObject focusedCellObject (focusedPoint focusedCell) focusedWidgetMVar) displayCellsFilled);

Now we make a list of all the points that appear in our table to actually be drawn on the screen.

>     places <- return (map 
>                 (\p -> Super.subtractPoint p 
>                       (grid_minimum_x,grid_minimum_y))
>          (map DisplayCell.displayCellPoint displayCellsFilled));

and then we zip those points with the widgets we want to attach to that table using the function attachCellForm to attach them.

>     sequence_ (zipWith (attachCellForm table) cellVBoxList places);

Now we draw the lines in our diagram.

>     onExpose table  $
>       (\x -> do drawWin <- widgetGetDrawWindow table
>                 allocations <- (mapM widgetGetAllocation  cellVBoxList)
>                 renderWithDrawable drawWin (drawArrows (zip allocations displayCellsFilled))
>                 return False);

>     focusedWidget <- takeMVar focusedWidgetMVar;
>     return focusedWidget;}

>focusedPoint (Just dc) = (DisplayCell.displayCellPoint dc)
>focusedPoint Nothing = (0,0)

| 'cellForm' generates a gtk widget which represents the a cell in our grid.  The return type of this should be changed, so that we get something more usefull than a VBox.  This should return a data which includes all widgets who's contents might want to be updated or saved.

>cellForm :: ThreadObject Grid -> ThreadObject (Maybe DisplayCell.DisplayCell) -> Super.Point -> MVar Button -> DisplayCell.DisplayCell -> IO VBox

>cellForm gridObject focusedCellObject focusedPoint focusedWidgetMVar dc@(DisplayCell.DisplayCellCode cell@Cell.Action{}) = do
>        outerBox <- vBoxNew False 0
>        box <- hBoxNew False 0

>        pure <- buttonNewWithLabel pureText
>        set pure [buttonRelief := ReliefHalf]
>        boxPackStart box pure PackNatural 0

>        bind <- buttonNewWithLabel bindText
>        set bind [buttonRelief := ReliefHalf]
>        boxPackStart box bind PackNatural 0

>        enter <- buttonNewWithLabel (Cell.cellText cell)
>        set enter [buttonRelief := ReliefHalf]
>        boxPackStart box enter PackGrow 0

>        if Cell.point cell == focusedPoint
>        then putMVar focusedWidgetMVar enter
>        else return ()

>        boxPackStart outerBox box PackGrow 0
>        return outerBox

>           where   bindText :: String 
>                   bindText =
>                    case cell of

Push Pull

>                     (Cell.Action _ _ _ True True _ _) -> "^>>=" 

Push Don't Pull

>                     (Cell.Action _ _ _ True False _ _) -> "^>>" 

Don't Push Don't Pull

>                     (Cell.Action _ _ _ False False _ _) -> ">>" 

Don't Push Pull

>                     (Cell.Action _ _ _ False True _ _) -> ">>=" 

>                   pureText :: String
>                   pureText =
>                    case cell of

Pure

>                     (Cell.Action _ _ True _ _ _ _) -> "=" 

Not pure

>                     (Cell.Action _ _ False _ _ _ _) -> "!" 

>cellForm gridObject focusedCellObject  focusedPoint focusedWidgetMVar dc@(DisplayCell.DisplayCellCode cell) = do
>        box <- vBoxNew False 0
>        enter <- buttonNewWithLabel (Cell.cellText cell)
>        set enter [buttonRelief := ReliefHalf]
>        boxPackStart box enter PackGrow 0

>        if Cell.point cell == focusedPoint
>        then putMVar focusedWidgetMVar enter
>        else return ()

>        return box



>cellForm gridObject focusedCellObject  focusedPoint focusedWidgetMVar (DisplayCell.DisplayCellComment (point,text)) = do
>        box <- vBoxNew False 0
>        enter <- buttonNewWithLabel text

>        if point == focusedPoint
>        then putMVar focusedWidgetMVar enter
>        else return ()


>        set enter [buttonRelief := ReliefNone]
>        boxPackStart box enter PackGrow 0
>        return box        

>cellForm gridObject focusedCellObject  focusedPoint focusedWidgetMVar  (DisplayCell.DisplayCellPath  path) = do
>        box <- vBoxNew False 0
>        enter <- buttonNew
>        set enter [buttonRelief := ReliefNone]

>        if Path.point path == focusedPoint
>        then putMVar focusedWidgetMVar enter
>        else return ()

>        boxPackStart box enter PackGrow 0
>        return box        

>cellForm gridObject focusedCellObject focusedPoint focusedWidgetMVar  (DisplayCell.DisplayCellArgument point argument) = do
>        box <- vBoxNew False 0
>        enter <- buttonNewWithLabel ("Argument")
>        set enter [buttonRelief := ReliefHalf]
>        boxPackStart box enter PackGrow 0

>        if point == focusedPoint
>        then putMVar focusedWidgetMVar enter
>        else return ()

>        return box        
       
>cellForm gridObject focusedCellObject  focusedPoint focusedWidgetMVar (DisplayCell.DisplayCellPattern pattern) = do
>        box <- vBoxNew False 0
>        enter <- buttonNewWithLabel (Cell.pattern pattern)
>        set enter [buttonRelief := ReliefHalf]
>        boxPackStart box enter PackGrow 0

>        if Cell.patternPoint pattern == focusedPoint
>        then putMVar focusedWidgetMVar enter
>        else return ()

>        return box        

>cellForm gridObject focusedCellObject focusedPoint focusedWidgetMVar (DisplayCell.DisplayCellMVarLabel point mvar) = do
>        box <- vBoxNew False 0
>        enter <- buttonNewWithLabel mvar
>        set enter [buttonRelief := ReliefHalf]
>        boxPackStart box enter PackGrow 0

>        if point == focusedPoint
>        then putMVar focusedWidgetMVar enter
>        else return ()

>        return box        

>cellForm gridObject focusedCellObject focusedPoint focusedWidgetMVar dc@(DisplayCell.DisplayCellBlank point) = do
>        box <- vBoxNew False 0
>        enter <- buttonNew
>        set enter [buttonRelief := ReliefNone]

>        if point == focusedPoint
>        then putMVar focusedWidgetMVar enter
>        else return ()

>        boxPackStart box enter PackGrow 0
>        onClicked enter (do {update gridObject (gridPlusComment point "Hello!")})
>        onFocus box (\_ -> do {update focusedCellObject (\_ -> Just dc);
>                         return False})

>        return box

>attachCellForm :: Table -> VBox -> (Int,Int) -> IO ()
>attachCellForm table form (x,y) = 
>              tableAttachDefaults table form x (x+1) y (y+1)
