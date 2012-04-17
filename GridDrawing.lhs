>module GridDrawing (drawGrid) where

>import Graphics.UI.Gtk
>import Data.Tuple
>import Data.List
>import Control.Concurrent
>import Control.Monad.IO.Class

>import Grid
>import GridExtremes
>import qualified Super
>import qualified DisplayCell
>import qualified Cell
>import qualified Path
>import ThreadObject
>import ArrowDrawing
>import EditModes
>import GridEditorObjects
>import GridAction

| Draw the actual gtk table which shows our grid.

>drawGrid :: GridEditorObjects -> Grid -> IO ()
>drawGrid  editorObjects mygrid = do{

This is a list of cells to be displayed.

>     displayCells <- return (DisplayCell.displayCellList mygrid);

We use these extremes to find how big a table to make.

>     grid_maximum_x <- return $ (maximum_x displayCells) + 1;
>     grid_maximum_y <- return $ (maximum_y displayCells) + 1;
>     grid_minimum_x <- return $ (minimum_x displayCells) - 1;
>     grid_minimum_y <- return $ (minimum_y displayCells) - 1;


Y before X because tableNew goes Rows Collumns order.

>     postGUIAsync (do {
>     table <- tableNew (lengthBetween grid_minimum_y grid_maximum_y) (lengthBetween grid_minimum_x grid_maximum_x) True;
>     canvas <- getObjectValue (canvasObject editorObjects);
>     scrolledWindowAddWithViewport canvas table;

A "filled in" list of display cells, to make a complete grid, including blank cells.

>     displayCellsFilled <- return (DisplayCell.filledinDisplayCellList displayCells (grid_minimum_x,grid_minimum_y) (grid_maximum_x,grid_maximum_y));

We have to find the widget associated with the focused cell, such that upon redrawing everything, focus doesn't change.

>     focusedWidgetMVar <- newEmptyMVar;
>     focusedCell <- getObjectValue (focusedCellObject editorObjects);

Now we build gtk widgets for each displayCell.

>     cellBoxList <- sequence (map (cellForm editorObjects (focusedPoint focusedCell) focusedWidgetMVar) displayCellsFilled);

Now we make a list of all the points that appear in our table to actually be drawn on the screen.

>     places <- return (map 
>                 (\p -> Super.subtractPoint p 
>                       (grid_minimum_x,grid_minimum_y))
>          (map DisplayCell.displayCellPoint displayCellsFilled));

and then we zip those points with the widgets we want to attach to that table using the function attachCellForm to attach them.

>     sequence_ (zipWith (attachCellForm table) cellBoxList places);

Now we add an event to draw the lines in our diagram.

>     onExpose table  $
>       (\x -> do drawWin <- widgetGetDrawWindow table
>                 allocations <- (mapM widgetGetAllocation  cellBoxList)
>                 renderWithDrawable drawWin (drawArrows (zip allocations displayCellsFilled))
>                 return False);

>     forkIO $ putMVar focusedWidgetMVar Nothing;
>     focusedWidgetMaybe <- takeMVar focusedWidgetMVar;
>     case focusedWidgetMaybe of{
>       Just focusedWidget -> widgetGrabFocus focusedWidget;
>       Nothing -> error "We expected at least one widget to be focused!";};
>     widgetShowAll canvas;
>     })}

>focusedPoint (Just dc) = (DisplayCell.displayCellPoint dc)
>focusedPoint Nothing = (0,0)

| 'cellForm' generates a gtk widget which represents the a cell in our grid.  The return type of this should be changed, so that we get something more usefull than a VBox.  This should return a data which includes all widgets who's contents might want to be updated or saved.

>cellForm :: GridEditorObjects -> Super.Point -> MVar (Maybe Widget )-> DisplayCell.DisplayCell -> IO Box

>cellForm editorObjects focusedPoint focusedWidgetMVar dc = do
>       box <- hBoxNew False 0
>       cellFormFill (toBox box) False editorObjects focusedPoint focusedWidgetMVar dc
>       return (toBox box)

>cellFormFill :: Box -> Bool -> GridEditorObjects -> Super.Point -> MVar (Maybe Widget) -> DisplayCell.DisplayCell -> IO ()

>cellFormFill box edit editorObjects focusedPoint focusedWidgetMVar dc@(DisplayCell.DisplayCellCode cell@Cell.Action{}) = do
>        pure <- buttonNewWithLabel pureText
>        set pure [buttonRelief := ReliefHalf]
>        boxPackStart box pure PackNatural 0

>        cellFormFill' (DisplayCell.displayCellText dc) (toBox box) edit editorObjects focusedPoint focusedWidgetMVar dc

>        bind <- buttonNewWithLabel bindText
>        set bind [buttonRelief := ReliefHalf]
>        boxPackStart box bind PackNatural 0
>        return ()

>           where   bindText :: String 
>                   bindText =
>                    case cell of

Push Pull

>                     (Cell.Action _ _ _ True True _ _ _) -> "^>>=" 

Push Don't Pull

>                     (Cell.Action _ _ _ True False _ _ _) -> "^>>" 

Don't Push Don't Pull

>                     (Cell.Action _ _ _ False False _ _ _) -> ">>" 

Don't Push Pull

>                     (Cell.Action _ _ _ False True _ _ _) -> ">>=" 

>                   pureText :: String
>                   pureText =
>                    case cell of

Pure

>                     (Cell.Action _ _ True _ _ _ _ _) -> "=" 

Not pure

>                     (Cell.Action _ _ False _ _ _ _ _) -> "IO" 

>cellFormFill box edit editorObjects focusedPoint focusedWidgetMVar dc = do
> cellFormFill' (DisplayCell.displayCellText dc) box edit editorObjects focusedPoint focusedWidgetMVar dc

>cellFormFill' :: String -> Box -> Bool -> GridEditorObjects -> Super.Point -> MVar (Maybe Widget) -> DisplayCell.DisplayCell -> IO ()

>cellFormFill' text box edit editorObjects focusedPoint focusedWidgetMVar dc = do
>        if not edit
>        then do
>         enter <- buttonNewWithLabel text
>         if not $ (DisplayCell.displayCellBlank dc) || (DisplayCell.displayCellPath dc) 
>         then set enter [buttonRelief := ReliefHalf]
>         else set enter [buttonRelief := ReliefNone]
>         boxPackStart box enter PackGrow 0
>         onClicked enter (do {updateIO (editModeObject editorObjects) (editModeAction editorObjects focusedPoint focusedWidgetMVar dc box)})

>         enter `on` focusInEvent $ do 
>          { liftIO $ 
>            update (focusedCellObject editorObjects)
>                   (\_->(Just dc));
>            return False};

>         if (DisplayCell.displayCellPoint dc) == focusedPoint
>         then putMVar focusedWidgetMVar (Just (toWidget enter))
>         else return ()
>        else do
>         entry <- entryNew
>         entrySetText entry text
>         boxPackStart box entry PackNatural 0
>         widgetGrabFocus entry;

>         entry `on` focusOutEvent $ do 
>             { liftIO $ 
>               updateIO (editModeObject editorObjects) 
>                        (editModeCancleCellEdit editorObjects focusedPoint focusedWidgetMVar dc box);
>               return False};
>         entry `on` keyPressEvent $ tryEvent $ do
>             "Return" <- eventKeyName
>             liftIO $ do
>               text <- entryGetText entry
>               update (gridObject editorObjects) (GridAction.gridSetDisplayCellText dc text);
>         return ()  

>        return ()

>editModeAction :: GridEditorObjects -> Super.Point -> MVar (Maybe Widget) -> DisplayCell.DisplayCell -> Box -> EditMode -> IO EditMode
>editModeAction editorObjects focusedPoint focusedWidgetMVar dc vbox FreeMovement = do
>   postGUIAsync (do {
>   containerForeach vbox (containerRemove vbox);
>   cellFormFill vbox True editorObjects focusedPoint focusedWidgetMVar dc; 
>   widgetShowAll vbox;

>   })
>   return (EditCell dc)


>editModeAction editorObjects focusedPoint focusedWidgetMVar dc vbox (EditCell cellWe'reEditing) = do
>   containerForeach vbox (containerRemove vbox)
>   cellFormFill vbox False editorObjects focusedPoint focusedWidgetMVar dc
>   return (EditCell dc)

>editModeAction editorObjects focusedPoint focusedWidgetMVar dc vbox (MoveCell cellWe'reMoving) = do
>   case cellWe'reMoving of
>     DisplayCell.DisplayCellBlank{} -> return ()
>     otherwise ->
>              update (gridObject editorObjects)
>                     (\grid -> gridPointsRelocation grid [(DisplayCell.displayCellPoint cellWe'reMoving,DisplayCell.displayCellPoint dc)])
>   return (FreeMovement)


>editModeCancleCellEdit :: GridEditorObjects -> Super.Point -> MVar (Maybe Widget) -> DisplayCell.DisplayCell -> Box -> EditMode -> IO EditMode
>editModeCancleCellEdit editorObjects focusedPoint focusedWidgetMVar dc vbox EditCell{} = do
>   postGUIAsync (do {
>   containerForeach vbox (containerRemove vbox);
>   cellFormFill vbox False editorObjects focusedPoint focusedWidgetMVar dc; 
>   widgetShowAll vbox;

>   })
>   return FreeMovement


>attachCellForm :: Table -> Box -> (Int,Int) -> IO ()
>attachCellForm table form (x,y) = 
>              tableAttachDefaults table form x (x+1) y (y+1)
