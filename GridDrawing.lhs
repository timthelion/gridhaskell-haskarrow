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

Now we build gtk widgets for each displayCell.

>     cellForms <- sequence (map (cellForm editorObjects) displayCellsFilled);

We have to find the widget associated with the focused cell, such that upon redrawing everything, focus doesn't change.

>     focusedCellMaybe <- getObjectValue (focusedCellObject editorObjects);

>     focusedWidgetMaybe <- return (case focusedCellMaybe of
>       Just focusedCell ->
>        lookup (DisplayCell.displayCellPoint focusedCell) $ zip (map DisplayCell.displayCellPoint displayCellsFilled) (map snd cellForms)
>       Nothing ->
>        Nothing);

Now we make a list of all the points that appear in our table to actually be drawn on the screen.

>     places <- return (map 
>                 (\p -> Super.subtractPoint p 
>                       (grid_minimum_x,grid_minimum_y))
>          (map DisplayCell.displayCellPoint displayCellsFilled));

and then we zip those points with the widgets we want to attach to that table using the function attachCellForm to attach them.

>     sequence_ (zipWith (attachCellForm table) (map fst cellForms) places);

Now we add an event to draw the lines in our diagram.

>     onExpose table  $
>       (\_ -> do drawWin <- widgetGetDrawWindow table
>                 allocations <- (mapM widgetGetAllocation (map fst cellForms))
>                 renderWithDrawable drawWin (drawArrows (zip allocations displayCellsFilled))
>                 return False);

>     (case focusedWidgetMaybe of
>       Just focusedWidget -> widgetGrabFocus focusedWidget;
>       Nothing -> return ());
>     widgetShowAll canvas;
>     })}

>focusedPoint (Just dc) = (DisplayCell.displayCellPoint dc)
>focusedPoint Nothing = (0,0)

| 'cellForm' generates a gtk widget which represents the a cell in our grid.  The return type of this should be changed, so that we get something more usefull than a VBox.  This should return a data which includes all widgets who's contents might want to be updated or saved.

>cellForm :: GridEditorObjects -> DisplayCell.DisplayCell -> IO (Box,Widget)

>cellForm editorObjects dc = do
>       box <- hBoxNew False 0
>       widget <- cellFormFill (toBox box) False editorObjects dc
>       return ((toBox box), widget)

>cellFormFill :: Box -> Bool -> GridEditorObjects -> DisplayCell.DisplayCell -> IO Widget

>cellFormFill box edit editorObjects dc@(DisplayCell.DisplayCellCode cell@Cell.Action{}) = do
>        pure <- buttonNewWithLabel pureText
>        set pure [buttonRelief := ReliefHalf]
>        boxPackStart box pure PackNatural 0

>        widget <- cellFormFill' (DisplayCell.displayCellText dc) (toBox box) edit editorObjects dc

>        bind <- buttonNewWithLabel bindText
>        set bind [buttonRelief := ReliefHalf]
>        boxPackStart box bind PackNatural 0
>        return widget

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

>cellFormFill box edit editorObjects dc = do
> cellFormFill' (DisplayCell.displayCellText dc) box edit editorObjects dc

>cellFormFill' :: String -> Box -> Bool -> GridEditorObjects -> DisplayCell.DisplayCell -> IO Widget

>cellFormFill' text box edit editorObjects dc = do
>        if not edit
>        then do
>         enter <- buttonNewWithLabel text
>         if not $ (DisplayCell.displayCellBlank dc) || (DisplayCell.displayCellPath dc) 
>         then set enter [buttonRelief := ReliefHalf]
>         else set enter [buttonRelief := ReliefNone]
>         boxPackStart box enter PackGrow 0
>         onClicked enter (do {updateIO (editModeObject editorObjects) (editModeAction editorObjects dc box)})

>         enter `on` focusInEvent $ do 
>          { liftIO $ do {
>            update (focusedCellObject editorObjects)
>                   (\_->(Just dc));

>            updateIO (focusedRectangleObject editorObjects) (\_-> do
>                {widgetGetAllocation box});
>            return ();};
>            return False};
>         return (toWidget enter)
>        else do
>         entry <- entryNew
>         entrySetText entry text
>         boxPackStart box entry PackNatural 0
>         widgetGrabFocus entry;

>         entry `on` focusOutEvent $ do 
>             { liftIO $ do
>               updateIO (editModeObject editorObjects) 
>                        (editModeCancleCellEdit editorObjects dc box);
>               return False};

>         entry `on` keyPressEvent $ tryEvent $ do
>            key  <- eventKeyName

>            case key of
>             "Return" ->
>              liftIO $ do
>               text <- entryGetText entry
>               update (gridObject editorObjects) (GridAction.gridSetDisplayCellText dc text);

>             "Escape" -> 
>              liftIO $ do
>               updateIO (editModeObject editorObjects) 
>                        (editModeCancleCellEditEscape editorObjects dc box);

>         return (toWidget entry)

>editModeAction :: GridEditorObjects -> DisplayCell.DisplayCell -> Box -> EditMode -> IO EditMode
>editModeAction editorObjects dc vbox FreeMovement = do
>   postGUIAsync (do {
>   containerForeach vbox (containerRemove vbox);
>   cellFormFill vbox True editorObjects dc; 
>   widgetShowAll vbox;

>   })
>   return (EditCell dc)


>editModeAction editorObjects dc vbox (EditCell cellWe'reEditing) = do
>   containerForeach vbox (containerRemove vbox)
>   cellFormFill vbox False editorObjects dc
>   return (EditCell dc)

>editModeAction editorObjects dc vbox (MoveCell cellWe'reMoving) = do
>   case cellWe'reMoving of
>     DisplayCell.DisplayCellBlank{} -> return ()
>     otherwise ->
>              update (gridObject editorObjects)
>                     (\grid -> gridPointsRelocation grid [(DisplayCell.displayCellPoint cellWe'reMoving,DisplayCell.displayCellPoint dc)])
>   return (FreeMovement)


>editModeCancleCellEdit :: GridEditorObjects -> DisplayCell.DisplayCell -> Box -> EditMode -> IO EditMode
>editModeCancleCellEdit editorObjects dc vbox EditCell{} = do
>   postGUIAsync (do {
>   containerForeach vbox (containerRemove vbox);
>   cellFormFill vbox False editorObjects dc; 
>   widgetShowAll vbox;

>   })
>   return FreeMovement

>editModeCancleCellEdit _ _ _ FreeMovement = do
>  return FreeMovement

>editModeCancleCellEdit _ _ _ mode = do
>  return $ error "Help! Expected to be in EditCell mode, but instead I got:" ++ (show mode)
>  return FreeMovement

>editModeCancleCellEditEscape :: GridEditorObjects -> DisplayCell.DisplayCell -> Box -> EditMode -> IO EditMode
>editModeCancleCellEditEscape editorObjects dc vbox EditCell{} = do
>   postGUIAsync (do {
>   containerForeach vbox (containerRemove vbox);
>   focusedWidget <- cellFormFill vbox False editorObjects dc;
>   widgetShowAll vbox;
>   widgetGrabFocus focusedWidget
>   })

>   return FreeMovement


>attachCellForm :: Table -> Box -> (Int,Int) -> IO ()
>attachCellForm table form (x,y) = 
>              tableAttachDefaults table form x (x+1) y (y+1)
