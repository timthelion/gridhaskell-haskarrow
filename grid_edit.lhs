>module Main where

Libraries external to application.

>import Graphics.UI.Gtk hiding (cellText)
>import System.Environment
>import Data.List
>import Data.Maybe
>import Control.Concurrent
>import System.Posix.Unistd
>import System.Exit
>import EditModes
>import Control.Monad.IO.Class

Internal libraries.

Our top level data type/file type.

>import Grid

Grid haskell comands are defined by the type Cell in the module Cell.

These Cells are represented on the screen, but there are also other things that we display on the screen.  For example, comments.  So therefore we have a seccond type to encapsulate both.

>import DisplayCell

And then we need some code to actually draw these DisplayCells on the screen.

>import GridDrawing

Once we have a window, we need to set up some generic events for the whole window:

>import GridEditWindowEvents

And keep track of a few mutable objects, such as our grid...

>import GridEditorObjects

This type of mutable object, is defined by the module:

>import ThreadObject

There are also some Super generic types that belong no where else.  Like type Point = (Int, Int)...

>import qualified Super

For reading and writing grid haskell files.

>import GridHaskellFile

>main :: IO ()
>main = do
>     (mygrid, filePath) <- loadGrid

>     initGUI

>     gridEditWindowEvent <- newEmptyMVar

Special MVar used to quit.  Put a value here when you want to terminate.

>     exit                <- newEmptyMVar


>     gridObject'             <- threadObject
>     canvasObject'           <- threadObject 
>     focusedCellObject'      <- threadObject
>     focusedRectangleObject' <- threadObject
>     editModeObject'         <- threadObject
>     fileObject'             <- threadObject
>     filePathObject'         <- threadObject
>     reFocusNeededObject'    <- threadObject

>     editorObjects <- return (GridEditorObjects gridObject' canvasObject' editModeObject' focusedCellObject' focusedRectangleObject' reFocusNeededObject' fileObject' filePathObject')

>     myWidgets <- loadWidgets gridEditWindowEvent editorObjects
>     (window,_,canvas,scrwinContainer,_,cellInfo,modeInfo) <- return myWidgets

>     widgetShowAll window
>     forkOS mainGUI


Note.  It would seem that order is important here as the sync function for gridObject references scrwinObject.   Luckly it is not.  When syncGridwithScrwin calls updateIO scrwinObject, updateIO will wait till the scrwinObject is initialized before continuing.

>     objectInit (editModeObject editorObjects) FreeMovement noSyncOnGet (syncEditModeWithLabel modeInfo) False
>     objectInit (gridObject editorObjects) mygrid noSyncOnGet (syncGridwithCanvas editorObjects) False
>     objectInit (canvasObject editorObjects) canvas noSyncOnGet noSyncOnPut False
>     objectInit (focusedCellObject editorObjects) Nothing noSyncOnGet (syncFocusedCellWithLabel cellInfo) True
>     objectInit (focusedRectangleObject editorObjects) (Rectangle 0 0 0 0) noSyncOnGet (syncFocusedRectangleWithScrolledWindow editorObjects) True
>     objectInit (reFocusNeededObject editorObjects) False noSyncOnGet noSyncOnPut True
>     objectInit (fileObject editorObjects) Nothing noSyncOnGet (saveFile editorObjects) False
>     objectInit (filePathObject editorObjects) filePath noSyncOnGet noSyncOnPut False

>     forkIO $ handleGridEditWindowEvent exit Nothing gridEditWindowEvent

>     signal <- takeMVar exit
>     exitWith signal

>syncGridwithCanvas :: GridEditorObjects -> Grid -> IO ()
>syncGridwithCanvas editorObjects mygrid = do

We'll want to restore our scroll location(if possible) after we're done updating.

>   oldRectangle <- getObjectValue (focusedRectangleObject editorObjects)

We update the canvas with a new one which displays the new state of the grid.

>   updateIO (canvasObject editorObjects)  (\canvas      -> do{
>   canvasMVar <- newEmptyMVar;

The function postGUIAsync is required to insure thread safety in GTK.  Without it, the sync function will succeed 30% of the time, and fail 70% of the time :D

>   postGUIAsync (do {

First we get rid of the old canvas

>    canvasContainer' <- (widgetGetParent canvas);
>    canvasContainer  <- return (castToContainer (fromJust canvasContainer'));
>    containerRemove canvasContainer canvas;

And we make a new one...

>    canvas' <- scrolledWindowNew Nothing Nothing;

This is a bit tricky.  The drawGrid function will not actually run untill after this sync is finished.  Since it getObjectValue of the canvasObject and the gridObject in editorObjects, it will have to wait till these values are returned from this function...

>    forkIO $ do {drawGrid editorObjects mygrid;};

>    containerAdd canvasContainer canvas';

>    putMVar canvasMVar canvas';
>   });
>   canvas' <- takeMVar canvasMVar;

>   update (reFocusNeededObject editorObjects) (\_->True);
>   canvas' `on` exposeEvent $ do {
>         liftIO $ do {
>         update2
>           (reFocusNeededObject editorObjects)
>           (focusedRectangleObject editorObjects)
>              (\reFocus rect->
>                if reFocus
>                then (False, oldRectangle)
>                else (False, rect));};
>         return False;};

>   return canvas';})


>syncFocusedCellWithLabel :: Label -> Maybe 	DisplayCell.DisplayCell -> IO()
>syncFocusedCellWithLabel cellInfo (Just dc) = do
>     set cellInfo [ labelText := (show (displayCellPoint dc))]
>     return ()

>syncFocusedCellWithLabel _ Nothing = do
>     return ()

>syncEditModeWithLabel :: Label -> EditMode -> IO()
>syncEditModeWithLabel modeInfo mode = do
>     set modeInfo [ labelText := 
>       (case mode of
>         AddAction{}  -> "Add action mode"
>         AddPattern{} -> "Add pattern mode"
>         EditPath{}   -> "Path edit mode"
>         MoveCell{}   -> "Move cell mode | F3/Esc Exit Mode | Enter Place Cell"
>         EditCell{}   -> "Cell edit mode | Esc Exit Mode"
>         FreeMovement -> "Navigation mode | F3 MoveCell")]
>     return ()

>saveFile :: GridEditorObjects -> Maybe String -> IO()
>saveFile editorObjects (Just contents) = do
> filePath <- getObjectValue (filePathObject editorObjects)
> writeFile filePath contents
>saveFile _ Nothing = return ()

>syncFocusedRectangleWithScrolledWindow :: GridEditorObjects -> Rectangle -> IO()
>syncFocusedRectangleWithScrolledWindow editorObjects rectangle@(Rectangle x y _ _) = do
>  updateIO (canvasObject editorObjects) (\scrolledWindow -> do {
>   postGUIAsync $ do {

WARNING! This is a hack!

>   scrolledWindowContents <- containerGetChildren scrolledWindow;
>   (Rectangle _ _ aW aH)  <- widgetGetAllocation (head scrolledWindowContents);

End of hack.

>   adjustmentX    <- scrolledWindowGetHAdjustment scrolledWindow;
>   upperX         <- adjustmentGetUpper adjustmentX;
>   lowerX         <- adjustmentGetLower adjustmentX;
>   adjustmentSetValue adjustmentX (scrollCord (fromIntegral x) upperX lowerX (fromIntegral aW));

>   adjustmentY    <- scrolledWindowGetVAdjustment scrolledWindow;
>   upperY         <- adjustmentGetUpper adjustmentY;
>   lowerY         <- adjustmentGetLower adjustmentY;
>   adjustmentSetValue adjustmentY (scrollCord (fromIntegral y) upperY lowerY (fromIntegral aH));
>};
>  return scrolledWindow;});

>scrollCord cord upper lower boxAllocation
> | cord + boxAllocation/2 > upper = upper - boxAllocation
> | cord - boxAllocation/2 < lower = lower
> | otherwise                      = cord - boxAllocation/2

>loadGrid :: IO (Grid,FilePath)
>loadGrid = do
>     args <- getArgs 
>     if null args
>     then return (emptyGrid,"")
>     else do {
>     gridString <- readFile (head args);
>     return (openGrid gridString::Grid,(head args))}

>createWindow :: IO Window
>createWindow = do
>    window <- windowNew
>    set window [ windowTitle := "Grid editor", 
>                  windowDefaultWidth := 300, windowDefaultHeight := 250]
>    return window

>loadWidgets :: MVar GridEditWindowEvent -> GridEditorObjects -> IO (Window, VBox, ScrolledWindow, VBox, HBox, Label,Label) 
>loadWidgets gridEditWindowEvent editorObjects = do
>     window <- createWindow

>     myTopVBox <- vBoxNew False 0
>     containerAdd window myTopVBox

>     scrwinContainer <-vBoxNew False 0
>     boxPackStart myTopVBox scrwinContainer PackGrow 0
>     scrwin <- scrolledWindowNew Nothing Nothing
>     boxPackStart scrwinContainer scrwin PackGrow 0
     
>     sep2 <- hSeparatorNew
>     boxPackStart myTopVBox sep2 PackNatural 7
>     buttonBox <- hBoxNew False 0
>     boxPackStart myTopVBox buttonBox PackNatural 0

>     save <- buttonNewFromStock stockSave
>     boxPackStart buttonBox save PackNatural 0

>     cellInfo <- labelNew Nothing
>     boxPackStart buttonBox cellInfo PackNatural 0

>     modeInfo <- labelNew Nothing
>     boxPackStart buttonBox modeInfo PackNatural 0

>     quit <- buttonNewFromStock stockQuit
>     boxPackEnd buttonBox quit PackNatural 0

>     save `on` buttonPressEvent $ do{
>         liftIO $ do {update2
>            (gridObject editorObjects)
>            (fileObject editorObjects)
>            (\grid file -> (grid, (Just $ saveGrid(grid))))};
>         return True}

>     window `on` keyPressEvent $ do
>            key <- eventKeyName

<            liftIO (print key)

>            case key of
>             "Escape" ->
>              liftIO $ do
>               updateReturning (editModeObject editorObjects) 
>                   (\mode->
>                     case mode of
>                       EditCell{} -> (mode,False)
>                       otherwise  -> (FreeMovement,True))
>             "F3" ->
>              liftIO $ do
>               updateWith (focusedCellObject editorObjects) (editModeObject editorObjects)

No need to finish the pattern with a Nothing, tryEvent will catch the exception...

>                      (\(Just focusedCell) editMode -> 
>                        case editMode of
>                        MoveCell dc  -> FreeMovement
>                        FreeMovement -> MoveCell focusedCell);
>               return True;
>             otherwise -> return False

>     onClicked quit (do {widgetDestroy window; putMVar gridEditWindowEvent GridEditWindowQuit})
>     onDestroy window (do {mainQuit; putMVar gridEditWindowEvent GridEditWindowQuit})
>     return (window,myTopVBox, scrwin, scrwinContainer, buttonBox, cellInfo,modeInfo)
