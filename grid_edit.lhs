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
>import StateRecords

There are also some Super generic types that belong no where else.  Like type Point = (Int, Int)...

>import qualified Super

For reading and writing grid haskell files.

>import GridHaskellFile

>main :: IO ()
>main = do
>     (mygrid, filePath) <- loadGrid

     initGUI

>     unsafeInitGUIForThreadedRTS

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

>     gridRecords'            <- stateRecords gridObject' 
>     focusedCellRecords'     <- stateRecords focusedCellObject'
>     focusedRectangleRecords'<- stateRecords focusedRectangleObject'

>     editorObjects <- return (GridEditorObjects gridObject' canvasObject' editModeObject' focusedCellObject' focusedRectangleObject' reFocusNeededObject' fileObject' filePathObject' gridRecords' focusedCellRecords' focusedRectangleRecords')

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

>syncGridwithCanvas :: GridEditorObjects -> Grid -> Maybe (RecorderSignal ()) -> IO ()
>syncGridwithCanvas editorObjects mygrid signal = do

This is as good a time as any to record the state of our grid.  We will keep at least 20 and up to 40 states which can be returned to with the undo(Ctrl-z) function.  

>   case signal of
>     Just (RecorderSignal False Nothing) ->
>       return ()
>     Nothing -> do
>       focusedCell <- getObjectValue (focusedCellObject editorObjects)
>       focusedRectangle <- getObjectValue (focusedRectangleObject editorObjects)
>       recordState 20 (gridRecords editorObjects) mygrid
>       recordState 20 (focusedCellRecords editorObjects) focusedCell
>       recordState 20 (focusedRectangleRecords editorObjects) focusedRectangle

We'll want to restore our scroll location(if possible) after we're done updating.

   print "Recorded states, now getting value of focusedRectangleObject"

>   oldRectangle <- getObjectValue (focusedRectangleObject editorObjects)

We update the canvas with a new one which displays the new state of the grid.

   print "entering update for canvasObject"

>   updateIO (canvasObject editorObjects)  (\canvas      -> do{

   print "entering postGUISync";

The function postGUIAsync is required to insure thread safety in GTK.  Without it, the sync function will succeed 30% of the time, and fail 70% of the time :D

>   postGUISync (do {

    print "Entered postGUISync on grid sync";

First we get rid of the old canvas

>    canvasContainer' <- (widgetGetParent canvas);

    print "Casting canvas's parent(VBox) to container.";

>    canvasContainer  <- return (castToContainer (fromJust canvasContainer'));

    print "destroying canvas!";

>    widgetDestroy canvas;

And we make a new one...

    print "Creating new canvas.";

>    canvas' <- scrolledWindowNew Nothing Nothing;

Don't show scroll bars, the don't work anyways.

>    scrolledWindowSetPolicy canvas' PolicyNever PolicyNever;

    print "Drawing the grid.";

>    focusedWidgetMaybe <- drawGrid editorObjects mygrid canvas';

    print "Adding canvas back into the container.";

>    containerAdd canvasContainer canvas';

    print "Canvas added.";

    print "updating reFocusNeededObject";

>    update (reFocusNeededObject editorObjects) (\_->True);

    print "adding exposeEvent";

>    canvas' `on` exposeEvent $ do {
>         liftIO $ do {
>         updateMulti
>            (reFocusNeededObject editorObjects) $
>           finallyUpdate
>            (focusedRectangleObject editorObjects) $
>              (\reFocus rect->
>                if reFocus
>                then (oldRectangle,False)
>                else (rect,False));};
>         return False;};

    print "Grabbing focus for focused cell.";

>    (case focusedWidgetMaybe of
>       Just focusedWidget -> widgetGrabFocus focusedWidget;
>       Nothing -> return ());

>    widgetShowAll canvas';

>    return canvas';
>   });})


>syncFocusedCellWithLabel :: Label -> Maybe 	DisplayCell.DisplayCell -> Maybe a -> IO()
>syncFocusedCellWithLabel cellInfo (Just dc) _ = postGUIAsync $ do
>     set cellInfo [ labelText := (show (displayCellPoint dc))]
>     return ()

>syncFocusedCellWithLabel _ Nothing _ = do
>     return ()

>syncEditModeWithLabel :: Label -> EditMode -> Maybe a -> IO()
>syncEditModeWithLabel modeInfo mode _ = postGUIAsync $ do
>     set modeInfo [ labelText := 
>       (case mode of
>         AddAction{}  -> "Add action mode"
>         AddPattern{} -> "Add pattern mode"
>         EditPath{}   -> "Path edit mode"
>         MoveCell{}   -> "Move cell mode | F3/Esc Exit Mode | Enter Place Cell"
>         EditCell{}   -> "Cell edit mode | Esc Exit Mode"
>         FreeMovement -> "Navigation mode | F3 MoveCell")]
>     return ()

>saveFile :: GridEditorObjects -> Maybe String -> Maybe () -> IO()
>saveFile editorObjects (Just contents) signal = do
> filePath <- getObjectValue (filePathObject editorObjects)
> writeFile filePath contents
>saveFile _ Nothing signal = return ()

>syncFocusedRectangleWithScrolledWindow :: GridEditorObjects -> Rectangle -> Maybe () -> IO()
>syncFocusedRectangleWithScrolledWindow editorObjects rectangle@(Rectangle x y _ _) signal = do
>  updateIO (canvasObject editorObjects) (\scrolledWindow -> do {
>   postGUIAsync $ do {

WARNING! This is a hack! (to take into accound the size of the scroll bars.  )

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
>         liftIO $ do {updateMulti
>            (gridObject editorObjects) $
>           finallyUpdate
>            (fileObject editorObjects) $
>            (\grid file -> ((Just $ saveGrid(grid)),grid))};
>         return True}

>     window `on` keyPressEvent $ do
>            modifier <- eventModifier
>            key <- eventKeyName

<            liftIO (print key)

>            case (modifier,key) of
>             ([],"Escape") ->
>              liftIO $ do
>               updateReturning (editModeObject editorObjects) 
>                   (\mode->
>                     case mode of
>                       EditCell{} -> (mode,False)
>                       otherwise  -> (FreeMovement,True))
>             ([],"F3") ->
>              liftIO $ do
>               updateWith (focusedCellObject editorObjects) (editModeObject editorObjects)

No need to finish the pattern with a Nothing, tryEvent will catch the exception...

>                      (\(Just focusedCell) editMode -> 
>                        case editMode of
>                        MoveCell dc  -> FreeMovement
>                        FreeMovement -> MoveCell focusedCell);
>               return True;

>             ([Control],"z")  -> 
>              liftIO $ do
>                undoStateAction (focusedCellRecords editorObjects)
>                undoStateAction (focusedRectangleRecords editorObjects)
>                undoStateActionOfRecorder (gridRecords editorObjects)
>                return True
               
>             otherwise -> return False

>     onClicked quit (do {widgetDestroy window; putMVar gridEditWindowEvent GridEditWindowQuit})
>     onDestroy window (do {mainQuit; putMVar gridEditWindowEvent GridEditWindowQuit})
>     return (window,myTopVBox, scrwin, scrwinContainer, buttonBox, cellInfo,modeInfo)
