>module Main where

Libraries external to application.

>import Graphics.UI.Gtk hiding (cellText)
>import Data.IORef 
>import System.Environment
>import Data.List
>import Control.Concurrent

Internal libraries.

Our top level data type/file type.

>import Grid

Grid haskell comands are defined by the type Cell in the module Cell.

These Cells are represented on the screen, but there are also other things that we display on the screen.  For example, comments.  So therefore we have a seccond type to encapsulate both.

>import DisplayCell

And then we need some code to actually draw these DisplayCells on the screen.

>import GridDrawing

>main :: IO ()
>main = do
>     mygrid <- loadGrid

>     initGUI

We use MVars to handle events.  This means no need for a global mutable state.  We can just sit in a loop and eat MVars all day.

>     gridEditWindowEvent <- newEmptyMVar 
>     myWidgets <- loadWidgets gridEditWindowEvent
>     (window,_,scrwin,_) <- return myWidgets
     
>     widgetShowAll window

>     (table,cellFormList)<-buildTable scrwin mygrid
     
>     widgetShowAll window
     
>     forkIO mainGUI
>     event <-takeMVar gridEditWindowEvent
>     handleGridEditWindowEvent event gridEditWindowEvent

This is a handy data type for figuring out what an event means.  It's like xlib's bitmasking.

>data GridEditWindowEvent = GridEditWindowQuit | GridEditWindowSave

>handleGridEditWindowEvent :: GridEditWindowEvent -> MVar GridEditWindowEvent -> IO ()
>handleGridEditWindowEvent GridEditWindowQuit _ = print "Quit"
>handleGridEditWindowEvent GridEditWindowSave eventMVar = do
>    print "Save"
>    event<-(takeMVar eventMVar)
>    handleGridEditWindowEvent event eventMVar
>    return ()

>loadGrid :: IO Grid
>loadGrid = do
>     args <- getArgs 
>     (if null args then return emptyGrid else do {
>        gridString <- readFile (head args);
>        return (read gridString::Grid)})

>createWindow :: IO Window
>createWindow = do
>    window <- windowNew
>    set window [ windowTitle := "Grid editor", 
>                  windowDefaultWidth := 300, windowDefaultHeight := 250]
>    return window

>loadWidgets :: MVar GridEditWindowEvent -> IO (Window, VBox, ScrolledWindow, HBox) 
>loadWidgets gridEditWindowEvent = do
>     window <- createWindow
>     myTopVBox <- vBoxNew False 0
>     containerAdd window myTopVBox

>     scrwin <- scrolledWindowNew Nothing Nothing
>     boxPackStart myTopVBox scrwin PackGrow 0
     
     
>     sep2 <- hSeparatorNew
>     boxPackStart myTopVBox sep2 PackNatural 7
>     buttonBox <- hBoxNew False 0
>     boxPackStart myTopVBox buttonBox PackNatural 0

>     save <- buttonNewFromStock stockSave
>     boxPackStart buttonBox save PackNatural 0

>     quit <- buttonNewFromStock stockQuit
>     boxPackEnd buttonBox quit PackNatural 0

>     onClicked save (do {putMVar gridEditWindowEvent GridEditWindowSave})

>     onClicked quit (do {widgetDestroy window; putMVar gridEditWindowEvent GridEditWindowQuit})
>     onDestroy window (do {mainQuit; putMVar gridEditWindowEvent GridEditWindowQuit})
>     return (window,myTopVBox, scrwin, buttonBox)
