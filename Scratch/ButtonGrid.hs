import Graphics.UI.Gtk
import Data.IORef 


main:: IO ()
main= do
     initGUI
     window <- windowNew
     set window [ windowTitle := "Grid editor", 
                  windowDefaultWidth := 300, windowDefaultHeight := 250]
     mb <- vBoxNew False 0
     containerAdd window mb
          
     scrwin <- scrolledWindowNew Nothing Nothing
     boxPackStart mb scrwin PackGrow 0

     table <- tableNew 10 10 True
     scrolledWindowAddWithViewport scrwin table

     buttonlist <- sequence (map numButton [1..100])
     let places = cross [0..9] [0..9]
     sequence_ (zipWith (attachButton table) buttonlist places)

     sep2 <- hSeparatorNew
     boxPackStart mb sep2 PackNatural 7
     hb <- hBoxNew False 0
     boxPackStart mb hb PackNatural 0
     play <- buttonNewFromStock stockNew
     quit <- buttonNewFromStock stockQuit
     boxPackStart hb play PackNatural 0
     boxPackEnd hb quit PackNatural 0
     
     randstore <- newIORef 50
     randomButton randstore play

     widgetShowAll window
     onClicked quit (widgetDestroy window)
     onDestroy window mainQuit
     mainGUI

numButton :: Int -> IO Entry
numButton n = do
        button <- entryNew
        return button

cross :: [Int] -> [Int] -> [(Int,Int)]
cross row col = do 
        x <- row
        y <- col
        return (x,y)

attachButton :: Table -> Entry -> (Int,Int) -> IO ()
attachButton ta bu (x,y) = 
              tableAttachDefaults ta bu y (y+1) x (x+1)


randomButton :: ButtonClass b => IORef Int -> b -> IO (ConnectId b)
randomButton rst b = 
    onClicked b $ do 
                     writeIORef rst 1
