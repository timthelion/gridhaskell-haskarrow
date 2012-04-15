>module GridEditWindowEvents where
>import Control.Concurrent
>import System.Exit

>data GridEditWindowEvent = GridEditWindowQuit

>handleGridEditWindowEvent :: MVar ExitCode -> Maybe GridEditWindowEvent -> MVar GridEditWindowEvent -> IO ()

>handleGridEditWindowEvent exit Nothing eventMVar = do
>    event<-(takeMVar eventMVar)
>    handleGridEditWindowEvent exit (Just event) eventMVar
>    return ()

>handleGridEditWindowEvent exit (Just GridEditWindowQuit) _ = do
>    print "Quit"
>    putMVar exit ExitSuccess
