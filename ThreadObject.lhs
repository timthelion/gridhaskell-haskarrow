LGPL copyright brmlab.cz contact timothyhobbs@seznam.cz

>module ThreadObject where
>import Control.Concurrent

This is a simple library for creating mutable objects in Haskell.  

To create an object you need to run

myObject <- threadObject
objectInit myObject myObjectsInitialValue syncorBindFunction

The sync function is run every time the object's value is updated.  This makes it easy to "bind" an object to a GTK widget which displays it(for example).

The nice thing about this library is that the two most frequently used functions are 100% thread safe(so long as you are using non mutable values and not GTK widgets or some other weirdness)!

If you want to get an object's value, you can use getObjectValue.  This is thread safe.

If you want to get an objects value, do something to it, and then set the object's value to a new value, you can use update.

update takes two arguments.  The object, and a pure haskell function which updates the value.  This is also thread safe.

The bind/sync function, and the updateIO functions are NOT thread safe, or even thread aware, however.  Be carefull of these.

>data ThreadObject a signal = ThreadObject{
>           tickerMVar     :: MVar (ActionType a signal)}

>data ActionType a signal = IOAction (a -> IO a) (Maybe signal) | PureAction (a -> a) (Maybe signal)| SetSyncOnGet (a -> IO a) | SetSyncOnPut (a -> (Maybe signal) -> IO ()) | GetObjectValueIO (a -> IO ()) | FreeObject

>threadObject :: IO (ThreadObject a signal)

>threadObject = do
>    tickerMVar    <- newEmptyMVar
>    return (ThreadObject tickerMVar)

>objectInit :: ThreadObject a signal -> a -> (a -> IO a) -> (a -> (Maybe signal) -> IO ()) -> Bool -> IO ()
>objectInit to value syncOnGet syncOnPut wait = do
>    forkIO $ loopObject to syncOnGet syncOnPut value wait Nothing
>    return ()

>loopObject :: ThreadObject a signal -> (a -> IO a) -> (a -> (Maybe signal) -> IO ()) -> a -> Bool -> (Maybe signal) -> IO ()
>loopObject  to@(ThreadObject tickerMVar)  syncOnGet syncOnPut value wait signal = do
>   if not wait
>   then syncOnPut value signal
>   else return ()
>   actionType <- takeMVar tickerMVar
>   case actionType of
>         PureAction action signal -> do
>                          value'  <- syncOnGet value
>                          loopObject to syncOnGet syncOnPut (action value') False signal
>         IOAction   action signal -> do
>                          value'  <- syncOnGet value
>                          value'' <- action value'
>                          loopObject to syncOnGet syncOnPut value'' False signal
>         GetObjectValueIO getter  -> do
>                          value'  <- syncOnGet value
>                          getter value'
>                          loopObject to syncOnGet syncOnPut value' True Nothing
>         SetSyncOnGet syncOnGet'  -> do
>                       loopObject to syncOnGet' syncOnPut value False Nothing
>         SetSyncOnPut syncOnPut'  -> do
>                       loopObject to syncOnGet syncOnPut' value False Nothing
>         FreeObject   ->    return ()

>update :: ThreadObject a signal -> (a -> a) -> IO ()
>update to action = do
>    updateWithSignal' to action Nothing

>updateWithSignal :: ThreadObject a signal -> (a -> a) -> signal -> IO ()
>updateWithSignal to action signal = do
>    updateWithSignal' to action (Just signal)

>updateWithSignal' :: ThreadObject a signal -> (a -> a) -> (Maybe signal) -> IO ()
>updateWithSignal' (ThreadObject tickerMVar) action signal = do
>    putMVar tickerMVar (PureAction action signal)

>updateReturning :: ThreadObject a signal -> (a -> (a,b)) -> IO b
>updateReturning to action = do
>  returnValueMVar <- newEmptyMVar
>  updateIO to (\value -> do
>   (value',returnValue) <- return (action value)
>   putMVar returnValueMVar returnValue
>   return value')
>  takeMVar returnValueMVar

>updateIOReturning :: ThreadObject a signal -> (a -> IO (a,b)) -> IO b
>updateIOReturning to action = do
>  returnValueMVar <- newEmptyMVar
>  updateIO to (\value -> do
>   (value',returnValue) <- action value
>   putMVar returnValueMVar returnValue
>   return value')
>  takeMVar returnValueMVar

f :: (a -> IO (a,b)) -> IO b
f action = do
 (a',b) <- action 1
 print a'
 return b

f $ f $ f (\a b c->return (a,(b,c)))

mainthread:
updateMulti to1 $ alsoUpdate to2 $ finallyUpdate to3 (\c b a -> (c,(b,a)))
to1:
value' <- action value -- action :: t1 -> IO t1
to2:                                      ^ putMVar v1MVar
value' <-action value  -- action :: t2 -> IO t2
to3:                                      ^ putMVar v2MVar
value' <-action value  -- action :: t3 -> IO t3

action v1 v2 v3 = (v3,(v2,(v1)))

>updateMulti :: ThreadObject a signalA -> (a -> IO a) -> IO ()
>updateMulti to action = do
>  updateIO to action

>alsoUpdate :: ThreadObject a signal -> (t -> a -> IO (a, b)) -> t -> IO b
>alsoUpdate to action = 
> (\a -> do
>  updateIOReturning to $ action a)

>finallyUpdate :: ThreadObject a signal -> (t -> a -> (a, b)) -> t -> IO b
>finallyUpdate to action = do
> (\a -> do
>  updateReturning to $ action a)

>updateHelper :: a -> b -> MVar a -> (a -> b -> IO (a,b)) -> IO b
>updateHelper a b aMVar action = do
>    (a',b') <- action a b
>    putMVar aMVar a'
>    return b'

>updateWith :: ThreadObject a signalA -> ThreadObject b signalB -> (a -> b -> b) -> IO ()
>updateWith to1 to2 action = do
> value1 <- getObjectValue to1
> update to2 (action value1)

>updateWith2 :: ThreadObject a signalA -> ThreadObject b signalB -> ThreadObject c signalC -> (a -> b -> c -> c) -> IO ()
>updateWith2 to1 to2 to3 action = do
> value1 <- getObjectValue to1
> value2 <- getObjectValue to2
> update to3 (action value1 value2)

| updateIO blocks by default.  Use updateIONoBlock if you don't want this to be the case.

>updateIO :: ThreadObject a signalA -> (a -> IO a) -> IO ()
>updateIO (ThreadObject tickerMVar) action = do
>    lock <- newEmptyMVar
>    putMVar tickerMVar $ IOAction
>        (\x -> do 
>                x' <- action x;
>                putMVar lock Nothing;
>                return x') Nothing
>    takeMVar lock
>    return ()

>updateIONoBlock :: ThreadObject a singalA -> (a -> IO a) -> IO ()
>updateIONoBlock (ThreadObject tickerMVar) action = do
>    putMVar tickerMVar (IOAction action Nothing) 


>getObjectValue :: ThreadObject a signalA -> IO a
>getObjectValue (ThreadObject tickerMVar) = do
>    valueMVar <- newEmptyMVar
>    putMVar tickerMVar (GetObjectValueIO (\value -> putMVar valueMVar value))
>    takeMVar valueMVar

>setSyncOnGet :: ThreadObject a signalA -> (a -> IO a) -> IO ()
>setSyncOnGet (ThreadObject tickerMVar) syncOnGet = do
>    putMVar tickerMVar $ SetSyncOnGet syncOnGet

>setSyncOnPut :: ThreadObject a signalA -> (a -> (Maybe signalA) -> IO ()) -> IO ()
>setSyncOnPut (ThreadObject tickerMVar) syncOnPut = do
>    putMVar tickerMVar $ SetSyncOnPut syncOnPut

>noSyncOnGet :: a -> IO a
>noSyncOnGet value = return value

>noSyncOnPut :: a -> (Maybe signalA) -> IO ()
>noSyncOnPut _ _ = return ()

>freeObject :: ThreadObject a signalA -> IO ()
>freeObject (ThreadObject tickerMVar) = do
>  putMVar tickerMVar FreeObject
