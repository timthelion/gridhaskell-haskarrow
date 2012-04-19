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

>data ThreadObject a = ThreadObject{
>           tickerMVar     :: MVar (ActionType a)}

>data ActionType a = IOAction (a -> IO a) | PureAction (a -> a)| SetSyncOnGet (a -> IO a) | SetSyncOnPut (a -> IO ()) | GetObjectValueIO (a -> IO ()) | FreeObject

>threadObject :: IO (ThreadObject a) 

>threadObject = do
>    tickerMVar    <- newEmptyMVar
>    return (ThreadObject tickerMVar)

>objectInit :: ThreadObject a -> a -> (a -> IO a) -> (a -> IO ()) -> Bool -> IO ()
>objectInit to value syncOnGet syncOnPut wait = do
>    forkIO $ loopObject to syncOnGet syncOnPut value wait
>    return ()

>loopObject :: ThreadObject a -> (a -> IO a) -> (a -> IO ()) -> a -> Bool -> IO ()
>loopObject  to@(ThreadObject tickerMVar)  syncOnGet syncOnPut value wait = do
>   if not wait
>   then syncOnPut value
>   else return ()
>   actionType <- takeMVar tickerMVar
>   case actionType of
>         IOAction   action        -> do
>                          value'  <- syncOnGet value
>                          value'' <- action value'
>                          loopObject to syncOnGet syncOnPut value'' False
>         PureAction action        -> do
>                          value'  <- syncOnGet value
>                          loopObject to syncOnGet syncOnPut (action value') False
>         GetObjectValueIO getter  -> do
>                          value'  <- syncOnGet value
>                          getter value'
>                          loopObject to syncOnGet syncOnPut value' True 
>         SetSyncOnGet syncOnGet'  -> do
>                       loopObject to syncOnGet' syncOnPut value False 
>         SetSyncOnPut syncOnPut'  -> do
>                       loopObject to syncOnGet syncOnPut' value False
>         FreeObject   ->    return ()

>update :: ThreadObject a -> (a -> a) -> IO ()
>update (ThreadObject tickerMVar) action = do
>    putMVar tickerMVar (PureAction action)

>updateReturning :: ThreadObject a -> (a -> (a,b)) -> IO b
>updateReturning to action = do
>  returnValueMVar <- newEmptyMVar
>  updateIO to (\value -> do
>   (value',returnValue) <- return (action value)
>   putMVar returnValueMVar returnValue
>   return value')
>  takeMVar returnValueMVar

>update2 :: ThreadObject a -> ThreadObject b -> (a -> b -> (a,b)) -> IO ()
>update2 to1 to2 action = do
>  value1MVar <- newEmptyMVar
>  updateIO to1 (\value1 -> do
>   updateIO to2 (\value2 -> do
>    update2helper value1 value2 value1MVar action);
>    takeMVar value1MVar)

>update2helper :: a -> b -> MVar a -> (a -> b -> (a,b)) -> IO b
>update2helper a b aMVar action =
>  let (a',b') = action a b in do
>    putMVar aMVar a'
>    return b'

>update3 :: ThreadObject a -> ThreadObject b -> ThreadObject c -> (a -> b-> c -> (a,b,c)) -> IO ()
>update3 to1 to2 to3 action = do
>  value1MVar <- newEmptyMVar
>  value2MVar <- newEmptyMVar
>  updateIO to1 (\value1 -> do
>   updateIO to2 (\value2 -> do
>    updateIO to3 (\value3 -> do
>     update3helper value1 value2 value3 value1MVar value2MVar action);
>    takeMVar value2MVar)
>   takeMVar value1MVar)

>update3helper :: a -> b -> c -> MVar a -> MVar b -> (a -> b -> c -> (a,b,c)) -> IO c
>update3helper a b c aMVar bMVar action =
>  let (a',b',c') = action a b c in do
>    putMVar aMVar a'
>    putMVar bMVar b'
>    return c'

>updateWith :: ThreadObject a -> ThreadObject b -> (a -> b -> b) -> IO ()
>updateWith to1 to2 action = do
> value1 <- getObjectValue to1
> update to2 (action value1)

>updateWith2 :: ThreadObject a -> ThreadObject b -> ThreadObject c -> (a -> b -> c -> c) -> IO ()
>updateWith2 to1 to2 to3 action = do
> value1 <- getObjectValue to1
> value2 <- getObjectValue to2
> update to3 (action value1 value2)

| updateIO blocks by default.  Use updateIONoBlock if you don't want this to be the case.

>updateIO :: ThreadObject a -> (a -> IO a) -> IO ()
>updateIO (ThreadObject tickerMVar) action = do
>    lock <- newEmptyMVar
>    putMVar tickerMVar $ IOAction
>        (\x -> do 
>                x' <- action x;
>                putMVar lock Nothing;
>                return x')
>    takeMVar lock
>    return ()

>updateIONoBlock :: ThreadObject a -> (a -> IO a) -> IO ()
>updateIONoBlock (ThreadObject tickerMVar) action = do
>    putMVar tickerMVar (IOAction action)


>getObjectValue :: ThreadObject a -> IO a
>getObjectValue (ThreadObject tickerMVar) = do
>    valueMVar <- newEmptyMVar
>    putMVar tickerMVar (GetObjectValueIO (\value -> putMVar valueMVar value))
>    takeMVar valueMVar

>setSyncOnGet :: ThreadObject a -> (a -> IO a) -> IO ()
>setSyncOnGet (ThreadObject tickerMVar) syncOnGet = do
>    putMVar tickerMVar $ SetSyncOnGet syncOnGet

>setSyncOnPut :: ThreadObject a -> (a -> IO ()) -> IO ()
>setSyncOnPut (ThreadObject tickerMVar) syncOnPut = do
>    putMVar tickerMVar $ SetSyncOnPut syncOnPut

>noSyncOnGet :: a -> IO a
>noSyncOnGet value = return value

>noSyncOnPut :: a -> IO ()
>noSyncOnPut _ = return ()

>freeObject :: ThreadObject a -> IO ()
>freeObject (ThreadObject tickerMVar) = do
>  putMVar tickerMVar FreeObject
