GPL V 3 or higher, copyright brmlab.cz contact timothyhobbs@seznam.cz

>module ThreadObject where
>import Control.Concurrent

This is a simple library for creating mutable objects in Haskell.  

To create an object you need to run

myObject <- threadObject
objectInit myObject myObjectsInitialValue syncorBindFunction

The sync function is run every time the object's value is updated.  This makes it easy to "bind" an object to a GTK widget which displays it(for example).

The nice thing about this library is that the two most frequently used functions are 100% thread safe!

If you want to get an object's value, you can use getObjectValue.  This is thread safe.

If you want to get an objects value, do something to it, and then set the object's value to a new value, you can use update.

update takes two arguments.  The object, and a pure haskell function which updates the value.  This is also thread safe.

The bind/sync function, and the updateIO functions are NOT thread safe, or even thread aware, however.  Be carefull of these.

>data ThreadObject a = ThreadObject{
>           actionMVar     :: MVar (a -> a),
>           actionIOMVar   :: MVar (a -> IO a),
>           syncOnGetMVar  :: MVar (a -> IO a),
>           syncOnPutMVar  :: MVar (a -> IO ()),
>           tickerMVar     :: MVar ActionType}

>data ActionType = IOAction | PureAction | SetSyncOnGet | SetSyncOnPut | FreeObject

>threadObject :: IO (ThreadObject a) 

>threadObject = do
>    actionMVar    <- newEmptyMVar
>    actionIOMVar  <- newEmptyMVar
>    syncOnGetMVar <- newEmptyMVar
>    syncOnPutMVar <- newEmptyMVar
>    tickerMVar    <- newEmptyMVar
>    return (ThreadObject actionMVar actionIOMVar syncOnGetMVar syncOnPutMVar tickerMVar)

>objectInit :: ThreadObject a -> a -> (a -> IO a) -> (a -> IO ()) -> IO ()
>objectInit to value syncOnGet syncOnPut = do
>    forkIO $ loopObject to syncOnGet syncOnPut value
>    return ()

>loopObject :: ThreadObject a -> (a -> IO a) -> (a -> IO ()) -> a -> IO ()
>loopObject  to@(ThreadObject actionMVar actionIOMVar syncOnGetMVar syncOnPutMVar tickerMVar)  syncOnGet syncOnPut value = do
>   syncOnPut value
>   actionType <- takeMVar tickerMVar
>   case actionType of
>         IOAction   -> do action  <- takeMVar actionIOMVar
>                          value'  <- syncOnGet value
>                          value'' <- action value'
>                          loopObject to syncOnGet syncOnPut value''
>         PureAction -> do action  <- takeMVar actionMVar
>                          value'  <- syncOnGet value
>                          loopObject to syncOnGet syncOnPut (action value')

>         SetSyncOnGet -> do syncOnGet' <- takeMVar syncOnGetMVar
>                            loopObject to syncOnGet' syncOnPut value
>         SetSyncOnPut -> do syncOnPut' <- takeMVar syncOnPutMVar
>                            loopObject to syncOnGet syncOnPut' value

>         FreeObject   ->    return ()

>update :: ThreadObject a -> (a -> a) -> IO ()
>update (ThreadObject actionMVar _ _ _ ticker) action = do
>    putMVar ticker PureAction
>    putMVar actionMVar action

>update2 :: ThreadObject a -> ThreadObject b -> (a -> b -> (a,b)) -> IO ()
>update2 to1 to2 action = do
>  valueMVar <- newEmptyMVar
>  updateIO to1 (\value1 -> do
>   updateIO to2 (\value2 -> do
>    update2helper value1 value2 valueMVar action);
>    takeMVar valueMVar)

>update2helper :: a -> b -> MVar a -> (a -> b -> (a,b)) -> IO b
>update2helper a b aMVar action =
>  let (a',b') = action a b in do
>    putMVar aMVar a'
>    return b' 

>update2' :: ThreadObject a -> ThreadObject b -> (a -> b -> (a,b)) -> IO ()
>update2' (ThreadObject _ actionIOMVar1 _ _ ticker1) (ThreadObject _ actionIOMVar2 _ _ ticker2) action = do
>    putMVar ticker1 IOAction
>    putMVar actionIOMVar1 action1
>    where action1 v1 = do
>           valueMVar <- newEmptyMVar;
>           putMVar ticker2 IOAction;
>           putMVar actionIOMVar2 (action2 valueMVar);
>           takeMVar valueMVar;
>           where action2 valueMVar v2 =
>                  let (v'1,v'2) = action v1 v2 in do
>                    putMVar valueMVar v'1
>                    return v'2

>update3 :: ThreadObject a -> ThreadObject b -> ThreadObject c -> (a -> b -> c -> (a,b,c)) -> IO ()
>update3 (ThreadObject _ actionIOMVar1 _ _ ticker1) (ThreadObject _ actionIOMVar2 _ _ ticker2) (ThreadObject _ actionIOMVar3 _ _ ticker3) action = do
>    putMVar ticker1 IOAction
>    putMVar actionIOMVar1 action1
>    where action1 v1 = do
>           valueMVar1 <- newEmptyMVar;
>           putMVar ticker2 IOAction
>           putMVar actionIOMVar2 (action2 valueMVar1)
>           takeMVar valueMVar1
>           where action2 valueMVar1 v2 = do
>                  valueMVar2 <- newEmptyMVar;
>                  putMVar ticker3 IOAction
>                  putMVar actionIOMVar3 (action3 valueMVar1 valueMVar2)
>                  takeMVar valueMVar2
>                  where action3 valueMVar1 valueMVar2 v3 = 
>                           let (v'1,v'2,v'3) = action v1 v2 v3 in do
>                             putMVar valueMVar1 v'1;
>                             putMVar valueMVar2 v'2;
>                             return v'3;

>updateIO :: ThreadObject a -> (a -> IO a) -> IO ()
>updateIO (ThreadObject _ actionIOMVar _ _ ticker) action = do
>    putMVar ticker IOAction
>    putMVar actionIOMVar action

>getObjectValue :: ThreadObject a -> IO a
>getObjectValue to = do
>     valueMVar <- newEmptyMVar;
>     updateIO to (\value -> do{
>      putMVar valueMVar value;
>      return value;
>     });
>     takeMVar valueMVar;

>setSyncOnGet :: ThreadObject a -> (a -> IO a) -> IO ()
>setSyncOnGet (ThreadObject _ _ syncOnGetMVar _ ticker) syncOnGet = do
>    putMVar ticker SetSyncOnGet
>    putMVar syncOnGetMVar syncOnGet

>setSyncOnPut :: ThreadObject a -> (a -> IO ()) -> IO ()
>setSyncOnPut (ThreadObject _ _ _ syncOnPutMVar ticker) syncOnPut = do
>    putMVar ticker SetSyncOnPut
>    putMVar syncOnPutMVar syncOnPut


>noSyncOnGet :: a -> IO a
>noSyncOnGet value = return value

>noSyncOnPut :: a -> IO ()
>noSyncOnPut _ = return ()

>freeObject :: ThreadObject a -> IO ()
>freeObject (ThreadObject _ _ _ _ ticker) = putMVar ticker FreeObject
