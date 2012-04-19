LGPL copyright brmlab.cz contact timothyhobbs@seznam.cz

>module StateRecords where

>import ThreadObject
>import Controll.Concurrent

>data RecordingPattern = Int

>type StateRecords a = ThreadObject (([a],[a]),(ThreadObject a))

>stateRecords :: ThreadObject a -> IO StateRecords a
>stateRecords to = do
> stateRecordsObject <- threadObject
> objectInit stateRecordsObject (([],[]),to) noSyncOnGet noSyncOnPut False
> stateRecordsObject 

>recordState ::  Int -> StateRecords a -> a -> IO ()
>recordState n stateRecordsObject value = do
>  update stateRecordsObject 
>    (\((stack1,stack2),to)->
>     if length stack1 >= n
>     then ((value:[],stack1), to)
>     else ((value:stack1,stack2), to))

>undoStateAction :: StateRecords a -> IO Bool
>undoStateAction stateRecordsObject = do
>  sucessMVar <- newEmptyMVar
>  updateIO stateRecordsObject
>    (\(stacks,to)-> do
>     case stacks of
>       (value:stack1,stack2) -> do
>           update to (\_->value)
>           putMVar sucessMVar True
>           return (stack1,stack2)

>       ([],value:stack2) -> do
>           update to (\_->value)
>           putMVar sucessMVar True
>           return (stack1,stack2)

>       ([],[])  -> do
>          putMVar sucessMVar False
>          return ([],[]))
>  takeMVar sucessMVar

>type StateRecords2 a b = ThreadObject (([(a,b)],[(a,b)]), (ThreadObject a), (ThreadObject b))

>stateRecords2 :: ThreadObject a -> ThreadObject b -> IO StateRecords2 a b
>stateRecords2 to1 to2 = do
> stateRecordsObject <- threadObject
> objectInit stateRecordsObject (([],[]),to1,to2) noSyncOnGet noSyncOnPut False
> stateRecordsObject


>recordState2 ::  Int -> StateRecords2 a b -> a -> IO ()
>recordState2 n stateRecordsObject value = do
>  value2 <- getObjectValue 
>  update stateRecordsObject
>    (\((stack1,stack2),to1,to2)-> 
>     if length stack1 >= n
>     then ((value,value2):[],stack1)
>     else ((value,value2):stack1,stack2))

>undoStateAction2 :: StateRecords2 a b -> IO Bool
>undoStateAction2 stateRecordsObject = do
>  sucessMVar <- newEmptyMVar
>  updateIO stateRecordsObject
>    (\(stacks,to,to2)-> do
>     case stacks of
>       (values:stack1,stack2) -> do
>           update2 to to2 (\_ _->values)
>           putMVar sucessMVar True
>           return (stack1,stack2)

>       ([],values:stack2) -> do
>           update2 to to2 (\_ _->values)
>           putMVar sucessMVar True
>           return (stack1,stack2)

>       ([],[])  -> do
>          putMVar sucessMVar False
>          return ([],[]))
>  takeMVar sucessMVar

>type StateRecords3 a b c = ThreadObject (([(a,b,c)],[(a,b,c)]), (ThreadObject a), (ThreadObject b), (ThreadObject c))

>stateRecords3 :: ThreadObject a -> ThreadObject b -> ThreadObject c -> IO StateRecords3 a b c
>stateRecords2 to1 to2 to3 = do
> stateRecordsObject <- threadObject
> objectInit stateRecordsObject (([],[]),to1,to2,to3) noSyncOnGet noSyncOnPut False
> stateRecordsObject

>recordState3 ::  Int -> StateRecords3 a b c -> a -> IO ()
>recordState3 n stateRecordsObject value = do
>  value2 <- getObjectValue
>  value3 <- getObjectValue
>  update stateRecordsObject
>    (\((stack1,stack2),to1,to2,to3)-> 
>     if length stack1 >= n
>     then ((value,value2,value3):[],stack1)
>     else ((value,value2,value3):stack1,stack2))

>undoStateAction3 :: StateRecords3 a b c -> IO Bool
>undoStateAction3 stateRecordsObject = do
>  sucessMVar <- newEmptyMVar
>  updateIO stateRecordsObject
>    (\(stacks,to,to2,to3)-> do
>     case stacks of
>       (values:stack1,stack2) -> do
>           update3 to to2 to3 (\_ _ _->values)
>           putMVar sucessMVar True
>           return (stack1,stack2)

>       ([],values:stack2) -> do
>           update3 to to2 to3 (\_ _ _->values)
>           putMVar sucessMVar True
>           return (stack1,stack2)

>       ([],[])  -> do
>          putMVar sucessMVar False
>          return ([],[]))
>  takeMVar sucessMVar
