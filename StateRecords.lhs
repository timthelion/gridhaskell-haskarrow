LGPL copyright brmlab.cz contact timothyhobbs@seznam.cz

>module StateRecords where

>import ThreadObject
>import Control.Concurrent

>data RecorderSignal signal = RecorderSignal Bool (Maybe signal) 

>type StateRecords a signal = ThreadObject (([a],[a]),(ThreadObject a (RecorderSignal signal))) ()

>stateRecords :: ThreadObject a (RecorderSignal signal) -> IO (StateRecords a signal)
>stateRecords to = do
> stateRecordsObject <- threadObject
> objectInit stateRecordsObject (([],[]),to) noSyncOnGet noSyncOnPut False
> return stateRecordsObject 

>recordState ::  Int -> StateRecords a signal -> a -> IO ()
>recordState n stateRecordsObject value = do
>  update stateRecordsObject 
>    (\((stack1,stack2),to)->
>     if length stack1 >= n
>     then ((value:[],stack1), to)
>     else ((value:stack1,stack2), to))

>undoStateAction :: StateRecords a signal -> IO Bool
>undoStateAction stateRecordsObject = do
>  sucessMVar <- newEmptyMVar
>  updateIO stateRecordsObject
>    (\(stacks,to)-> do
>     case stacks of
>       (value:stack1,stack2) -> do
>           updateWithSignal to (\_->value) (RecorderSignal False Nothing)
>           putMVar sucessMVar True
>           return ((stack1,stack2),to)

>       ([],value:stack2) -> do
>           updateWithSignal to (\_->value) (RecorderSignal False Nothing)
>           putMVar sucessMVar True
>           return ((stack2,[]),to)

>       ([],[])  -> do
>          putMVar sucessMVar False
>          return (([],[]),to))
>  takeMVar sucessMVar

>type StateRecords2 a b signalA signalB = ThreadObject (([(a,b)],[(a,b)]), (ThreadObject a (RecorderSignal signalA)), (ThreadObject b signalB)) ()

>stateRecords2 :: ThreadObject a (RecorderSignal signalA) -> ThreadObject b signalB -> IO (StateRecords2 a b signalA signalB)
>stateRecords2 to1 to2 = do
> stateRecordsObject <- threadObject
> objectInit stateRecordsObject (([],[]),to1,to2) noSyncOnGet noSyncOnPut False
> return stateRecordsObject


>recordState2 ::  Int -> StateRecords2 a b signalA signalB -> a -> IO ()
>recordState2 n stateRecordsObject value = do 
>  updateIO stateRecordsObject
>    (\((stack1,stack2),to1,to2)-> do 
>     value2 <- getObjectValue to2
>     if length stack1 >= n
>     then return (((value,value2):[],stack1),to1,to2)
>     else return (((value,value2):stack1,stack2),to1,to2))

>undoStateAction2 :: StateRecords2 a b signalA signalB -> IO Bool
>undoStateAction2 stateRecordsObject = do
>  sucessMVar <- newEmptyMVar
>  updateIO stateRecordsObject
>    (\(stacks,to,to2)-> do
>     case stacks of
>       (values:stack1,stack2) -> do
>           updateWithSignal to  (\_ ->fst values) (RecorderSignal False Nothing)
>           update           to2 (\_ ->snd values)
>           putMVar sucessMVar True
>           return ((stack1,stack2),to,to2)

>       ([],values:stack2) -> do
>           updateWithSignal to  (\_ ->fst values) (RecorderSignal False Nothing)
>           update           to2 (\_ ->snd values)
>           putMVar sucessMVar True
>           return ((stack2,[]),to,to2)

>       ([],[])  -> do
>          putMVar sucessMVar False
>          return (([],[]),to,to2))
>  takeMVar sucessMVar

>type StateRecords3 a b c signalA signalB signalC = ThreadObject (((Maybe (a,b,c)),[(a,b,c)],[(a,b,c)]), (ThreadObject a (RecorderSignal signalA)), (ThreadObject b signalB), (ThreadObject c signalC)) ()

>stateRecords3 :: ThreadObject a (RecorderSignal signalA) -> ThreadObject b signalB -> ThreadObject c signalC -> IO (StateRecords3 a b c signalA signalB signalC)
>stateRecords3 to1 to2 to3 = do
> stateRecordsObject <- threadObject
> objectInit stateRecordsObject ((Nothing,[],[]),to1,to2,to3) noSyncOnGet noSyncOnPut False
> return stateRecordsObject

>recordState3 ::  Int -> StateRecords3 a b c signalA signalB signalC -> a -> IO ()
>recordState3 n stateRecordsObject value = do
>  updateIO stateRecordsObject
>    (\((last,stack1,stack2),to1,to2,to3)-> do 
>     value2 <- getObjectValue to2
>     value3 <- getObjectValue to3
>     if length stack1 >= n
>     then case last of
>           Just last -> return ((Just (value,value2,value3),last:[],stack1),to1,to2,to3)
>           Nothing   -> return ((Just (value,value2,value3),[],stack1),to1,to2,to3)
>     else case last of
>           Just last -> return ((Just (value,value2,value3),last:stack1,stack2),to1,to2,to3)
>           Nothing   -> return ((Just (value,value2,value3),stack1,stack2),to1,to2,to3))

>undoStateAction3 :: StateRecords3 a b c signalA signalB signalC -> IO Bool
>undoStateAction3 stateRecordsObject = do
>  sucessMVar <- newEmptyMVar
>  updateIO stateRecordsObject
>    (\(stacks,to,to2,to3)-> do
>     case stacks of
>       (_,(v1,v2,v3):stack1,stack2) -> do
>           updateWithSignal to  (\_ ->v1) (RecorderSignal False Nothing)
>           update           to2 (\_ ->v2)
>           update           to3 (\_ ->v3)
>           putMVar sucessMVar True
>           return ((Nothing,stack1,stack2),to,to2,to3)

>       (_,[],(v1,v2,v3):stack2) -> do
>           updateWithSignal to  (\_ ->v1) (RecorderSignal False Nothing)
>           update           to2 (\_ ->v2)
>           update           to3 (\_ ->v3)
>           putMVar sucessMVar True
>           return ((Nothing,stack2,[]),to,to2,to3)

>       (_,[],[])  -> do
>          putMVar sucessMVar False
>          return ((Nothing,[],[]),to,to2,to3))
>  takeMVar sucessMVar
