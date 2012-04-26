LGPL copyright brmlab.cz contact timothyhobbs@seznam.cz

>module StateRecords where

>import ThreadObject
>import Control.Concurrent

>data RecorderSignal signal = RecorderSignal Bool (Maybe signal) 

>type StateRecords a signal = ThreadObject ((Maybe a, [a],[a]),(ThreadObject a signal)) ()

>stateRecords :: ThreadObject a signal -> IO (StateRecords a signal)
>stateRecords to = do
> stateRecordsObject <- threadObject
> objectInit stateRecordsObject ((Nothing,[],[]),to) noSyncOnGet noSyncOnPut False
> return stateRecordsObject 

>recordState ::  Int -> StateRecords a signal -> a -> IO ()
>recordState n stateRecordsObject value = do
>  updateIO stateRecordsObject 
>    (\((last,stack1,stack2),to)-> do

     print "length"
     print $ length stack1
     print "n"
     print n

>     if length stack1 >= n
>     then case last of
>       Just last -> do

         print 1

>         return ((Just value,last:[],stack1), to)
>       Nothing   -> do

         print 2

>         return ((Just value,[],stack1), to)
>     else case last of
>       Just last -> do

        print 3;

>        return ((Just value,last:stack1,stack2), to)
>       Nothing   -> do 

        print 4;

>        return ((Just value,stack1,stack2), to))

| This is to undo an action applied the thread object which is used as your "metronome", that thread object, within who's syncOnPut, are the recordState commands.  

>undoStateActionOfRecorder :: StateRecords a (RecorderSignal signal) -> IO Bool
>undoStateActionOfRecorder stateRecordsObject = do
> undoStateAction' stateRecordsObject (\to value -> updateWithSignal to (\_->value) (RecorderSignal False Nothing))


>undoStateAction :: StateRecords a signal -> IO Bool
>undoStateAction stateRecordsObject = do
>  undoStateAction' stateRecordsObject (\to value -> update to (\_->value))

>undoStateAction' :: StateRecords a signal -> (ThreadObject a signal -> a -> IO ()) -> IO Bool
>undoStateAction' stateRecordsObject myUpdate = do
>  updateIOReturning stateRecordsObject
>    (\(stacks,to)-> do
>     case stacks of
>       (Just last,value:stack1,stack2) -> do
>           myUpdate to value

           print "Jv"

>           return (((Nothing,stack1,stack2),to),True)

>       (Nothing,value:stack1,stack2) -> do
>           myUpdate to value

           print "Nv"

>           return (((Nothing,stack1,stack2),to),True)

>       (Just last,[],value:stack2) -> do
>           myUpdate to value

           print "J[]"

>           return (((Nothing,stack2,[]),to),True)

>       (Nothing,[],value:stack2) -> do
>           myUpdate to value

           print "N[]"

>           return (((Nothing,stack2,[]),to),True)

>       (Just last,[],[])  -> do

          print "J[]"

>          return (((Nothing,[],[]),to),False)

>       (Nothing,[],[])  -> do

          print "N[]"

>          return (((Nothing,[],[]),to),False))
