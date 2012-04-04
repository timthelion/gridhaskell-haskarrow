| This module exists because I needed to resolve cyclicle dependency on Point and Prototype.

>module Super where

| 'Prototype' is a string because I have yet to make it anything different. 

>type Prototype = String

(X,Y)

>type Point   = (Int, Int)

>subtractPoint :: Point -> Point -> Point
>subtractPoint a b = ((fst a) - (fst b),(snd a) - (snd b))

Here we have the version of the whole grid haskell project.

>version :: Float
>version = 0.0
