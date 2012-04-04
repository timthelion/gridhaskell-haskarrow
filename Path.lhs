>module Path where

>import Super

| A path to a value. Make it very visual, very clear where things get used.

>data Path = SteppingStone   {point :: Point,
>                             next  :: Path}
>          | PathDestination {point :: Point}
>    deriving (Show, Read)

>destination :: Path -> Point
>destination (SteppingStone _ path)  = destination path
>destination (PathDestination point) = point 
