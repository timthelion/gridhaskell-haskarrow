GPLV3.0 or later copyright brmlab.cz contact timothyhobbs@seznam.cz

Copyright 2012.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

>pathPointFilled :: Path -> Point -> Bool
>pathPointFilled (SteppingStone point next) pointToCheck =
> (point == pointToCheck) ||
> pathPointFilled next pointToCheck

>pathPointFilled (PathDestination point) pointToCheck =
> point == pointToCheck
