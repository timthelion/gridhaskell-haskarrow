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


| This module exists because I needed to resolve cyclicle dependency on Point and Prototype.

>module Super where

| 'Prototype' is a string because I have yet to make it anything different. 

>type Prototype = String

(X,Y)

>type Point   = (Int, Int)

>type Nat     = Int -- :( Will wait for better natural number support.

(Width,Heigth)

>type Size    = (Nat, Nat)

>type Rectangle = (Point, Size)

Here we have the version of the whole grid haskell project.

>version :: Float
>version = 0.0

>rectanglePoint :: Rectangle -> Point
>rectanglePoint (point,_) = point

|This is the smallest Size a Rectangle can have.

>small :: Size
>small = (1,1)
