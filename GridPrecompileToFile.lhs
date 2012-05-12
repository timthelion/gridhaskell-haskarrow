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

>module GridPrecompileToFile where

>import Grid
>import GridHaskellFile

>import GridPreCompile
>import System.FilePath


>preCompileToFile :: FilePath -> FilePath -> IO ()
>preCompileToFile input output = do
>            gridString <- readFile input

<            print "Reading the grid."

>            mygrid     <- return (openGrid gridString::Grid)

<            print "Precompiling to haskell."

>            writeFile output (code mygrid)

>guessHaskellFileName :: FilePath -> FilePath
>guessHaskellFileName gridHaskellFileName = replaceExtension gridHaskellFileName "hs"
