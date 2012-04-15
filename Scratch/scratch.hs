
main = do print f

f = a ++ b where
 a :: String
 a = "hi!"
 b = "bye"
{-
f0x0 = f1x0 >>= \v0x0 -> f0x1 v0x0
f0x1 p1 = print p1 
f1x0 = getChar >>= \v1x0 -> f2x0 v1x0 v1x0
f2x0 v1x0 p1 =  return '\n'>>= \v2x0 -> f3x0 v1x0 p1 v2x0
f3x0 v1x0 p1 p2 = return ((==) p1 p2) >>= \v3x0 -> f4x0 v1x0 v3x0
f4x0 v1x0 p1 = if p1 then f5x0 v1x0 else f4x1 v1x0
f5x0 v1x0 = return []
f4x1 p1 = return ((:) p1) >>= \v4x1 -> f4x2 v4x1
f4x2 p1 = return f1x0 >>= \v4x2 -> f4x3 p1 v4x2   
f4x3 p1 p2 =  fmap p1 p2


main = f1x0
f1x0 = getChar >>= \v1x0 -> f2x0 v1x0 v1x0 
f2x0 v1x0 p1 = return 'y' >>= \v2x0 -> f3x0 v1x0 p1 v2x0 
f3x0 v1x0 p1  p2 = if (==) p1 p2 then f4x0 v1x0 else f1x0
f4x0 v1x0 = return "Goodbye." >>= \v4x0 -> f5x0 v4x0
f5x0 p1 = print p1 


push return "file"
push return "contents\n"
pull 2 writeFile
-}
