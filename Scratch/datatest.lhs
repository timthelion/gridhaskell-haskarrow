>data Foo = Foo {l::Int, myproperty :: String, myotherproperty :: Int}
>         | Bannana {myproperty :: String }
>           deriving (Show)

>f c@Foo{} = myotherproperty c
>f _ = 0
