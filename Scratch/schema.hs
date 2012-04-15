{-
Basic very high level types:
    - Object
        a thread.
    - Action
        is either safe or unsafe
        - Modifies object.
        - Moves information from one object to another.
    - Event
        
Workflow:
    - Select object type.
        + In this place you can add and edit object types.
    - Select the actions on that object that you want to use in this project.
        + In this place you can add and edit actions.
    - You will now have a list of objects which are used in those actions.
        + This becomes a recursive activity.  You can now select objects and go back to step 2.
    - Eventually you will get a list of all the objects and actions you need.
    - Now layout these objects.
        + Drag and drop :D
-}
Object a
type Actions = [Object a -> Object -> a] [Object b -> Object a] [Object a -> Object -> b]
type ActionObjects = [Object b] -- this is not code, this list contains Objects of many types.
Viewer = View PointerToObject
Viewer -> [Viewer] -> Layout
