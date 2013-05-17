What is it?
===========
A haskell beginner's very quick, rough implementation of Phi Dinh's 2d dungeon generation algorithm, following his write-up, here:
http://www.reddit.com/r/gamedev/comments/1dlwc4/procedural_dungeon_generation_algorithm_explained/

This is not an example of good haskell practice.

Keys
----
'1' through '9' - toggle visibility of each layer
'Ctrl-1' through 'Ctrl-9' - Snap to grid of that tolerance
'r' - rerandomize the starting rectangles
'g' - turn grid visibility on and off
'Space' - pause/unpause updates
'Cursor Up' / 'Cursor Down' - Inc / Dec the minimum area param used to identify a major room
