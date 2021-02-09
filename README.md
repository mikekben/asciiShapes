ASCII Shapes - Ben Mikek
=========================================
Tutorial:

Draw a circle with r=5: ``drawWide (Circle 5)``
Draw two right triangles beside each other: ``drawWide (Beside (RightTriangle 7 7) (RightTriangle 7 7) BMiddle)``
A snowman (check out the examples section of shapes.hs to see how it is constructed): ``drawWide snowman``
A shape vaguely resembling a dog: ``drawWide dog``
A Sierpinski triangle: ``draw (sierpinski 7 5)``

``draw`` produces an IO in which each unit represents one character; so ``draw (Square 5)`` produces a 5 x 5 grid. This function is recommended for use with square-character fonts.
``drawWide`` doubles the width of the output, so that ``draw (Square 5)`` produces a 10 x 5 grid which looks like more of a square on most terminal fonts.



-----------------------------------------
Existing functionality:
  + Basic shapes:
    + Rectangle width height
    + RightTriangle width height
    + CenterTriangle sideLength
    + Circle radius
  + Basic relations:
    + Beside realtions:
      + Top
      + Middle
      + Bottom
    + Above relations:
      + Left
      + Middle
      + Right
  + Basic transformations:
    + Vertical flip (flipV)
    + Horizontal flip (flipH)

-----------------------------------------
In progress functionality:
  + Lines
-----------------------------------------
Planned functionality:
  + Overlays
  + Outlines
  + Rotation
  + n-gons