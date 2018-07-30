module Draw where

import Graphics.Gloss.Data.Picture (Picture(Line), Point, Vector)

import Turtle (Action(Walk, Turn, Push, Pop))

(.*) :: Num a => a -> (a, a) -> (a, a)
(.*) x (v1, v2) = (x*v1, x*v2)

(|+|) :: Num a => (a, a) -> (a, a) -> (a, a)
(|+|) (x1, y1) (x2, y2) = (x1+x2, y1+y2)

drawTurtleImage :: [Point] -> Picture
drawTurtleImage = Line

turtlePoints :: [Action] -> [Point]
turtlePoints = map snd' . scanl drawAction start
  where
    snd' (_, x, _) = x

    drawAction :: ([(Point, Vector)], Point, Vector) -> Action -> ([(Point, Vector)], Point, Vector)
    drawAction (stack, position, direction) (Walk d) =
      (
          stack,
          position |+| (d .* direction),
          direction
        )

    drawAction (stack, position, (vx, vy)) (Turn a) =
      (
          stack,
          position,
          (
              cos a * vx - sin a * vy,
              sin a * vx + cos a * vy
            )
        )

    drawAction ((position,direction):ps, _, _) Pop =
      (
          ps,
          position,
          direction
        )

    drawAction ([], position, direction) Pop =
      (
          [],
          position,
          direction
        )

    drawAction (stack, position, direction) Push =
      (
          (position,direction):stack,
          position,
          direction
        )

    start = ([], (0, 0), (0, 1))
