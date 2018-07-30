module Main where

import Numeric.Natural as Natural
import System.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Data.ViewPort

import Turtle (Action(Walk, Turn))
import Draw (drawTurtleImage, turtlePoints)
import Grammar
import PlantLang

image :: [Action]
image = [Walk 100, Turn (pi/2), Walk 100]

kochRules = [Production F [F, PlantLang.Left, F, PlantLang.Right, F, PlantLang.Right, F, PlantLang.Left, F]]
plant1Rules = [Production F [
                    F, PlantLang.Push, PlantLang.Right, F, PlantLang.Pop, F, PlantLang.Push,
                    PlantLang.Left, F, PlantLang.Pop, PlantLang.Push, F, PlantLang.Pop
                  ]
                ]
plant2Rules = [Production F [
                    F, PlantLang.Push, PlantLang.Left, F, F, PlantLang.Pop, PlantLang.Right, PlantLang.Push,
                    F, F, F, PlantLang.Pop, PlantLang.Left, F, F, PlantLang.Push, PlantLang.Left, F,
                    PlantLang.Left, F, PlantLang.Pop
                  ]
                ]

koch = Genotype 2 (pi/2) kochRules
plant1 = Genotype 2 (pi/8) plant1Rules
plant2 = Genotype 2 (pi/8) plant2Rules
plant3 = crossover (mkStdGen 8284134) plant1 plant2

plantImage :: Natural -> Genotype -> Picture
plantImage growthSteps = drawTurtleImage . growPlant growthSteps 

-- main :: IO ()
-- main = display
--   (InWindow "Nice Window" (200, 200) (10, 10))
--   white
--   (plantImage 2 plant1)

type World = (Genotype, Float, Int, ViewPort, Maybe (Float, Float))

-- TODO only redraw when necessary
render (plant, growth, _, vp, _) = (pure . applyViewPortToPicture vp . plantImage growth) plant

handleInput
    (EventKey (Char 'n') Up _ _)
    (plant, growth, seed, vp, mouseState)
  = pure (
        crossover (mkStdGen seed) plant1 plant2,
        growth,
        seed + 1,
        vp,
        mouseState
      )

handleInput
    (EventKey (Char '+') Up _ _)
    (plant, growth, seed, vp, mouseState)
  = pure (
        plant,
        growth + 1,
        seed,
        vp,
        mouseState
      )

handleInput
    (EventKey (Char '-') Up _ _)
    (plant, growth, seed, vp, mouseState)
  = pure (
        plant,
        if growth > 1 then
          growth - 1
        else
          growth,
        seed,
        vp,
        mouseState
      )

-- zoom and pan
handleInput
    (EventKey (MouseButton WheelDown) _ _ _)
    (plant, growth, seed, ViewPort viewPortTranslate viewPortRotate viewPortScale, mouseState)
  = pure (
        plant,
        growth,
        seed,
        ViewPort viewPortTranslate viewPortRotate (viewPortScale + 1),
        mouseState
      )

handleInput
    (EventKey (MouseButton WheelUp) _ _ _)
    (plant, growth, seed, ViewPort viewPortTranslate viewPortRotate viewPortScale, mouseState)
  = pure (
        plant,
        growth,
        seed,
        ViewPort viewPortTranslate viewPortRotate (viewPortScale - 1),
        mouseState
      )

handleInput
    (EventKey (MouseButton LeftButton) Down _ mousePosition)
    (plant, growth, seed, vp, _)
  = pure (
        plant,
        growth,
        seed,
        vp,
        Just mousePosition
      )

handleInput
    (EventMotion mousePosition@(x', y'))
    (plant, growth, seed, ViewPort (tx, ty) viewPortRotate scale, Just (x, y))
  = pure (
        plant,
        growth,
        seed,
        ViewPort (tx + (x' - x)/scale, ty + (y' - y)/scale) viewPortRotate scale,
        Just mousePosition
      )

handleInput
    (EventKey (MouseButton LeftButton) Up _ _)
    (plant, growth, seed, vp, _)
  = pure (
        plant,
        growth,
        seed,
        vp,
        Nothing
      )

-- event sink
handleInput _ world = pure world

control _ = pure ()

main :: IO ()
main = do
  mapM_ (putStrLn . printGenome) [plant1, plant2, plant3]

  interactIO
    (InWindow "PlantGenerator" (400, 400) (10, 10))
    white
    (plant1, 2, 0, viewPortInit, Nothing)
    render
    handleInput
    control
