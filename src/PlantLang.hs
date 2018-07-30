module PlantLang where

import Grammar
import Turtle
import Draw (turtlePoints)

import Graphics.Gloss.Data.Picture (Point)

import Data.Maybe (fromMaybe)
import Numeric.Natural as Natural
import System.Random

data Instruction = F | Left | Right | Push | Pop deriving Eq
data Genotype = Genotype Float Float (Rules Instruction)
type Phenotype = [Point]

type Stack a = [a]

toTurtleActions :: Float -> Float -> [Instruction] -> [Action]
toTurtleActions stepSize turnAngle = map decideAction 
  where
    decideAction :: Instruction -> Action
    decideAction i = case i of
      F -> Walk stepSize
      PlantLang.Left -> Turn turnAngle
      PlantLang.Right -> Turn (-turnAngle)
      PlantLang.Push -> Turtle.Push
      PlantLang.Pop -> Turtle.Pop

growPlant :: Natural -> Genotype -> Phenotype
growPlant growthSteps (Genotype stepSize turnAngle rules) =
  (
        turtlePoints
      . toTurtleActions stepSize turnAngle
      . (!! (fromIntegral growthSteps))
      . iterate (parallelApplication rules)
    ) [F]

highestAndLowest :: Ord a => [a] -> Maybe (a, a)
highestAndLowest = foldl gather Nothing
  where
    gather Nothing x = Just (x, x)
    gather (Just (xmin, xmax)) x = Just (min xmin x, max xmax x)

measureAbsolutePhototropism :: [Point] -> Float
measureAbsolutePhototropism = maximum . map snd

measureRelativePhototropism :: [[Point]] -> [Point] -> Float
measureRelativePhototropism phenotypes phenotype =
    ((measureAbsolutePhototropism phenotype) - lowestPhototropism) / (highestPhototropism - lowestPhototropism)
  where
    (highestPhototropism, lowestPhototropism) = fromMaybe
      (0, 0)
      ((highestAndLowest . map measureAbsolutePhototropism) phenotypes)

measureBilateralSymmetry :: [Point] -> Float
measureBilateralSymmetry points = leftOfCenter / rightOfCenter
  where
    (leftMostPoint, rightMostPoint) = fromMaybe (0, 0) ((highestAndLowest . map fst) points)

    xCenter = (rightMostPoint - leftMostPoint) / 2

    (leftOfCenter, rightOfCenter) = foldl count (0, 0) points
      where
        count (l, r) (x, _)
          | x < xCenter = (l + 1, r)
          | x > xCenter = (l, r + 1)

genFitnessFun :: [[Point]] -> (Float, Float) -> [Point] -> Float
genFitnessFun phenotypes (phototropismWeight, symmetryWeight) currentPhenotype = 
    (phototropismWeight * phototropism + symmetryWeight * symmetry) /
      (phototropism + symmetry)
  where
    measurePhototropism = measureRelativePhototropism phenotypes

    phototropism = measurePhototropism currentPhenotype
    symmetry = measureBilateralSymmetry currentPhenotype

type Layer = [[Instruction]]

hierarchy :: Genotype -> Stack Layer
hierarchy (Genotype _ _ [Production F is]) =
    let
      (baseInstructions, subHierarchies, _) = idBaseAndSubHierarchies [] [] is
    in
      mergeStacks baseInstructions subHierarchies
  where
    idBaseAndSubHierarchies :: [Instruction] -> [Stack Layer] -> [Instruction] -> ([Instruction], [Stack Layer], [Instruction])
    idBaseAndSubHierarchies prefix collectedStacks (PlantLang.Push:is) =
      let
        (subBaseString, subStacks, remainingIs) = idBaseAndSubHierarchies [] [] is
        subStack = mergeStacks subBaseString subStacks
      in
        idBaseAndSubHierarchies (prefix ++ [PlantLang.Push]) (subStack:collectedStacks) remainingIs

    idBaseAndSubHierarchies prefix collectedStacks (PlantLang.Pop:is)  = (prefix, collectedStacks, is)
    idBaseAndSubHierarchies prefix collectedStacks (i:is)    = idBaseAndSubHierarchies (prefix ++ [i]) collectedStacks is
    idBaseAndSubHierarchies prefix collectedStacks []        = (prefix, collectedStacks, [])

    mergeStacks :: [Instruction] -> [Stack Layer] -> Stack Layer
    mergeStacks baseString subStacks = [baseString]:(foldl mergeStacks' [] subStacks)
      where
        mergeLayers :: Layer -> Layer -> Layer
        mergeLayers = (++)

        mergeStacks' :: Stack Layer -> Stack Layer -> Stack Layer
        mergeStacks' [] stack          = stack
        mergeStacks' stack []          = stack
        mergeStacks' (l1:ls1) (l2:ls2) = (mergeLayers l1 l2):(mergeStacks' ls1 ls2)

hierarchy _ = []

crossover :: StdGen -> Genotype -> Genotype -> Genotype
crossover gen p1@(Genotype stepSize turnAngle _) p2 =
    let
      mainProductionExpr = crossover' gen (hierarchy p1) (hierarchy p2)
    in
      Genotype stepSize turnAngle [Production F mainProductionExpr]
  where
    crossover' :: StdGen -> Stack Layer -> Stack Layer -> [Instruction]
    crossover' gen [] p2 = singleParent gen p2
    crossover' gen p1 [] = singleParent gen p1
        
    crossover' gen p1@(_:p1') p2@(_:p2') =
      let
        g1:g2:g3:gs = (map mkStdGen . randoms) gen

        dominantParent = selectParent g1 p1 p2
        instructions = selectInstructions g2 dominantParent
      in
        resolve g3 p1' p2' instructions

    singleParent :: StdGen -> Stack Layer -> [Instruction]
    singleParent gen p@(l:ls)
        | continue  = (resolve g2 ls [] . selectInstructions g3) p
        | otherwise = []
      where
        g1:g2:g3:gs = (map mkStdGen . randoms) gen
        (continue, _) = random g1 :: (Bool, StdGen)

    resolve :: StdGen -> Stack Layer -> Stack Layer -> [Instruction] -> [Instruction]
    resolve _ _ _ []                      = []
    resolve gen p1 p2 (PlantLang.Push:is) = (PlantLang.Push:(crossover' g1 p1 p2)) ++ (PlantLang.Pop:(resolve g2 p1 p2 is))
      where
        g1:g2:gs = (map mkStdGen . randoms) gen
    resolve gen p1 p2 (i:is) = i:(resolve gen p1 p2 is)

    selectParent :: StdGen -> Stack Layer -> Stack Layer -> Stack Layer
    selectParent gen s s' =
      let
        (parentSelection, _) = random gen
      in
        if parentSelection then
          s
        else
          s'

    selectInstructions :: StdGen -> Stack Layer -> [Instruction]
    selectInstructions _ [] = []
    selectInstructions gen (l:_)
        | null l          = []
        | otherwise       = ((!!) l . (`mod` (length l)) . abs) instructionsIdx
      where
        (instructionsIdx, _) = random gen

printGenome :: Genotype -> String
printGenome (Genotype _ _ [Production F is]) = map printInstruction is
  where
    printInstruction :: Instruction -> Char
    printInstruction F = 'F'
    printInstruction PlantLang.Push = '['
    printInstruction PlantLang.Pop = ']'
    printInstruction PlantLang.Right = '+'
    printInstruction PlantLang.Left = '-'
    printInstruction _ = '?'
printGenome _ = []
