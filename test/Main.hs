{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE NamedFieldPuns         #-}
module Main(main) where

import Control.Applicative
import Control.Monad.Identity
import Data.Foldable(asum)
import Data.Tree

import Control.Monad.State.LGBT

-- | Laboratory maze will be defined by just @String@ labels in a tree...
type Maze = Tree String

-- | We will need to remember how many motivating @Raisins@ our meerkat still has.
type Raisins = Int

-- | We define
type Path = [String]

type MeerkatM a = LGBT Local Global Identity a

-- | First maze is relatively easy, but since Merryssa is very liberal,
--   she will probably choose the leftist path until proven that it goes nowhere.
maze :: Maze
maze  = Node "top" [Node "righty" [peanoNode 4
                                  ,Node "lefty" [Node "Deeper" [Node "FINISH" []]
                                                ,peanoNode 100]
                                  ]
                   ]

-- | Second maze has no exit, so probably results in the boring end.
maze2 :: Maze
maze2  = Node "top" [Node "righty" [peanoNode 4
                                   ,Node "lefty" [Node "Deeper" [Node "Eternal" []]
                                                 ,peanoNode 100]
                                   ]
                    ]

-- | Make a deep dead-end tunnel to the ends of Peano-land.
peanoNode :: Int -> Maze
peanoNode 0 = Node "zero" [               ]
peanoNode i = Node "succ" [peanoNode $ i-1]

-- | Global state keeps track of how long the meerkat will still walk...
data Global = Global { food    :: Raisins }
-- | Local state keeps track of where Merryssa currently is.
data Local  = Local  { path    :: Path
                     , subMaze :: Maze    }

-- | Apply a given action on @Global@ variable
withRaisins :: (Raisins -> Raisins) -> Global -> Global
withRaisins f (Global x) = Global $ f x

type MazeM = LGBT Local Global Identity [String]

-- | Merryssa the Meerkat tries to find her way inside the forest maze...
--   but she has limited amount of food.
meerkat :: MazeM
meerkat = do
  current <- getsLocal $ rootLabel . subMaze
  modifyLocal $ \Local{..} -> Local { path=current:path, .. }
  remainingFood <- getsGlobal food
  when (remainingFood == 0) $ fail "No more raisins!"
  if current == "FINISH"
    then  reverse            <$> getsLocal  path -- ^ Return the path to finish
    else (asum . map stepTo) =<< getsLocal (subForest . subMaze)
  where
    stepTo    :: Maze -> MazeM
    stepTo new = do
      eat
      modifyLocal  $ \Local {..} -> Local { subMaze = new, .. }
      meerkat

-- | Eaten breadcrumbs do not renew, so they are part of global state.
eat :: MeerkatM ()
eat  = modifyGlobal $ withRaisins (-1+) -- eat food

-- | Final outcome of the search can be either lucky
--   or extremely tragic.
data Result = Bored          -- ^ Ran out of food, and phased out of forest maze
            | Asleep  Int    -- ^ Went through entire maze, and found nothing
            | Escaped Int    -- ^ Remaining food
                    [String] -- ^ Path
  deriving (Show)

experiment :: Int -> Maze -> Result
experiment givenFood theMaze =
    extract     $
    runIdentity $
    runLGBT  meerkat Local  { subMaze = theMaze,   path = [] }
                     Global         { food      = givenFood  }
  where
    extract (Left         _ , Global { food=0 }) = Bored
    extract (Left         _ , Global { food   }) = Asleep  food
    extract (Right (path, _), Global { food   }) = Escaped food path

test :: Int -> Maze -> IO ()
test someFood aMaze = do
    putStr $ "Testing with " ++ show someFood ++ " " -- ++ "on:\n" ++ drawTree maze
    print  $ experiment someFood aMaze

-- | Test the backtracing on two different forest mazes.
main :: IO ()
main  = do
  test 1   maze
  test 10  maze
  test 100 maze
  test 1   maze2
  test 10  maze2
  test 200 maze2

