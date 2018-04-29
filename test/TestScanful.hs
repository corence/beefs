
import Data.Bifunctor
import Test.Hspec
import Test.QuickCheck
import qualified ScanFactors
import ScanFactors(ScanFactors)
import qualified Task
import Task(Task(..))
import SimsKeys
import qualified Scanful
import qualified SolutionNode
import SolutionNode(SolutionNode(..))
import TestFunctions
import Data.Function((&))
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Debug.Trace
import qualified Data.Map.Lazy as LMap

type LMap = LMap.Map

main = hspec $ do
  describe "Scanful" $ do
    describe "findPrecursors" $ do
      it "should find a set of zero-cost tasks that might lead somewhere" $ do
        let highTask = ScanFactors.allTasks standardFactors !! 2
            highNode = Scanful.taskToNode standardFactors highTask
            expectedTasks = [
              Task "fly" (Set.fromList []) (Set.fromList [Y])
              ]
            expectedNodes = map (SolutionNode 1000) expectedTasks
            solvedNodes = Scanful.findPredecessors standardFactors highNode
          in solvedNodes `shouldMatchList` expectedNodes

      it "should not need any follow-on tasks if the needs can all be solved by direct costs" $ do
        let highTask = ScanFactors.allTasks standardFactors !! 4
            highNode = Scanful.taskToNode standardFactors highTask
            solvedNodes = Scanful.findPredecessors standardFactors highNode
          in solvedNodes `shouldMatchList` []


    describe "findCompleteSolutions" $ do
      it "should scan for a subtask to complete the desired task" $ do
        let victory = Set.fromList [Victory]
        let factors
              = ScanFactors.empty
              & addTask (Task "go east" Set.empty (Set.fromList [X]))
              & addTask (Task "wrap around the world" (Set.fromList [X]) victory)
        Task.name (bestTask factors) `shouldBe` "go east"

      it "should prioritize solutions" $ do
        let bests = Scanful.findCompleteSolutions standardFactors Victory & Map.toList
        let besties = map (second (Task.name . SolutionNode.task)) bests
        besties `shouldBe` [(12.0, "pump iron"), (120.0, "gather food"), (1000.0, "fly")]

      describe "deep non-branching tasks" $ do
        let deepTasks
              = [ (0, makeTask "serve dinner" [Item Inventory Food] [Victory])
                , (6, makeTask "bake"         [Item Inventory ChoppedIngredients, Item Floor Oven, Delay] [Item Inventory Food])
                , (1, makeTask "grab chopped" [Item Floor ChoppedIngredients, Delay] [Item Inventory ChoppedIngredients])
                , (1, makeTask "chop"         [Item Floor RawIngredients, Delay] [Item Floor ChoppedIngredients])
                , (0, makeTask "drop ings"    [Item Inventory RawIngredients] [Item Floor RawIngredients])
                , (8, makeTask "buy ings"     [Item Floor Fridge, Cash] [Item Inventory RawIngredients])
                ]

        let deepFactors
              = foldr (addTask . snd) ScanFactors.empty deepTasks
              & addPrice (Item Floor Oven) 5
              & addPrice (Item Floor Fridge) 5
              & addPrice Delay 1
              & addPrice Cash 3

        -- findAnswersStep :: ScanFactors -> (LMap Double SolutionNode, [SolutionNode]) -> Maybe (LMap Double SolutionNode, [SolutionNode])
        --it "should step" $ do
          --let nodes = LMap.fromList [()]
          --findAnswersStep deepFactors

        it "should be able to go n levels deep and ignore the others" $
          property $ forAll (choose (0, length deepTasks - 1)) $ \index -> do
            let expectedCost
                  = deepTasks
                  & drop index
                  & map fst
                  & sum
            let expectedTask
                  = deepTasks !! (length deepTasks - 1)
                  & snd
                  & \task -> task { needs = Set.empty }
            let outcomes
                  = deepTasks !! index
                  & snd
                  & Task.outcomes
            let outcome
                  = outcomes
                  & Set.toList
                  & head
            let bestNode
                  = trace "------->>>>" outcome
                  & traceShowId
                  & Scanful.fA deepFactors
                  & traceShowId
                  & trace "<<<<--------" head
            bestNode `shouldBe` SolutionNode expectedCost expectedTask

        it "should go many levels deep, accumulating costs" $ do
          let bestNode = Scanful.fA deepFactors Victory & head
          bestNode `shouldBe` SolutionNode 16 (makeTask "buy ings" [] [Item Inventory RawIngredients])

      it "should do some really basic branching" $ do
        let tasks
              = [ makeTask "serve dinner" [Item Inventory Food, Delay] [Victory]
                , makeTask "punch fridge" [Item Floor Fridge] [Victory]
                , makeTask "buy oven"     [Cash] [Victory]
                ]

        let factors
              = foldr addTask ScanFactors.empty tasks
              & addPrice (Item Inventory Food) 15
              & addPrice (Item Floor Fridge) 5
              & addPrice Delay 1
              & addPrice Cash 8

        let bestNode = Scanful.fA factors Victory & head
        bestNode `shouldBe` SolutionNode 5 (makeTask "punch fridge" [] [Victory])

      it "should combine priced needs" $ do
        let tasks = [ makeTask "serve dinner" [Item Inventory Food, Delay] [Victory] ]

        let factors
              = foldr addTask ScanFactors.empty tasks
              & addPrice (Item Inventory Food) 15
              & addPrice Delay 1

        let bestNode = Scanful.fA factors Victory & head
        bestNode `shouldBe` SolutionNode 16 (makeTask "serve dinner" [] [Victory])

      describe "branching solutions" $ do
        let branchingTasks
              = [ (0, makeTask "serve dinner" [Item Inventory Food] [Victory])
                , (6, makeTask "bake"         [Item Inventory ChoppedIngredients, Item Floor Oven, Delay] [Item Inventory Food])
                , (1, makeTask "grab chopped" [Item Floor ChoppedIngredients, Delay] [Item Inventory ChoppedIngredients])
                , (1, makeTask "chop"         [Item Floor RawIngredients, Delay] [Item Floor ChoppedIngredients])
                , (0, makeTask "drop ings"    [Item Inventory RawIngredients] [Item Floor RawIngredients])
                , (8, makeTask "buy ings"     [Item Floor Fridge, Cash] [Item Inventory RawIngredients])
                ]

        let branchingFactors
              = foldr (addTask . snd) ScanFactors.empty branchingTasks
              & addPrice (Item Floor Oven) 5
              & addPrice (Item Floor Fridge) 5
              & addPrice Delay 1
              & addPrice Cash 3


        it "should traverse between multiple branching options, accumulating costs" $ do
          "todo" `shouldBe` "implemented"

      it "should NOT prioritize solutions that solve multiple needs at once" $ do
        let multiSolvingTasks
              = [ makeTask "invent electricity" [Item Inventory Fridge, Item Inventory Oven, Item Inventory Food] [Victory]
                , makeTask "buy starter kit" [Cash, Delay, X, Y] [Item Inventory Fridge, Item Inventory Oven, Item Inventory Food]
                , makeTask "buy fridge" [Cash, Delay] [Item Inventory Fridge]
                , makeTask "grow food" [Delay, X] [Item Inventory Food]
                , makeTask "retrieve oven" [X, Y] [Item Inventory Oven]
                ]
        let multiSolvingFactors
              = foldr addTask ScanFactors.empty multiSolvingTasks
              & addPrice Cash 1
              & addPrice Delay 1
              & addPrice X 1
              & addPrice Y 1

        let bestNode = Scanful.fA multiSolvingFactors Victory & head
        bestNode `shouldBe` SolutionNode 8 (makeTask "buy ings" [] [Item Inventory RawIngredients])
