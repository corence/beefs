
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
import Data.Maybe(fromJust)

main = hspec $ do
  describe "Scanful" $ do
    describe "findPrecursors" $ do
      it "should find a set of zero-cost tasks that might lead somewhere" $ do
        let highTask = ScanFactors.allTasks standardFactors !! 4
            expectedTasks = [
              Task "fly" (Set.fromList [X]) (Set.fromList [Y])
              ]
            expectedNodes = map (SolutionNode 0 . Just) expectedTasks
            solvedNodes = Scanful.findPrecursors standardFactors highTask
          in solvedNodes `shouldMatchList` expectedNodes

    describe "findCompleteSolutions" $ do
      it "should do thing" $ do
        3 `shouldBe` 4

      it "should scan for a subtask to complete the desired task" $ do
        let victory = Set.fromList [Victory]
        let factors
              = ScanFactors.empty
              & addTask (Task "go east" Set.empty (Set.fromList [X]))
              & addTask (Task "wrap around the world" (Set.fromList [X]) victory)
        Task.name (bestTask factors) `shouldBe` "go east"

      it "should prioritize a cheap task over an expensive task" $ do
        let outcomes = Set.fromList [Victory]
        let factors
              = ScanFactors.empty
              & addTask (Task "build pyramids" (Set.fromList [Item Inventory Fridge]) outcomes)
              & addTask (Task "drink water" (Set.fromList [Item Inventory Oven]) outcomes)
              & addTask (Task "pump iron" (Set.fromList [Item Inventory Food]) outcomes)
              & addPrice (Item Inventory Fridge) 9001
              & addPrice (Item Inventory Oven) 5
              & addPrice (Item Inventory Food) 1006
        Task.name (bestTask factors) `shouldBe` "drink water"

      it "should prioritize a task with a cheap subtask over an expensive one" $ do
        let victory = Set.fromList [Victory]
        let factors
              = ScanFactors.empty
              & addPrice Y 1000
              & addPrice (Item Inventory Fridge) 12
              & addPrice (Item Inventory Food) 120
              & addTask (Task "fly" Set.empty (Set.fromList [Y]))
              & addTask (Task "pump iron" Set.empty (Set.fromList [Item Inventory Fridge]))
              & addTask (Task "gather food" Set.empty (Set.fromList [Item Inventory Food]))
              & addTask (Task "enjoy being high" (Set.fromList [Y]) victory)
              & addTask (Task "enjoy being strong" (Set.fromList [Item Inventory Fridge]) victory)
              & addTask (Task "chow down" (Set.fromList [Item Inventory Food]) victory)
        Task.name (bestTask factors) `shouldBe` "pump iron"
