
import Test.Hspec
import Test.QuickCheck
import qualified ScanFactors
import ScanFactors(ScanFactors)
import qualified Task
import Task(Task(..))
import SimsKeys
import qualified Scanful
import qualified SolutionNode
import SolutionNode(SolutionNode)
import TestFunctions
import Data.Function((&))
import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

main = hspec $ do
  describe "scanful" $ do
    it "should do thing" $ do
      3 `shouldBe` 4

    it "should scan for a subtask to complete the desired task" $ do
      let victory = Set.fromList [Victory]
      let factors
            = ScanFactors.empty
            & addTask (Task "go east" Set.empty (Set.fromList [X]))
            & addTask (Task "wrap around the world" (Set.fromList [X]) victory)
      let bestNode
            = Scanful.findCompleteSolutions factors Victory
            & Map.toList
            & head
            & snd
      Task.name (SolutionNode.task bestNode) `shouldBe` "go east"

    it "should prioritize a cheap task over an expensive task" $ do
      let outcomes = Set.fromList [Victory]
      let factors
            = ScanFactors.empty
            & addCostedTask 1000 (Task "build pyramids" Set.empty outcomes)
            & addCostedTask 12   (Task "drink water" Set.empty outcomes)
            & addCostedTask 120  (Task "pump iron" Set.empty outcomes)
      let bestNode
            = Scanful.findCompleteSolutions factors Victory
            & Map.toList
            & head
            & snd
      Task.name (task bestNode) `shouldBe` "drink water"

    it "should prioritize a task with a cheap subtask over an expensive one" $ do
      let victory = Set.fromList [Victory]
      let factors
            = ScanFactors.empty
            & addCostedTask 1000 (Task "fly" Set.empty (Set.fromList [Y]))
            & addCostedTask 12   (Task "pump iron" Set.empty (Set.fromList [Item Inventory Fridge]))
            & addCostedTask 120  (Task "gather food" Set.empty (Set.fromList [Item Inventory Food]))
            & addTask (Task "enjoy being high" (Set.fromList [Y]) victory)
            & addTask (Task "enjoy being strong" (Set.fromList [Item Inventory Fridge]) victory)
            & addTask (Task "chow down" (Set.fromList [Item Inventory Food]) victory)
      let bestNode
            = Scanful.findCompleteSolutions factors Victory
            & Map.toList
            & head
            & snd
      Task.name (task bestNode) `shouldBe` "pump iron"
