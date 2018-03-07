
module Achikaps where

-- task: "i want to burn food at the Altar." (x: altar.x, y: altar.y, food: -1, divine favour: +1)
-- task: "haul food to the altar." (x: altar.x, y: altar.y. Relative priorities inherited from the above)
-- task: "convert meat into food at any Kitchen." (meat: -1, food: +1) (how do we mark meat as a non-negative resource? I guess we could just never match against it, but it seems flakey and wasteful... Maybe it's a new Interval type? (IntInterval + CountingInterval))
-- task: "convert meat into food at this specific Kitchen." (x: altar.x, y: altar.y, relatives inherited from the above)
-- task: "forage for berries." (food: +1)
-- task: "forage for berries at this berry bush out in woop woop." (x: bush.x, y: bush.y, inherited)
-- task: "create a new kitchen." (food +.1, civic +1, urgency -1, votes +1)

{-
Task = {
  whoHasClaimedMe :: Maybe Actor,
  prereqs (absolute-value resources),
  outcomes (relative-value resources -- rewards and costs),
  thisIsASubtaskOf :: Maybe Task,
  subtasks :: [Task],
  generateSubtasks :: [Task], -- if we're not ready to execute, then some subtasks get generated instead
  execute :: Command (a Command goes into the game world; the actor is ready to attempt an action)
}
-}

{-
some types of dimension so far:
-- x, y, z spatial. Absolute values. Match interval against interval. Requirement rather than outcome. Inherits: no. Degrades: no.
-- time of day. Just like xyz spatial.
-- reward or cost. Relative value. Always Double or Int (or CountingInterval!). Inherits: yes. Degrades: no.
-- priority. DoubleInterval. Absolute value. Inherits: yeah, we combine the values to inherit it. Degrades: yep.
-- 
-}
