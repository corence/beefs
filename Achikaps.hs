
module Achikaps where

import AchiTask(MapVolume, AchiTask)
import RTree(RTree)

-- task: "i want to burn food at the Altar." (x: altar.x, y: altar.y, food: -1, divine favour: +1)
-- task: "haul food to the altar." (x: altar.x, y: altar.y. Relative priorities inherited from the above)
-- task: "convert meat into food at any Kitchen." (meat: -1, food: +1) (how do we mark meat as a non-negative resource? I guess we could just never match against it, but it seems flakey and wasteful... Maybe it's a new Interval type? (IntInterval + CountingInterval))
-- task: "convert meat into food at this specific Kitchen." (x: altar.x, y: altar.y, relatives inherited from the above)
-- task: "forage for berries." (food: +1)
-- task: "forage for berries at this berry bush out in woop woop." (x: bush.x, y: bush.y, inherited)
-- task: "create a new kitchen." (food +.1, civic +1, urgency -1, votes +1)

type Tasks = RTree MapVolume AchiTask
