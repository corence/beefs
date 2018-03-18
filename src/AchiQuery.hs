
module AchiQuery where

import AchiTask(MapVolume)

-- todo: weights somehow
data Query = Query {
  prerequisites :: MapVolume,
  outcomes :: MapVolume
  }
