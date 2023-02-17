module Util
    ( pathListHeight
    ) where


-- No of lines for path list given terminal height
pathListHeight :: Int -> Int
pathListHeight h = h - 4
