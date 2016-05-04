{-# LANGUAGE OverloadedStrings #-}

module InitData (vehicleTbl) where

import Types

import qualified Data.IntMap.Lazy as IM (IntMap, fromList)


vehicles :: [Vehicle]
vehicles = [ Vehicle "vin0" 2016 "M. Plus" []
           , Vehicle "vin1" 2015 "Void"    [ Issue Battery    Low
                                           , Issue Electrical High
                                           , Issue Brakes     Med ]
           , Vehicle "vin2" 2014 "Pure"    [] ]


vehicleTbl :: IM.IntMap Vehicle
vehicleTbl = IM.fromList . zip [0..] $ vehicles
