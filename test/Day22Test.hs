module Day22Test where

import Test.HUnit (test, (~:), (~=?))
import Day22 (addS, splitSegments, Segment(..))

seg1 a b = Segment a b [Unit]
segs1 = [seg1 20 30]

testsAdd =   [ [seg1 5 19, seg1 20 30]  ~=?  segs1 `addS` seg1 5 19
             , [seg1 5 30]  ~=?  segs1 `addS` seg1 5 20
             , [seg1 5 30]  ~=?  segs1 `addS` seg1 5 21
             , [seg1 5 30]  ~=?  segs1 `addS` seg1 5 29
             , [seg1 5 30]  ~=?  segs1 `addS` seg1 5 30
             , [seg1 5 31]  ~=?  segs1 `addS` seg1 5 31

             , [seg1 19 30]  ~=?  segs1 `addS` seg1 19 20
             , [seg1 19 30]  ~=?  segs1 `addS` seg1 19 21
             , [seg1 19 30]  ~=?  segs1 `addS` seg1 19 29
             , [seg1 19 30]  ~=?  segs1 `addS` seg1 19 30
             , [seg1 19 31]  ~=?  segs1 `addS` seg1 19 31

             , [seg1 20 30]  ~=?  segs1 `addS` seg1 20 21
             , [seg1 20 30]  ~=?  segs1 `addS` seg1 20 29
             , [seg1 20 30]  ~=?  segs1 `addS` seg1 20 30
             , [seg1 20 31]  ~=?  segs1 `addS` seg1 20 31

             , [seg1 20 30]  ~=?  segs1 `addS` seg1 21 29
             , [seg1 20 30]  ~=?  segs1 `addS` seg1 21 30
             , [seg1 20 31]  ~=?  segs1 `addS` seg1 21 31
             
             , [seg1 20 30]  ~=?  segs1 `addS` seg1 29 30
             , [seg1 20 31]  ~=?  segs1 `addS` seg1 29 31

             , [seg1 20 31]  ~=?  segs1 `addS` seg1 30 31
             , [seg1 20 32]  ~=?  segs1 `addS` seg1 30 32
             
             , [seg1 20 30, seg1 31 32]  ~=?  segs1 `addS` seg1 31 32

             ]

segs2 = [seg1 10 20, seg1 30 40]

testsSplitSegments = 
    [ segs1  ~=?  splitSegments segs1 []
    , []  ~=?  splitSegments [] [1,2,3]
    , segs1  ~=?  splitSegments segs1 [18,19,20]
    , segs1  ~=?  splitSegments segs1 [30,31,32]

    , [seg1 20 21, seg1 21 29, seg1 29 30]  ~=?  splitSegments segs1 [19,20,21,29,30,31]

    , segs2  ~=?  splitSegments segs2 []
    , segs2  ~=?  splitSegments segs2 [9,10,20,21,29,30,40,41]

    , [seg1 10 11, seg1 11 19, seg1 19 20, seg1 30 31, seg1 31 39, seg1 39 40]  ~=?  splitSegments segs2 [9,10,11,19,20,21,29,30,31,39,40,41]
    ]


tests = test $ testsAdd ++ testsSplitSegments
