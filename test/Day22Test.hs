module Day22Test where

import Test.HUnit (test, (~:), (~=?))
import Day22 (add, remove, splitSegments, Segment(..))


simplify :: [Segment] -> [Segment]
simplify [] = []
simplify [Unit] = [Unit]
simplify (Segment a b [] : rest) = simplify rest
simplify [Segment a b sub] = [Segment a b (simplify sub)]
simplify (a@(Segment a1 a2 subA) : b@(Segment b1 b2 subB) : rest) =
    if a2 == b1 && subA' == subB'
        then simplify (Segment a1 b2 subA' : rest)
        else Segment a1 a2 subA' : simplify (Segment b1 b2 subB' : rest)
    where
        subA' = simplify subA
        subB' = simplify subB
simplify _ = error "unhaldled case in simplify"

addS s s1 = simplify $ s `add` s1
removeS s s1 = simplify $ s `remove` s1


seg1 a b = Segment a b [Unit]
segs1 = [seg1 20 30]

testsAddOneDimension =   [ [seg1 5 19, seg1 20 30]  ~=?  segs1 `addS` seg1 5 19
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

testsRemoveOneDimension = [ [seg1 20 30]  ~=?  segs1 `removeS` seg1 5 19
                          , [seg1 20 30]  ~=?  segs1 `removeS` seg1 5 20
                          , [seg1 21 30]  ~=?  segs1 `removeS` seg1 5 21
                          , [seg1 29 30]  ~=?  segs1 `removeS` seg1 5 29
                          , []  ~=?  segs1 `removeS` seg1 5 30
                          , []  ~=?  segs1 `removeS` seg1 5 31

                          -- , [seg1 19 30]  ~=?  segs1 `removeS` seg1 19 20
                          -- , [seg1 19 30]  ~=?  segs1 `removeS` seg1 19 21
                          -- , [seg1 19 30]  ~=?  segs1 `removeS` seg1 19 29
                          -- , [seg1 19 30]  ~=?  segs1 `removeS` seg1 19 30
                          -- , [seg1 19 31]  ~=?  segs1 `removeS` seg1 19 31

                          -- , [seg1 20 30]  ~=?  segs1 `removeS` seg1 20 21
                          -- , [seg1 20 30]  ~=?  segs1 `removeS` seg1 20 29
                          -- , [seg1 20 30]  ~=?  segs1 `removeS` seg1 20 30
                          -- , [seg1 20 31]  ~=?  segs1 `removeS` seg1 20 31

                          -- , [seg1 20 30]  ~=?  segs1 `removeS` seg1 21 29
                          -- , [seg1 20 30]  ~=?  segs1 `removeS` seg1 21 30
                          -- , [seg1 20 31]  ~=?  segs1 `removeS` seg1 21 31
                         
                          -- , [seg1 20 30]  ~=?  segs1 `removeS` seg1 29 30
                          -- , [seg1 20 31]  ~=?  segs1 `removeS` seg1 29 31

                          -- , [seg1 20 31]  ~=?  segs1 `removeS` seg1 30 31
                          -- , [seg1 20 32]  ~=?  segs1 `removeS` seg1 30 32
                         
                          -- , [seg1 20 30, seg1 31 32]  ~=?  segs1 `removeS` seg1 31 32
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


tests = test $ testsAddOneDimension ++ testsRemoveOneDimension ++ testsSplitSegments
