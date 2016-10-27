-- provides some tests for COMP3071 PA1, the triage priority queue
-- see the bottom for 
module PA1_tests where

import PA1

h0 = Tip
h1 = HeapNode ("Al",20,50.0) (HeapNode ("Bette",18,45.0) (HeapNode ("Camille",5,40.0) Tip Tip) (HeapNode ("Don",6,45.0) Tip Tip)) (HeapNode ("Ephram",10,60.0) Tip Tip)
p2 = ("Zag",50,30.55)
p3 = ("Yao",30,2000.0)
p4 = ("Xi",70,9)
p5 = ("Wil",10,359.99)

pl = [p2, p3, p4, p5]

-- helper function for insertion
testIns :: [Patient] -> Heap Patient
testIns [p] = insert p Tip
testIns (p:ps) = insert p (testIns ps)

testInsert = testIns pl

-- helper function for deletions
testDel :: Int -> Heap Patient -> Heap Patient
testDel 2 h = h
testDel x h = testDel (x-1) (delete h)

testDelete = testDel 5 h1

testPeek = peek h1

testHeap2InList = heap2InList (insert p4 (insert p3 (insert p2 h0)))

testHeap2PreList = heap2PreList (insert p4 (insert p3 (insert p2 h0)))

testHeap2Names = heap2Names h1

testEmptyTrue = isEmpty h0
testEmptyFalse = isEmpty h1

testHeight = height h1

testHeapMap = heapMap (\x -> 2 * x + 2) h1

testSize = size h1


{- GHCi session inputs and outputs:
Prelude> :l pa1_tests.hs
[1 of 2] Compiling PA1              ( PA1.hs, interpreted )
[2 of 2] Compiling PA1_tests        ( pa1_tests.hs, interpreted )
Ok, modules loaded: PA1_tests, PA1.
*PA1_tests> testInsert
HeapNode ("Xi",70,9.0) (HeapNode ("Zag",50,30.55) (HeapNode ("Wil",10,359.99) Tip Tip) Tip) (HeapNode ("Yao",30,2000.0) Tip Tip)
*PA1_tests> testDelete
HeapNode ("Don",6,45.0) (HeapNode ("Camille",5,40.0) Tip Tip) Tip
*PA1_tests> testPeek
("Al",20,50.0)
*PA1_tests> testHeap2InList
[("Yao",30,2000.0),("Zag",50,30.55),("Xi",70,9.0)]
*PA1_tests> testHeap2PreList
[("Xi",70,9.0),("Zag",50,30.55),("Yao",30,2000.0)]
*PA1_tests> testHeap2Names
["Al","Bette","Camille","Don","Ephram"]
*PA1_tests> testEmptyTrue
True
*PA1_tests> testEmptyFalse
False
*PA1_tests> testHeight
3
*PA1_tests> testHeapMap
HeapNode ("Al",20,102.0) (HeapNode ("Bette",18,92.0) (HeapNode ("Camille",5,82.0) Tip Tip) (HeapNode ("Don",6,92.0) Tip Tip)) (HeapNode ("Ephram",10,122.0) Tip Tip)
*PA1_tests> testSize
5
 -}
