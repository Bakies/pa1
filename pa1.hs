{-|
  | This module implements a binary tree that satisfies the properties of a maximum heap.
  | The heap is used as a priority queue, with the highest priority found at the root of
  | the heap. The heap holds patient information, including the patient's name, an 
  | integer priority, and the amount they owe.
  |
  | The module has the expected insert, delete, and peek that works with single patients.
  | In addition, the module supports several other operations to get information about
  | the heap's current properties.
-}

module PA1 where

type Patient = (Name, Priority, Bill)
type Name = String
type Priority = Int
type Bill = Double

-- data type for a binary tree where Patient values are stored at each HeapNode and
-- Tips are valueless leaf node markers
data Heap a = Tip | HeapNode a (Heap a) (Heap a) deriving (Eq, Ord, Show)

-- insert a Patient value into the heap by merging a heap with only the patient with the rest of the heap
insert :: Patient -> Heap Patient -> Heap Patient
insert p h = merge (HeapNode p Tip Tip) h

-- delete the root of the heap and merge the two child nodes
delete :: Heap Patient -> Heap Patient
delete (HeapNode p l r) = merge l r

-- get the priority of a patient
getPriority :: Patient -> Priority
getPriority (_,prio,_) = prio


{- TODO: implement the functions below
-- merge two heaps together while maintaining heap properties
merge :: Heap Patient -> Heap Patient -> Heap Patient

-- return the patient with the highest priority
peek :: Heap Patient -> Patient

-- convert a heap to a list of patients, with the list showing the tree from left to right
heap2InList :: Heap Patient -> [Patient]

-- convert a heap to a list of patients with each node printed before its children
heap2PreList :: Heap Patient -> [Patient]

-- extract all of the patient names in order of top-to-bottom, left-to-right
heap2Names :: Heap Patient -> [Name]

-- return whether the heap has no patients in it
isEmpty :: Heap Patient -> Bool

-- return the height of the heap
height :: Heap Patient -> Int

-- return the heap with a function applied to all Bills in the heap
heapMap :: (Bill -> Bill) -> Heap Patient -> Heap Patient

size :: Heap Patient -> Int
-}