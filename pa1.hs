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


-- TODO: implement the functions below
-- merge two heaps together while maintaining heap properties
merge :: Heap Patient -> Heap Patient -> Heap Patient
merge (HeapNode x l r) Tip = HeapNode x l r 
merge Tip (HeapNode x l r) = HeapNode x l r
merge (HeapNode x x1 x2)  (HeapNode y y1 y2)
      | getPriority x > getPriority y =  (HeapNode x (merge (HeapNode y y1 y2)  x2) x1)
      | getPriority x <= getPriority y =  (HeapNode y (merge (HeapNode x x1 x2) y2) y1)

-- return the patient with the highest priority

peek :: Heap Patient -> Patient
peek (HeapNode p _ _) = p 

-- convert a heap to a list of patients, with the list showing the tree from left to right
heap2InList :: Heap Patient -> [Patient]
heap2InList (HeapNode p Tip Tip) = p:[]
heap2InList (HeapNode p l Tip) = (heap2InList l) ++  [p]
heap2InList (HeapNode p Tip r) =  p : (heap2InList r)
heap2InList (HeapNode p l r) = (heap2InList l) ++ [p] ++ (heap2InList r)


-- convert a heap to a list of patients with each node printed before its children
heap2PreList :: Heap Patient -> [Patient]
heap2PreList (HeapNode p Tip Tip) = p:[]
heap2PreList (HeapNode p l Tip) = [p] ++ (heap2PreList l)
heap2PreList (HeapNode p Tip r) = [p] ++ (heap2PreList r)
heap2PreList (HeapNode p l r)   = [p] ++ (heap2PreList l) ++ (heap2PreList r)

-- extract all of the patient names in order of top-to-bottom, left-to-right
heap2Names :: Heap Patient -> [Name]
heap2Names (HeapNode (n, _, _) Tip Tip) = n:[]
heap2Names (HeapNode (n, _, _) l Tip)   = [n] ++ (heap2Names l)
heap2Names (HeapNode (n, _, _) Tip r)   = [n] ++ (heap2Names r)
heap2Names (HeapNode (n, _, _) l r)     = [n] ++ (heap2Names l) ++ (heap2Names r)

-- return whether the heap has no patients in it
isEmpty :: Heap Patient -> Bool
isEmpty Tip = True
isEmpty _ = False

-- return the height of the heap
height :: Heap Patient -> Int
height Tip = 0 
height (HeapNode p Tip Tip) = 1
height (HeapNode p l Tip) = 1 + height l
height (HeapNode p Tip r) = 1 + height r
height (HeapNode p l r) = 1 + (max (height l) (height r))

-- return the heap with a function applied to all Bills in the heap
heapMap :: (Bill -> Bill) -> Heap Patient -> Heap Patient


size :: Heap Patient -> Int
size x = length (heap2Names x)