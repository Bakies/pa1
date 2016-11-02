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
-- get nothing give nothing
merge Tip Tip = Tip
-- if right heap is empty return left
merge x Tip = x
-- vice versa
merge Tip x = x
merge (HeapNode lp ll lr)  (HeapNode rp rl rr)
      | getPriority lp > getPriority rp =  (HeapNode lp (merge (HeapNode rp rl rr)  lr) ll)
      | getPriority lp <= getPriority rp =  (HeapNode rp (merge (HeapNode lp ll lr) rr) rl)

-- return the patient with the highest priority
peek :: Heap Patient -> Patient
peek Tip = error "Cant peek empty heap"
peek (HeapNode p _ _) = p 

-- convert a heap to a list of patients, with the list showing the tree from left to right
heap2InList :: Heap Patient -> [Patient]
heap2InList Tip = []
-- add patient to list if it has no childrn
heap2InList (HeapNode p Tip Tip) = p:[]
-- take the list from the left node and add it to this patient then add everything on the right
heap2InList (HeapNode p l r) = (heap2InList l) ++ [p] ++ (heap2InList r)


-- convert a heap to a list of patients with each node printed before its children
heap2PreList :: Heap Patient -> [Patient]
-- get nothing, give nothing
heap2PreList Tip = []
-- if patient has no children prepend it to an empty list
heap2PreList (HeapNode p Tip Tip) = p:[]
heap2PreList (HeapNode p l r)   = [p] ++ (heap2PreList l) ++ (heap2PreList r)

-- extract all of the patient names in order of top-to-bottom, left-to-right
heap2Names :: Heap Patient -> [Name]
-- get nothing, give nothing
heap2Names Tip = []
-- if no children add the name to empty list
heap2Names (HeapNode (n, _, _) Tip Tip) = n:[]
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
heapMap _ Tip = Tip 
heapMap func (HeapNode (n, p, b) l r) = (HeapNode (n, p, func b) (heapMap func l) (heapMap func r))

-- just get the size of list of all the patients names
size :: Heap Patient -> Int
size x = length (heap2Names x)