module Main where

import Data.Time (getCurrentTime, diffUTCTime)
import Text.Printf

-- Definition of the TreeNode data type
data TreeNode = Leaf Int
              | Node Int TreeNode TreeNode

-- Function to create a new TreeNode
newTreeNode :: Int -> Maybe TreeNode -> Maybe TreeNode -> TreeNode
newTreeNode value Nothing Nothing = Leaf value
newTreeNode value (Just left) Nothing = Node value left (Leaf 0)
newTreeNode value Nothing (Just right) = Node value (Leaf 0) right
newTreeNode value (Just left) (Just right) = Node value left right

-- Function to calculate the maximum sum from root to any leaf
maxSum :: TreeNode -> Int
maxSum (Leaf value) = value
maxSum (Node value left right) = value + max (maxSum left) (maxSum right)

main :: IO ()
main = do
  start <- getCurrentTime

  -- Create sample tree
  let sampleTree = newTreeNode 17 (Just (newTreeNode 3 (Just (newTreeNode 2 Nothing Nothing)) Nothing))
                                  (Just (newTreeNode (-10) (Just (newTreeNode 16 Nothing Nothing))
                                                           (Just (newTreeNode 1 (Just (newTreeNode 13 Nothing Nothing)) Nothing))))

  -- Calculate the max sum of any one tree path
  printf "Max Sum: %d\n" (maxSum sampleTree)
  
  end <- getCurrentTime
  print (diffUTCTime end start)
