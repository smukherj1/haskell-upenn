
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.List

isDigit :: Char -> Bool
isDigit c = any (c==) ['0'..'9']

isNumber :: String -> Bool
isNumber s = all isDigit s

parseSplitMessage :: [String] -> LogMessage
-- Error messages begin with "E" followed by two numbers and finally the message
parseSplitMessage ("E":(el:(n:xs)))
  | (isNumber el && isNumber n) = LogMessage (Error (read el :: Int)) (read n :: Int) (unwords xs)
-- Warning messages begin with "W" followed by a number and then the message
parseSplitMessage ("W":(n:xs))
  | isNumber n = LogMessage Warning (read n :: Int) (unwords xs)
-- Info messages begin with "I" followed by a number and then the message
parseSplitMessage ("I":(n:xs))
  | isNumber n = LogMessage Info (read n :: Int) (unwords xs)
-- Unknown format
parseSplitMessage xs = Unknown (unwords xs)

parseMessage :: String -> LogMessage
parseMessage s = parseSplitMessage (words s)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert log_msg Leaf = Node Leaf log_msg Leaf
insert (LogMessage msg_type0 ts0 msg0) (Node left (LogMessage msg_type1 ts1 msg1) right)
  -- Insert into left subtree if timestamp is lower than the root of the current message tree
  | ts0 < ts1 = Node (LogAnalysis.insert (LogMessage msg_type0 ts0 msg0) left) (LogMessage msg_type1 ts1 msg1) right
  -- Otherwise insert into right subtree
  | otherwise = Node left (LogMessage msg_type1 ts1 msg1) (LogAnalysis.insert (LogMessage msg_type0 ts0 msg0) right)
-- Return tree unchanged for other cases
insert _ x = x

build :: [LogMessage] -> MessageTree
build ls = foldr LogAnalysis.insert Leaf ls

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = (inOrder left) ++ [m] ++ (inOrder right)

isLogRelevant :: LogMessage -> Bool
isLogRelevant (LogMessage (Error el) _ _) = el >= 50
isLogRelevant _ = False

getLogMsg :: LogMessage -> String
getLogMsg (LogMessage _ _ msg) = msg
getLogMsg (Unknown msg) = msg

whatWentWrong :: [LogMessage] -> [String]
-- whatWentWrong ls = map getLogMsg (filter isLogRelevant (inOrder (build ls)))
whatWentWrong ls = map getLogMsg (filter isLogRelevant (inOrder (build ls)))

main = do
  let msg1 = "E 2 562 help help"
  print msg1
  print (parseMessage msg1)
  let msg2 = "I 29 la la la"
  print msg2
  print (parseMessage msg2)
  let msg3 = "This is not in the right format"
  print msg3
  print (parseMessage msg3)
