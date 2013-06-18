import Data.List

count s = show (length s) ++ "\n"

main = interact (count . lines)
