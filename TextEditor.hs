module Main where

slice from to x = take (to - from + 1)(drop from x) --slice an array and return values from index to to index from

data TextEditor = TextEditor { line :: String,
                               cursorPos :: Int,
                               highlightLeft :: Int,
                               highlightRight :: Int,
                               clipboard :: String
                              } deriving (Show)

main = do { putStrLn "### TEXT EDITOR ###\n1 - New file\n2 - Load file";
            x <- readLn;
            if x == 1 then
              putStrLn "Creating new file...";
            else if x == 2 then do {
                putStrLn "Enter name of file to load";
                y <- readLn;
                if y == 1 then
                  putStrLn "ok";
                else
                  putStrLn "not ok";
              }
            else
              putStrLn "Unknown command";
          }

displayText (TextEditor {line = l, cursorPos = cp, highlightLeft = hl, highlightRight = hr}) = slice 0 (cp-1) l ++ ['|'] ++ cp `drop` l
