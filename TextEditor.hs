module Main where

slice from to x = take (to - from + 1)(drop from x) --slice an array and return values from index to to index from

data TextEditor = TextEditor { line :: String,
                               cursorPos :: Int,
                               highlightLeft :: Int,
                               highlightRight :: Int,
                               clipboard :: String
                              } deriving (Show)

displayText (TextEditor {line = l, cursorPos = cp, highlightLeft = hl, highlightRight = hr}) = slice 0 (cp-1) l ++ ['|'] ++ cp `drop` l

intro = putStrLn "#### TEXT EDITOR ####\nType '/loadFile *filename*' to load a file.\nType '/saveFile *filename*' to save a file.\nType '/cursor left' or '/cursor right' to move the cursor.\nType '/highlight on' and '/highlight off' to start or stop highlighting.\nType '/delete' to delete everything that is currently highlighted.\nType '/cut', '/copy' and '/paste' to use those functions.\n\nEverything else will append the current line."

main = do
        i <- getLine
        if null i
          then return ()
        else if i == "/help"
          then do
            intro
            main
          else do
            putStrLn (displayText (TextEditor {line = i, cursorPos = length i, highlightLeft = 0, highlightRight = 0, clipboard = "empty"}))
            main






--let x = TextEditor {line="Hello World", cursorPos=3, highlightLeft=0, highlightRight=2, clipboard="world"}
