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
        intro
        editor (TextEditor {line = "", cursorPos = 0, highlightLeft = 0, highlightRight = 0, clipboard = ""})

editor (TextEditor {line = l, cursorPos = cp, highlightLeft = hl, highlightRight = hr, clipboard = clip}) = do { --in order to be able to change values within the editor we need to use recursion, changing the parameter each time
  putStrLn (displayText (TextEditor {line = l, cursorPos = cp, highlightLeft = hl, highlightRight = hr}));
  i <- getLine;
  if i == "/cursor left"
    then do
      editor (TextEditor {line = l, cursorPos = (pred cp), highlightLeft = hl, highlightRight = hr, clipboard = clip});
  else if i == "/cursor right"
    then do
      editor (TextEditor {line = l, cursorPos = (succ cp), highlightLeft = hl, highlightRight = hr, clipboard = clip});
  else if i == "/quit"
    then do
      return ();
  else if head i == '/' --we don't want to insert into the line text that is a mistyped command
    then do
      putStrLn "Unknown command.";
      editor (TextEditor {line = l, cursorPos = cp, highlightLeft = hl, highlightRight = hr, clipboard = clip});
  else
    do editor (TextEditor {line = slice 0 (cp-1) l ++ i ++ cp `drop` l, cursorPos = length (slice 0 (cp-1) l ++ i ++ cp `drop` l), highlightLeft = hl, highlightRight = hr, clipboard = clip});

}





--let x = TextEditor {line="Hello World", cursorPos=3, highlightLeft=0, highlightRight=2, clipboard="world"}
