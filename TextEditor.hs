module Main where

import System.IO --for file reading and saving

slice from to x = take (to - from + 1)(drop from x) --slice an array and return values from index to to index from

data TextEditor = TextEditor { line :: String,
                               cursorPos :: Int,
                               highlightLeft :: Int,
                               highlightRight :: Int,
                               clipboard :: String,
                               highlightOn :: Bool
                              } deriving (Show)

displayText (TextEditor {line = l, cursorPos = cp}) = slice 0 (cp-1) l ++ ['|'] ++ cp `drop` l
displayHLText (TextEditor {line = l, cursorPos = cp, highlightLeft = hl, highlightRight = hr}) = (slice 0 (hl-1) (displayText (TextEditor {line = l, cursorPos = cp}))) ++ ['['] ++ (slice hl (hr-1) (displayText (TextEditor {line = l, cursorPos = cp}))) ++ [']'] ++ (slice hr (length l) (displayText (TextEditor {line = l, cursorPos = cp})))


intro = putStrLn "#### TEXT EDITOR ####\nType '/loadfile to load a file.\nType '/saveFile *filename*' to save a file.\nType '/cl' or '/cr' to move the cursor left and right.\nType '/highlight on' and '/highlight off' to start or stop highlighting.\nType '/delete' to delete everything that is currently highlighted or the previous character.\nType '/cut', '/copy' and '/paste' to use those functions.\n\nEverything else will append the current line."

main = do
        intro
        editor (TextEditor {line = "", cursorPos = 0, highlightLeft = 0, highlightRight = 0, clipboard = "", highlightOn = False})

editor (TextEditor {line = l, cursorPos = cp, highlightLeft = hl, highlightRight = hr, clipboard = clip, highlightOn = ho}) = do { --in order to be able to change values within the editor we need to use recursion, changing the parameter each time
  if ho == False
    then do putStrLn (displayText (TextEditor {line = l, cursorPos = cp}));
  else
    putStrLn (displayHLText (TextEditor {line = l, cursorPos = cp, highlightLeft = hl, highlightRight = hr}));

  i <- getLine;
  if i == "/cl"
    then do
      if ho == True && (pred cp) < hl
        then do
          editor (TextEditor {line = l, cursorPos = (pred cp), highlightLeft = (pred cp), highlightRight = hr, clipboard = clip, highlightOn = ho});
      else
        editor (TextEditor {line = l, cursorPos = (pred cp), highlightLeft = hl, highlightRight = hr, clipboard = clip, highlightOn = ho});

  else if i == "/cr"
    then do

      if ho == True && (succ cp) > hl
        then do
          editor (TextEditor {line = l, cursorPos = (succ cp), highlightLeft = hl, highlightRight = (succ cp), clipboard = clip, highlightOn = ho});
      else
        editor (TextEditor {line = l, cursorPos = (succ cp), highlightLeft = hl, highlightRight = hr, clipboard = clip, highlightOn = ho});

  else if i == "/quit"
    then do
      return ();
  else if i == "/delete"
    then do
      if ho == True
        then do
          editor (TextEditor {line = ((slice 0 (hl-1) l) ++ (hr `drop` l)), cursorPos = (length (slice 0 (hl-1) l)), highlightLeft = hl, highlightRight = hr, clipboard = clip, highlightOn = False});
      else
        editor (TextEditor {line = ((slice 0 (cp-1) l) ++ ((cp+1) `drop` l)), cursorPos = cp, highlightLeft = hl, highlightRight = hr, clipboard = clip, highlightOn = False});

  else if i == "/highlight on"
    then do
      putStrLn "Highlight mode is on.";
      editor (TextEditor {line = l, cursorPos = cp, highlightLeft = cp, highlightRight = succ cp, clipboard = clip, highlightOn = True});
  else if i == "/highlight off"
    then do
      putStrLn "Highlight mode is off.";
      editor (TextEditor {line = l, cursorPos = cp, highlightLeft = hl, highlightRight = hr, clipboard = clip, highlightOn = False});
  else if i == "/cut"
    then do
      if ho == True
        then do
          putStrLn ("Copied '" ++ (slice hl (hr-1) l) ++ "' to clipboard.");
          editor (TextEditor {line = ((slice 0 (hl-1) l) ++ (hr `drop` l)), cursorPos = (length (slice 0 (hl-1) l)), highlightLeft = hl, highlightRight = hr, clipboard = (slice hl (hr-1) l), highlightOn = False});
      else --prevent user errors
        putStrLn "Nothing is highlighted.";
        editor (TextEditor {line = l, cursorPos = cp, highlightLeft = hl, highlightRight = hr, clipboard = clip, highlightOn = ho});
  else if i == "/copy"
    then do
      if ho == True
        then do
          putStrLn ("Copied '" ++ (slice hl (hr-1) l) ++ "' to clipboard.");
          editor (TextEditor {line = l, cursorPos = (length (slice 0 (hl-1) l)), highlightLeft = hl, highlightRight = hr, clipboard = (slice hl (hr-1) l), highlightOn = False});
      else
        putStrLn "Nothing is highlighted.";
        editor (TextEditor {line = l, cursorPos = cp, highlightLeft = hl, highlightRight = hr, clipboard = clip, highlightOn = ho});
  else if i == "/paste"
    then do
      editor (TextEditor {line = slice 0 (cp-1) l ++ clip ++ cp `drop` l, cursorPos = (cp + length clip), highlightLeft = hl, highlightRight = hr, clipboard = clip, highlightOn = ho});
  else if i == "/loadfile"
    then do
      putStrLn "Enter filename with extension.";
      x <- getLine;
      a <- openFile x ReadMode;
      line <- hGetLine a;
      editor (TextEditor {line = line, cursorPos = length line, highlightLeft = hl, highlightRight = hr, clipboard = clip, highlightOn = ho});
  else if head i == '/' --we don't want to insert into the line text that is a mistyped command
    then do
      putStrLn "Unknown command.";
      editor (TextEditor {line = l, cursorPos = cp, highlightLeft = hl, highlightRight = hr, clipboard = clip, highlightOn = ho});
  else
    do editor (TextEditor {line = slice 0 (cp-1) l ++ i ++ cp `drop` l, cursorPos = (cp + length i), highlightLeft = hl, highlightRight = hr, clipboard = clip, highlightOn = ho});

}





--let x = TextEditor {line="Hello World", cursorPos=3, highlightLeft=0, highlightRight=2, clipboard="world"}
