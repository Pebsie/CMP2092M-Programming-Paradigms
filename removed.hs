{ putStrLn "### TEXT EDITOR ###\n1 - New file\n2 - Load file";
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
          }remove
