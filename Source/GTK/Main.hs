module Main (
    main
) where

import System.Posix.Env
import Graphics.UI.Gtk

fakeDISPLAY = do
    display <- getEnv "DISPLAY"
    let display' (Just d) = d
        display' Nothing  = ":0.0"
    setEnv "DISPLAY" (display' display) True
    return ()

main = do
    fakeDISPLAY
    
    initGUI
    window <- windowNew
    button <- buttonNew
    set window [ containerBorderWidth := 10,
                 containerChild := button ]
    set button [ buttonLabel := "Hello World" ]
    onClicked button (putStrLn "Hello World")
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
