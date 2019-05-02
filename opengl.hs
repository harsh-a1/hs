module Main where
import Data.Bits
import Graphics.X11.Xlib
import System.Exit (exitWith, ExitCode(..))
import System.Time
import Control.Concurrent (threadDelay)
 
main :: IO ()
main = do
 dpy <- openDisplay ""
 let dflt = defaultScreen dpy
 scr = defaultScreenOfDisplay dpy
 rootw <- rootWindow dpy dflt
 win <- mkUnmanagedWindow dpy scr rootw 0 0 200 100
 setTextProperty dpy win "Hello World - The Clock" wM_NAME
 mapWindow dpy win
 updateWin dpy win
 
updateWin :: Display -> Window -> IO ()
updateWin dpy win = do
 drawInWin dpy win =<< date
 sync dpy False
 threadDelay (1 * 1000000)
 updateWin dpy win
 
date :: IO String
date = do
 t <- toCalendarTime =<< getClockTime
 return $ calendarTimeToString t
 
drawInWin :: Display -> Window -> String ->IO ()
drawInWin dpy win str = do
 bgcolor <- initColor dpy "green"
 fgcolor <- initColor dpy "blue"
 gc <- createGC dpy win
 fontStruc <- loadQueryFont dpy "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
 p <- createPixmap dpy win 200 100 (defaultDepthOfScreen (defaultScreenOfDisplay dpy))
 setForeground dpy gc bgcolor
 fillRectangle dpy p gc 0 0 200 100
 setForeground dpy gc fgcolor
 fillRectangle dpy p gc 2 2 196 96
 printString dpy p gc fontStruc str
 copyArea dpy p win gc 0 0 200 100 0 0
 freeGC dpy gc
 freeFont dpy fontStruc
 freePixmap dpy p
 
printString :: Display
 -> Drawable
 -> GC
 -> FontStruct
 -> String
 -> IO ()
printString dpy d gc fontst str =
 do let strLen = textWidth fontst str
 (_,asc,_,_) = textExtents fontst str
 valign = (100 + fromIntegral asc) `div` 2
 remWidth = 200 - strLen
 offset = remWidth `div` 2
 fgcolor <- initColor dpy "white"
 bgcolor <- initColor dpy "blue"
 setForeground dpy gc fgcolor
 setBackground dpy gc bgcolor
 drawImageString dpy d gc offset valign str
 
mkUnmanagedWindow :: Display
 -> Screen
 -> Window
 -> Position
 -> Position
 -> Dimension
 -> Dimension
 -> IO Window
mkUnmanagedWindow dpy scr rw x y w h = do
 let visual = defaultVisualOfScreen scr
 win <- allocaSetWindowAttributes $
 \attributes -> do
 set_override_redirect attributes True
 createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr)
 inputOutput visual cWOverrideRedirect attributes
 return win
 
initColor :: Display -> String -> IO Pixel
initColor dpy color = do
 let colormap = defaultColormap dpy (defaultScreen dpy)
 (apros,real) <- allocNamedColor dpy colormap color
 return $ color_pixel apros
