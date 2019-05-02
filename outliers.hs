import System.Random
import Graphics.Gnuplot.Simple

main = do
  let num = 1000
  g <- getStdGen
  let ds = take 1000 (randomRs (1,num) g::[Integer])
  plotListStyle [] (defaultStyle {plotType=Dots}) ds
  print ds
