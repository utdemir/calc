module Main where

import qualified Calc
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback

foreign import javascript unsafe "runCalcHs = $1"
   js_set_runCalcHs :: JSVal -> IO ()

runCalc :: JSVal -> IO JSVal
runCalc str' = do
  str <- fromJSValUnchecked str'
  toJSVal $ Calc.run str

main :: IO ()
main =
  syncCallback1' runCalc
    <&> jsval
    >>= js_set_runCalcHs
