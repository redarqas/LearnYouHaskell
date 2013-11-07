module PutJSON where

import SimpleJSON
import Data.List

renderJValue :: JValue -> String
renderJValue (JString s) = s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue (JNull) = "null"
renderJValue (JObject o) = "{" ++ renderPairs o ++ "}"
    where renderPairs [] = ""
          renderPairs ps = intercalate ", " (map renderPair ps)
          renderPair (key, value) = show key ++ ": " ++ renderJValue value
renderJValue (JArray arr) = "[" ++ renderVals arr ++ "]"
    where renderVals [] = ""
          renderVals arr = intercalate ", " (map renderJValue arr)

putJValue :: JValue -> IO ()
putJValue = putStrLn . renderJValue  



