-- Caesar Cypher - Phil Picinic
import Data.Char

strToUpper str = map toUpper str

encrypt str shiftAmount = encrypthelper (strToUpper str) shiftAmount
  where encrypthelper str 0 = str
        encrypthelper str shiftAmount = encrypthelper (encryptOnce str) (shiftAmount - 1)
        encryptOnce [] = []
        encryptOnce (x:xs) = if x >= 'A' then if x <= 'Z' then (fixEncrypt (chr(ord(x) + 1))) : encryptOnce(xs) else x : encryptOnce(xs) else x : encryptOnce(xs)
        fixEncrypt c = if c > 'Z' then 'A' else c

decrypt str shiftAmount = decrypthelper (strToUpper str) shiftAmount
  where decrypthelper str 0 = str
        decrypthelper str shiftAmount = decrypthelper (decryptOnce str) (shiftAmount - 1)
        decryptOnce [] = []
        decryptOnce (x:xs) = if x >= 'A' then if x <= 'Z' then (fixDecrypt (chr(ord(x) - 1))) : decryptOnce(xs) else x : decryptOnce(xs) else x : decryptOnce(xs)
        fixDecrypt c = if c < 'A' then 'Z' else c

solve str 0 = "CAESER 0: " ++ (decrypt str 0)
solve str maxShiftValue = "CAESER " ++ (show maxShiftValue) ++ ": " ++ (decrypt str maxShiftValue) ++ "\n" ++ (solve str (maxShiftValue - 1))