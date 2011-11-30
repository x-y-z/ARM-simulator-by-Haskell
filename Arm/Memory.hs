module Memory
where
  
import Data.Map (Map)  
import qualified Data.Map as Map

import Data.Word

import Control.Monad.State

type Memory = Map Address Word32

type Address = Word32

type WordAddress = Address

type ByteAddress = Address

emptyMem :: Memory
emptyMem = Map.empty

wordAddress :: ByteAddress -> WordAddress
wordAddress addr = addr `div` 4

getMemWord :: WordAddress -> State Memory Word32
getMemWord addr = do m <- get
                     if Map.member addr m 
                       then return (m Map.! addr) else return 0

setMemWord :: WordAddress -> Word32 -> State Memory ()
setMemWord addr val = do m <- get
                         put $ Map.insert addr val m

readMem :: Address -> State Memory Word32
readMem byteAddr = getMemWord (wordAddress byteAddr)
    
writeMem :: Address -> Word32 -> State Memory ()
writeMem byteAddr val = setMemWord (wordAddress byteAddr) val