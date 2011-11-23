----------------------------------------------------------------------
-- FILE:              Memory.hs
-- DATE:              02/17/2001
-- PROJECT:           HARM (was VARM (Virtual ARM)), for CSE240 Spring 2001
-- LANGUAGE PLATFORM: HUGS
-- OS PLATFORM:       RedHat Linux 6.2
-- AUTHOR:            Jeffrey A. Meunier
-- EMAIL:             jeffm@cse.uconn.edu
-- MAINTAINER:        Alex Mason
-- EMAIL:             axman6@gmail.com
----------------------------------------------------------------------



module Arm.Memory
where



----------------------------------------------------------------------
-- Standard libraries.
----------------------------------------------------------------------
-- import IOExts
import Data.Array.IO
import Data.Word



----------------------------------------------------------------------
-- Local libraries.
----------------------------------------------------------------------



----------------------------------------------------------------------
-- Memory is an array of Word32 indexed by an Address.
----------------------------------------------------------------------
type Memory
  = IOArray Address Word32

type Address
  = Word32

type WordAddress
  = Address

type ByteAddress
  = Address



----------------------------------------------------------------------
-- Create a new memory array.
----------------------------------------------------------------------
emptyMem
  :: Address
  -> IO Memory

emptyMem size
  = newArray (0, size-1) 0
--  = listArray (0, size-1) (repeat 0)



----------------------------------------------------------------------
-- Return the word_32 address of a byte address.
-- This can be read as ``the word address of the nth byte in memory''.
----------------------------------------------------------------------
wordAddress
  :: ByteAddress
  -> WordAddress

wordAddress addr
  = addr `div` 4



----------------------------------------------------------------------
-- Get the value at a memory location.
----------------------------------------------------------------------
getMemWord
  :: Memory
  -> WordAddress
  -> IO Word32

getMemWord mem addr
  = readArray mem addr
--  = mem ! addr


  
----------------------------------------------------------------------
-- Set the value at a memory location.
----------------------------------------------------------------------
setMemWord
  :: Memory
  -> WordAddress
  -> Word32
  -> IO ()

setMemWord mem addr val
  = writeArray mem addr val
--  = mem // [(addr, val)]



----------------------------------------------------------------------
-- Read memory.  The byte address of the memory location is given.
----------------------------------------------------------------------
readMem
  :: Memory
  -> Address
  -> IO Word32

readMem mem byteAddr
  = getMemWord mem (wordAddress byteAddr)



----------------------------------------------------------------------
-- Write memory.  The byte address of the memory location is given.
----------------------------------------------------------------------
writeMem
  :: Memory
  -> Address
  -> Word32
  -> IO ()

writeMem mem byteAddr val
  = setMemWord mem (wordAddress byteAddr) val



----------------------------------------------------------------------
-- eof
----------------------------------------------------------------------
