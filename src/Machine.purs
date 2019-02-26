-- Machine constants
module Machine where

import Prelude

-- | Machine’s word size
wordsize :: Int
wordsize = 2

-- | Code offset in default dataspace 4 in `wordsize`
codeOffset :: Int
codeOffset = 0x40000 `div` wordsize
-- | Code section size in `wordsize`
codeSize :: Int
codeSize = (128*1024) `div` wordsize

-- | Pagesize in `wordsize`, documentation sometimes calls this `ps`
pagesize :: Int
pagesize = 512 `div` wordsize

-- | Max size af a single code module, `cms`, in words
codeModuleSize :: Int
codeModuleSize = 4096

defaultDsId :: Int
defaultDsId = 4

