{-# LINE 1 "Regex.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Regex.hsc" #-}

module Regex where

import Foreign
import Foreign.C
import Foreign.C.Types
import Data.ByteString


{-# LINE 11 "Regex.hsc" #-}

newtype PCREOption = PCREOption { unPCREOption :: CInt } deriving(Show, Eq)

caseless        :: PCREOption
caseless        = PCREOption 1
dollar_endonly  :: PCREOption
dollar_endonly  = PCREOption 32
dotall          :: PCREOption
dotall          = PCREOption 4

{-# LINE 19 "Regex.hsc" #-}

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . Prelude.foldr ((.|.) . unPCREOption) 0


type PCRE = ()
-- data PCRE

foreign import ccall unsafe "pcre.h pcre_compile" c_pcre_compile :: CString -> PCREOption -> Ptr CString -> Ptr CInt -> Ptr Word8 -> IO (Ptr PCRE)

data Regex = Regex !(ForeignPtr PCRE) !ByteString deriving (Show, Ord, Eq)

compile :: ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
    useAsCString str $ \pattern -> do
        alloca $ \errptr -> do
        alloca $ \erroffset -> do
        pcre_ptr <- c_pcre_compile pattern (combineOptions flags) errptr erroffset nullPtr
        if pcre_ptr == nullPtr 
            then do
                err <- peekCString =<< peek errptr
                return (Left err)
            else do
                reg <- newForeignPtr finalizerFree pcre_ptr -- release with free()
                return (Right (Regex reg str))
        
