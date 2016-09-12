module Network.Haskoin.Transaction.Types
( Tx
, createTx
, txVersion
, txIn
, txOut
, txLockTime
, txHash
, wTxHash
, txWitness
, TxIn(..)
, TxOut(..)
, OutPoint(..)
, TxHash(..)
, WTxHash(..)
, Witness(..)
, hexToTxHash
, txHashToHex
, hexToWTxHash
, wTxHashToHex
, nosigTxHash
, coinbaseWTxHash 
) where

import Control.DeepSeq (NFData, rnf)
import Control.Monad (when, liftM2, replicateM, forM_, mzero, (<=<))

import Data.Aeson (Value(String), FromJSON, ToJSON, parseJSON, toJSON, withText)
import Data.Word (Word32, Word64)
import Data.Serialize (Serialize, get, put, runPut, Putter, encode)
import Data.Serialize.Get
    ( getWord8
    , getWord32le
    , getWord64le
    , getByteString
    , remaining
    , lookAhead
    , skip
    )
import Data.Serialize.Put
    ( putWord32le
    , putWord64le
    , putWord8
    , putByteString
    )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
    ( length
    , empty
    , reverse
    , splitAt 
    , append
    )
import Data.Maybe (fromMaybe)
import Data.String (IsString, fromString)
import Data.String.Conversions (cs)
import Text.Read (readPrec, parens, lexP, pfail)
import qualified Text.Read as Read (Lexeme(Ident, String))
import Network.Haskoin.Util
import Network.Haskoin.Crypto.Hash
import Network.Haskoin.Node.Types

newtype TxHash = TxHash { getTxHash :: Hash256 }
    deriving (Eq, Ord)

newtype WTxHash = WTxHash { getWTxHash :: Hash256 }
    deriving (Eq, Ord)

newtype Witness = Witness {getWitness :: [ByteString] } 
    deriving (Eq, Show, Read)

instance NFData TxHash where
    rnf  = rnf . getHash256 . getTxHash

instance NFData WTxHash where
    rnf  = rnf . getHash256 . getWTxHash

instance NFData Witness where
    rnf  = rnf . getWitness

instance Read TxHash where
    readPrec = parens $ do
        Read.Ident "TxHash" <- lexP
        Read.String str <- lexP
        maybe pfail return $ hexToTxHash $ cs str

instance Show TxHash where
    showsPrec d h = showParen (d > 10) $
        showString "TxHash " . shows (txHashToHex h)

instance IsString TxHash where
    fromString =
        TxHash . fromMaybe e . bsToHash256
               . BS.reverse . fromMaybe e' . decodeHex . cs
      where
        e = error "Could not read transaction hash from decoded hex string"
        e' = error "Colud not decode hex string with transaction hash"


instance IsString WTxHash where
    fromString =
        WTxHash . fromMaybe e . bsToHash256
                . BS.reverse . fromMaybe e' . decodeHex . cs
      where
        e = error "Could not read witness transaction hash from decoded hex string"
        e' = error "Colud not decode hex string with witness transaction hash"

instance Serialize TxHash where
    get = TxHash <$> get
    put = put . getTxHash

instance Serialize WTxHash where
    get = WTxHash <$> get
    put = put . getWTxHash

instance Serialize Witness where
    get = Witness <$> (repList =<< get) 
      where
        repList (VarInt c) = replicateM (fromIntegral c) getBS
        getBS = do
          (VarInt len) <- get
          getByteString $ fromIntegral len

    put (Witness ws) = do 
        put $ VarInt $ fromIntegral $ length ws
        forM_ ws putBS
      where putBS bs = do
              put $ VarInt $ fromIntegral $ BS.length bs 
              putByteString bs

nosigTxHash :: Tx -> TxHash
nosigTxHash tx =
    TxHash $ doubleHash256 $ encode tx{ _txIn = map clearInput $ txIn tx }
  where
    clearInput ti = ti{ scriptInput = BS.empty }

txHashToHex :: TxHash -> ByteString
txHashToHex (TxHash h) = encodeHex $ BS.reverse $ getHash256 h

hexToTxHash :: ByteString -> Maybe TxHash
hexToTxHash hex = do
    bs <- BS.reverse <$> decodeHex hex
    h <- bsToHash256 bs
    return $ TxHash h

wTxHashToHex :: WTxHash -> ByteString
wTxHashToHex (WTxHash h) = encodeHex $ BS.reverse $ getHash256 h

hexToWTxHash :: ByteString -> Maybe WTxHash
hexToWTxHash hex = do
    bs <- BS.reverse <$> decodeHex hex
    h <- bsToHash256 bs
    return $ WTxHash h

instance FromJSON TxHash where
    parseJSON = withText "Transaction id" $ \t ->
        maybe mzero return $ hexToTxHash $ cs t

instance ToJSON TxHash where
    toJSON = String . cs . txHashToHex

-- | Data type representing a bitcoin transaction
data Tx = Tx
    { -- | Transaction data format version
      _txVersion  :: !Word32
      -- | List of transaction inputs
    , _txIn       :: ![TxIn]
      -- | List of transaction outputs
    , _txOut      :: ![TxOut]
     -- | Witness 
    , _txWitness  :: ![Witness] 
      -- | The block number of timestamp at which this transaction is locked
    , _txLockTime :: !Word32
     -- | Hash of the transaction
    , _txHash     :: !TxHash
     -- | Hash of the witness transaction
    , _wtxHash     :: !WTxHash
    } deriving (Eq)
{--
isCoinbase :: Tx -> Bool
isCoinbase tx = 
   length tin == 1 && (tin !! 0)
  where tin =  _txIn tx
  --}

txVersion :: Tx -> Word32
txVersion = _txVersion

txIn :: Tx -> [TxIn]
txIn = _txIn

txOut :: Tx -> [TxOut]
txOut = _txOut

txLockTime :: Tx -> Word32
txLockTime = _txLockTime

txHash :: Tx -> TxHash
txHash = _txHash

wTxHash :: Tx -> WTxHash
wTxHash = _wtxHash

txWitness:: Tx -> [Witness]
txWitness = _txWitness

createTx :: Word32 -> [TxIn] -> [TxOut] -> Word32 -> Tx
createTx v is os l =
    Tx { _txVersion  = v
       , _txIn       = is
       , _txOut      = os
       , _txWitness  = [] 
       , _txLockTime = l
       , _txHash     = TxHash txid
       , _wtxHash    = WTxHash txid 
       }
  where
    txid = doubleHash256 $ encodeWithoutWitness tx
    tx = Tx { _txVersion  = v
            , _txIn       = is
            , _txOut      = os
            , _txWitness  = []
            , _txLockTime = l
            , _txHash     = fromString empty 
            , _wtxHash    = fromString empty
            }
    empty = replicate 64 '0'


coinbaseWTxHash :: WTxHash
coinbaseWTxHash = fromString $ replicate 64 '0'

isEmptyWitness :: Witness -> Bool
isEmptyWitness (Witness [])  = True
isEmptyWitness _  = False


createWTx :: Word32 -> [(TxIn, Witness)] -> [TxOut] -> Word32 -> Tx
createWTx v is os l
  | and $ map (isEmptyWitness . snd) is = createTx v (map fst is) os l
  | otherwise = 
    Tx { _txVersion  = v
       , _txIn       = map fst is 
       , _txOut      = os
       , _txWitness  = map snd is 
       , _txLockTime = l
       , _txHash     = txid
       , _wtxHash    = wtxid 
       }
  where
    txid  = TxHash $ doubleHash256 $ encodeWithoutWitness tx
    wtxid = WTxHash $ doubleHash256 $ encode tx
    tx    = Tx { _txVersion  = v
            , _txIn       = map fst is
            , _txOut      = os
            , _txWitness  = map snd is
            , _txLockTime = l
            , _txHash     = fromString empty 
            , _wtxHash    = fromString empty
            }
    empty = replicate 64 '0'

instance Show Tx where
    showsPrec d tx = showParen (d > 10) $
        showString "Tx " . shows (encodeHex $ encode tx)

instance Read Tx where
    readPrec = parens $ do
        Read.Ident "Tx" <- lexP
        Read.String str <- lexP
        maybe pfail return $ decodeToMaybe =<< decodeHex (cs str)

instance IsString Tx where
    fromString =
        fromMaybe e . (decodeToMaybe <=< decodeHex) . cs
      where
        e = error "Could not read transaction from hex string"

instance NFData Tx where
    rnf (Tx v i o w l t wt ) = rnf v `seq` rnf i `seq` rnf o `seq` rnf w `seq` rnf l `seq` rnf t  `seq` rnf wt


instance Serialize Tx where
    get = do
        hasWitNess <- detectWitness 
        get' hasWitNess
     where
        get' False = do
          start <- remaining
          (v, is, os, l, end) <- lookAhead $ do
              v   <- getWord32le
              is  <- replicateList =<< get
              os  <- replicateList =<< get
              l   <- getWord32le
              end <- remaining
              return (v, is, os, l, end)
          bs <- getByteString $ fromIntegral $ start - end
          let hash = doubleHash256 bs
          return Tx { _txVersion  = v
            , _txIn       = is
            , _txOut      = os
            , _txWitness  = []
            , _txLockTime = l
            , _txHash     = TxHash hash
            , _wtxHash    = WTxHash hash 
            }

        get' True = do
          start <- remaining
          (v, is, os, wit, l, ws, we, end) <- lookAhead $ do
              v  <- getWord32le
              skip 2 -- marker and flag
              is  <- replicateList =<< get
              os  <- replicateList =<< get
              ws  <- remaining
              wit <- replicateM (length is) get 
              we  <- remaining
              l   <- getWord32le
              end <- remaining
              return (v, is, os, wit, l, ws, we, end)
          let len = fromIntegral $ start - end
          bs          <- getByteString len
          noWitnesses <-  getByteStringExcludes (start - ws) (start - we) bs
          clean       <-  getByteStringExcludes 4 6 noWitnesses 

          return Tx { _txVersion  = v
            , _txIn       = is
            , _txOut      = os
            , _txWitness  = wit
            , _txLockTime = l
            , _txHash     = TxHash $ doubleHash256 clean
            , _wtxHash    = WTxHash $ doubleHash256 bs
            }
            
        replicateList (VarInt c) = replicateM (fromIntegral c) get

        detectWitness = do
          --todo lookAheadM
          (m ,f) <- lookAhead $ getWord32le >> (,) <$> getWord8 <*> getWord8
          when ( m == 0 && f /= 0 && f /= 1) $ fail "unknown falg"
          let r = ( m == 0 && f == 1)
          return r

        getByteStringExcludes b e content = do
          return $ (fst $ BS.splitAt b content) `BS.append` (snd $ BS.splitAt e content)

    put (Tx v is os w l _ _) = do
        putWord32le v
        when hasWitness $ putWord8 0 >> putWord8 1
        put $ VarInt $ fromIntegral $ length is
        forM_ is put
        put $ VarInt $ fromIntegral $ length os
        forM_ os put
        when hasWitness $ do 
            put $ VarInt $ fromIntegral $ length w 
            forM_ w put
        putWord32le l
      where hasWitness = not $ null w  -- todo || all of witness script is empty

encodeWithoutWitness = runPut . putWithoutWitness 

putWithoutWitness :: Putter Tx
putWithoutWitness (Tx v is os _ l _ _) = do
        putWord32le v
        put $ VarInt $ fromIntegral $ length is
        forM_ is put
        put $ VarInt $ fromIntegral $ length os
        forM_ os put
        putWord32le l

instance FromJSON Tx where
    parseJSON = withText "Tx" $
        maybe mzero return . (decodeToMaybe <=< decodeHex) . cs

instance ToJSON Tx where
    toJSON = String . cs . encodeHex . encode

-- | Data type representing a transaction input.
data TxIn =
    TxIn {
           -- | Reference the previous transaction output (hash + position)
           prevOutput   :: !OutPoint
           -- | Script providing the requirements of the previous transaction
           -- output to spend those coins.
         , scriptInput  :: !ByteString
           -- | Transaction version as defined by the sender of the
           -- transaction. The intended use is for replacing transactions with
           -- new information before the transaction is included in a block.
         , txInSequence :: !Word32
         } deriving (Eq, Show, Read)

instance NFData TxIn where
    rnf (TxIn p i s) = rnf p `seq` rnf i `seq` rnf s

instance Serialize TxIn where
    get =
        TxIn <$> get <*> (readBS =<< get) <*> getWord32le
      where
        readBS (VarInt len) = getByteString $ fromIntegral len

    put (TxIn o s q) = do
        put o
        put $ VarInt $ fromIntegral $ BS.length s
        putByteString s
        putWord32le q

-- | Data type representing a transaction output.
data TxOut =
    TxOut {
            -- | Transaction output value.
            outValue     :: !Word64
            -- | Script specifying the conditions to spend this output.
          , scriptOutput :: !ByteString
          } deriving (Eq, Show, Read)

instance NFData TxOut where
    rnf (TxOut v o) = rnf v `seq` rnf o

instance Serialize TxOut where
    get = do
        val <- getWord64le
        (VarInt len) <- get
        TxOut val <$> (getByteString $ fromIntegral len)

    put (TxOut o s) = do
        putWord64le o
        put $ VarInt $ fromIntegral $ BS.length s
        putByteString s

-- | The OutPoint is used inside a transaction input to reference the previous
-- transaction output that it is spending.
data OutPoint = OutPoint
    { -- | The hash of the referenced transaction.
      outPointHash  :: !TxHash
      -- | The position of the specific output in the transaction.
      -- The first output position is 0.
    , outPointIndex :: !Word32
    } deriving (Read, Show, Eq)

instance NFData OutPoint where
    rnf (OutPoint h i) = rnf h `seq` rnf i

instance FromJSON OutPoint where
    parseJSON = withText "OutPoint" $
        maybe mzero return . (decodeToMaybe <=< decodeHex) . cs

instance ToJSON OutPoint where
    toJSON = String . cs . encodeHex . encode

instance Serialize OutPoint where
    get = do
        (h,i) <- liftM2 (,) get getWord32le
        return $ OutPoint h i
    put (OutPoint h i) = put h >> putWord32le i

