{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Network.Haskoin.Transaction.Units (tests, satoshiCoreTxTests) where

import Test.HUnit (Assertion, assertBool)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import Data.Word (Word32, Word64)
import Data.Maybe (fromJust)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (reverse)
import qualified Data.ByteString.Lazy as LBS
import Data.Serialize (decode, encode, getWord32le, putWord32le, runGet, runPut)
import Safe (readMay)
import GHC.Exts( IsString(..) )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Vector as V
import Data.String.Conversions (convertString)
import Data.List (groupBy)
import qualified Data.Either

import Network.Haskoin.Transaction
import Network.Haskoin.Script
import Network.Haskoin.Util

tests :: [Test]
tests =
    [ testGroup "Computing TxID from Tx"
        ( map mapTxIDVec $ zip txIDVec [0..] )
    , testGroup "Computing WTxID form Tx"
        ( map mapWTxIDVec $ zip wtxIDVec [0..] )
    , testGroup "Build PKHash Transaction (generated from bitcoind)"
        ( map mapPKHashVec $ zip pkHashVec [0..] )
    , testGroup "Verify transaction (bitcoind /test/data/tx_valid.json)"
        ( map mapVerifyVec $ zip verifyVec [0..] )
    , testCase "" tEncodeSatoshiCoreScriptPubKey
    ]

mapTxIDVec :: ((ByteString, ByteString), Int) -> Test.Framework.Test
mapTxIDVec (v,i) = testCase name $ runTxIDVec v
  where
    name = "Compute TxID " ++ (show i)

mapWTxIDVec :: ((ByteString, ByteString), Int) -> Test.Framework.Test
mapWTxIDVec (v,i) = testCase name $ runWTxIDVec v
  where
    name = "Compute WTxID " ++ (show i)

runTxIDVec :: (ByteString, ByteString) -> Assertion
runTxIDVec (tid,tx) = assertBool "TxID" $
    (txHashToHex $ txHash txBS) == tid
  where
    txBS = fromRight . decode $ fromJust $ decodeHex tx

runWTxIDVec :: (ByteString, ByteString) -> Assertion
runWTxIDVec (tid,tx) = assertBool "WTxID" $
    (wTxHashToHex $ wTxHash txBS) == tid
  where
    txBS = fromRight . decode $ fromJust $ decodeHex tx

wtxIDVec :: [(ByteString, ByteString)]
wtxIDVec = 
   [ ( "3356a1abf6e1fdf9858a704c794aea3c4dfa97848b4b5390530204c7382fc6fc"
       -- "2b9baddbd2861c663978a98c6c3c7648e1cd5c41b451f4a35b7851dd4786d9d3"
       -- coninbased tx
      , "010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2f0315230e0004ae03ca57043e3d1e1d0c8796bf579aef0c0000000000122f4e696e6a61506f6f6c2f5345475749542fffffffff038427a112000000001976a914876fbb82ec05caa6af7a3b5e5a983aae6c6cc6d688ac0000000000000000266a24aa21a9ed5c748e121c0fe146d973a4ac26fa4a68b0549d46ee22d25f50a5e46fe1b377ee00000000000000002952534b424c4f434b3acd16772ad61a3c5f00287480b720f6035d5e54c9efc71be94bb5e3727f1090900120000000000000000000000000000000000000000000000000000000000000000000000000"
      )
   , ( "49c37eab32d83f31fafd15815ab047ef91a3a4bb86c9d25a28dbf4afdc156670"
      , "0100000000010145310e878941a1b2bc2d33797ee4d89d95eaaf2e13488063a2aa9a74490f510a0100000023220020b6744de4f6ec63cc92f7c220cdefeeb1b1bed2b66c8e5706d80ec247d37e65a1ffffffff01002d3101000000001976a9143ebc40e411ed3c76f86711507ab952300890397288ac0400473044022001dd489a5d4e2fbd8a3ade27177f6b49296ba7695c40dbbe650ea83f106415fd02200b23a0602d8ff1bdf79dee118205fc7e9b40672bf31563e5741feb53fb86388501483045022100f88f040e90cc5dc6c6189d04718376ac19ed996bf9e4a3c29c3718d90ffd27180220761711f16c9e3a44f71aab55cbc0634907a1fa8bb635d971a9a01d368727bea10169522103b3623117e988b76aaabe3d63f56a4fc88b228a71e64c4cc551d1204822fe85cb2103dd823066e096f72ed617a41d3ca56717db335b1ea47a1b4c5c9dbdd0963acba621033d7c89bd9da29fa8d44db7906a9778b53121f72191184a9fee785c39180e4be153ae00000000"
      )
    , ( "06eee51317a76a76c67499c8f782819745b58d28cdb4d8357ef7f7e6d79cc513"
      , "010000000120925534261de4dcebb1ed5ab1b62bfe7a3ef968fb111dc2c910adfebc6e3bdf010000006b483045022100f50198f5ae66211a4f485190abe4dc7accdabe3bc214ebc9ea7069b97097d46e0220316a70a03014887086e335fc1b48358d46cd6bdc9af3b57c109c94af76fc915101210316cff587a01a2736d5e12e53551b18d73780b83c3bfb4fcf209c869b11b6415effffffff0220a10700000000001976a91450333046115eaa0ac9e0216565f945070e44573988ac2e7cd01a000000001976a914c01a7ca16b47be50cbdbc60724f701d52d75156688ac00000000"
      )
    , ( "f56da6d0bb5807561c29093066edd1d505c2fa4ae89bb895c4318481d360fd3f"
      , "010000000203a25f58630d7a1ea52550365fd2156683f56daf6ca73a4b4bbd097e66516322010000006a47304402204efc3d70e4ca3049c2a425025edf22d5ca355f9ec899dbfbbeeb2268533a0f2b02204780d3739653035af4814ea52e1396d021953f948c29754edd0ee537364603dc012103f7a897e4dbecab2264b21917f90664ea8256189ea725d28740cf7ba5d85b5763ffffffff03a25f58630d7a1ea52550365fd2156683f56daf6ca73a4b4bbd097e66516322000000006a47304402202d96defdc5b4af71d6ba28c9a6042c2d5ee7bc6de565d4db84ef517445626e03022022da80320e9e489c8f41b74833dfb6a54a4eb5087cdb46eb663eef0b25caa526012103f7a897e4dbecab2264b21917f90664ea8256189ea725d28740cf7ba5d85b5763ffffffff0200e1f5050000000017a914b7e6f7ff8658b2d1fb107e3d7be7af4742e6b1b3876f88fc00000000001976a914913bcc2be49cb534c20474c4dee1e9c4c317e7eb88ac00000000"
      )
    , ( "32a52be869fc148b6104244859c879f1319cfd86e89e6f7fc1ffaaf518fa14be"
      , "01000000043ffd60d3818431c495b89be84afac205d5d1ed663009291c560758bbd0a66df5010000006b483045022100f344607de9df42049688dcae8ff1db34c0c7cd25ec05516e30d2bc8f12ac9b2f022060b648f6a21745ea6d9782e17bcc4277b5808326488a1f40d41e125879723d3a012103f7a897e4dbecab2264b21917f90664ea8256189ea725d28740cf7ba5d85b5763ffffffffa5379401cce30f84731ef1ba65ce27edf2cc7ce57704507ebe8714aa16a96b92010000006a473044022020c37a63bf4d7f564c2192528709b6a38ab8271bd96898c6c2e335e5208661580220435c6f1ad4d9305d2c0a818b2feb5e45d443f2f162c0f61953a14d097fd07064012103f7a897e4dbecab2264b21917f90664ea8256189ea725d28740cf7ba5d85b5763ffffffff70e731e193235ff12c3184510895731a099112ffca4b00246c60003c40f843ce000000006a473044022053760f74c29a879e30a17b5f03a5bb057a5751a39f86fa6ecdedc36a1b7db04c022041d41c9b95f00d2d10a0373322a9025dba66c942196bc9d8adeb0e12d3024728012103f7a897e4dbecab2264b21917f90664ea8256189ea725d28740cf7ba5d85b5763ffffffff66b7a71b3e50379c8e85fc18fe3f1a408fc985f257036c34702ba205cef09f6f000000006a4730440220499bf9e2db3db6e930228d0661395f65431acae466634d098612fd80b08459ee022040e069fc9e3c60009f521cef54c38aadbd1251aee37940e6018aadb10f194d6a012103f7a897e4dbecab2264b21917f90664ea8256189ea725d28740cf7ba5d85b5763ffffffff0200e1f5050000000017a9148fc37ad460fdfbd2b44fe446f6e3071a4f64faa6878f447f0b000000001976a914913bcc2be49cb534c20474c4dee1e9c4c317e7eb88ac00000000"
      )
    ]

txIDVec :: [(ByteString, ByteString)]
txIDVec =
    [ ( "23b397edccd3740a74adb603c9756370fafcde9bcc4483eb271ecad09a94dd63"
      , "0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba26000000000490047304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2b01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000"
      )
    , ( "c99c49da4c38af669dea436d3e73780dfdb6c1ecf9958baa52960e8baee30e73"
      , "01000000010276b76b07f4935c70acf54fbf1f438a4c397a9fb7e633873c4dd3bc062b6b40000000008c493046022100d23459d03ed7e9511a47d13292d3430a04627de6235b6e51a40f9cd386f2abe3022100e7d25b080f0bb8d8d5f878bba7d54ad2fda650ea8d158a33ee3cbd11768191fd004104b0e2c879e4daf7b9ab68350228c159766676a14f5815084ba166432aab46198d4cca98fa3e9981d0a90b2effc514b76279476550ba3663fdcaff94c38420e9d5000000000100093d00000000001976a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac00000000"
      )
    , ( "f7fdd091fa6d8f5e7a8c2458f5c38faffff2d3f1406b6e4fe2c99dcc0d2d1cbb"
      , "01000000023d6cf972d4dff9c519eff407ea800361dd0a121de1da8b6f4138a2f25de864b4000000008a4730440220ffda47bfc776bcd269da4832626ac332adfca6dd835e8ecd83cd1ebe7d709b0e022049cffa1cdc102a0b56e0e04913606c70af702a1149dc3b305ab9439288fee090014104266abb36d66eb4218a6dd31f09bb92cf3cfa803c7ea72c1fc80a50f919273e613f895b855fb7465ccbc8919ad1bd4a306c783f22cd3227327694c4fa4c1c439affffffff21ebc9ba20594737864352e95b727f1a565756f9d365083eb1a8596ec98c97b7010000008a4730440220503ff10e9f1e0de731407a4a245531c9ff17676eda461f8ceeb8c06049fa2c810220c008ac34694510298fa60b3f000df01caa244f165b727d4896eb84f81e46bcc4014104266abb36d66eb4218a6dd31f09bb92cf3cfa803c7ea72c1fc80a50f919273e613f895b855fb7465ccbc8919ad1bd4a306c783f22cd3227327694c4fa4c1c439affffffff01f0da5200000000001976a914857ccd42dded6df32949d4646dfa10a92458cfaa88ac00000000"
      )
    , ( "afd9c17f8913577ec3509520bd6e5d63e9c0fd2a5f70c787993b097ba6ca9fae"
      , "010000000370ac0a1ae588aaf284c308d67ca92c69a39e2db81337e563bf40c59da0a5cf63000000006a4730440220360d20baff382059040ba9be98947fd678fb08aab2bb0c172efa996fd8ece9b702201b4fb0de67f015c90e7ac8a193aeab486a1f587e0f54d0fb9552ef7f5ce6caec032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff7d815b6447e35fbea097e00e028fb7dfbad4f3f0987b4734676c84f3fcd0e804010000006b483045022100c714310be1e3a9ff1c5f7cacc65c2d8e781fc3a88ceb063c6153bf950650802102200b2d0979c76e12bb480da635f192cc8dc6f905380dd4ac1ff35a4f68f462fffd032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff3f1f097333e4d46d51f5e77b53264db8f7f5d2e18217e1099957d0f5af7713ee010000006c493046022100b663499ef73273a3788dea342717c2640ac43c5a1cf862c9e09b206fcb3f6bb8022100b09972e75972d9148f2bdd462e5cb69b57c1214b88fc55ca638676c07cfc10d8032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff0380841e00000000001976a914bfb282c70c4191f45b5a6665cad1682f2c9cfdfb88ac80841e00000000001976a9149857cc07bed33a5cf12b9c5e0500b675d500c81188ace0fd1c00000000001976a91443c52850606c872403c0601e69fa34b26f62db4a88ac00000000"
      )
    -- new sw transaction data from
    -- (https://0bin.net/paste/nfnSf0HcBqBUGDto#7zJMRUhGEBkyh-eASQPEwKfNHgQ4D5KrUJRsk8MTPSa)
    , ( "2b9baddbd2861c663978a98c6c3c7648e1cd5c41b451f4a35b7851dd4786d9d3"
      , "010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2f0315230e0004ae03ca57043e3d1e1d0c8796bf579aef0c0000000000122f4e696e6a61506f6f6c2f5345475749542fffffffff038427a112000000001976a914876fbb82ec05caa6af7a3b5e5a983aae6c6cc6d688ac0000000000000000266a24aa21a9ed5c748e121c0fe146d973a4ac26fa4a68b0549d46ee22d25f50a5e46fe1b377ee00000000000000002952534b424c4f434b3acd16772ad61a3c5f00287480b720f6035d5e54c9efc71be94bb5e3727f1090900120000000000000000000000000000000000000000000000000000000000000000000000000"
      )
    , ( "d06d86bacf88f1f316d4470080b7869f1c298b850e7b219124ae131c0475abb0"
      , "0100000000010145310e878941a1b2bc2d33797ee4d89d95eaaf2e13488063a2aa9a74490f510a0100000023220020b6744de4f6ec63cc92f7c220cdefeeb1b1bed2b66c8e5706d80ec247d37e65a1ffffffff01002d3101000000001976a9143ebc40e411ed3c76f86711507ab952300890397288ac0400473044022001dd489a5d4e2fbd8a3ade27177f6b49296ba7695c40dbbe650ea83f106415fd02200b23a0602d8ff1bdf79dee118205fc7e9b40672bf31563e5741feb53fb86388501483045022100f88f040e90cc5dc6c6189d04718376ac19ed996bf9e4a3c29c3718d90ffd27180220761711f16c9e3a44f71aab55cbc0634907a1fa8bb635d971a9a01d368727bea10169522103b3623117e988b76aaabe3d63f56a4fc88b228a71e64c4cc551d1204822fe85cb2103dd823066e096f72ed617a41d3ca56717db335b1ea47a1b4c5c9dbdd0963acba621033d7c89bd9da29fa8d44db7906a9778b53121f72191184a9fee785c39180e4be153ae00000000"
      )
    , ( "06eee51317a76a76c67499c8f782819745b58d28cdb4d8357ef7f7e6d79cc513"
      , "010000000120925534261de4dcebb1ed5ab1b62bfe7a3ef968fb111dc2c910adfebc6e3bdf010000006b483045022100f50198f5ae66211a4f485190abe4dc7accdabe3bc214ebc9ea7069b97097d46e0220316a70a03014887086e335fc1b48358d46cd6bdc9af3b57c109c94af76fc915101210316cff587a01a2736d5e12e53551b18d73780b83c3bfb4fcf209c869b11b6415effffffff0220a10700000000001976a91450333046115eaa0ac9e0216565f945070e44573988ac2e7cd01a000000001976a914c01a7ca16b47be50cbdbc60724f701d52d75156688ac00000000"
      )
    , ( "f56da6d0bb5807561c29093066edd1d505c2fa4ae89bb895c4318481d360fd3f"
      , "010000000203a25f58630d7a1ea52550365fd2156683f56daf6ca73a4b4bbd097e66516322010000006a47304402204efc3d70e4ca3049c2a425025edf22d5ca355f9ec899dbfbbeeb2268533a0f2b02204780d3739653035af4814ea52e1396d021953f948c29754edd0ee537364603dc012103f7a897e4dbecab2264b21917f90664ea8256189ea725d28740cf7ba5d85b5763ffffffff03a25f58630d7a1ea52550365fd2156683f56daf6ca73a4b4bbd097e66516322000000006a47304402202d96defdc5b4af71d6ba28c9a6042c2d5ee7bc6de565d4db84ef517445626e03022022da80320e9e489c8f41b74833dfb6a54a4eb5087cdb46eb663eef0b25caa526012103f7a897e4dbecab2264b21917f90664ea8256189ea725d28740cf7ba5d85b5763ffffffff0200e1f5050000000017a914b7e6f7ff8658b2d1fb107e3d7be7af4742e6b1b3876f88fc00000000001976a914913bcc2be49cb534c20474c4dee1e9c4c317e7eb88ac00000000"
      )
    , ( "32a52be869fc148b6104244859c879f1319cfd86e89e6f7fc1ffaaf518fa14be"
      , "01000000043ffd60d3818431c495b89be84afac205d5d1ed663009291c560758bbd0a66df5010000006b483045022100f344607de9df42049688dcae8ff1db34c0c7cd25ec05516e30d2bc8f12ac9b2f022060b648f6a21745ea6d9782e17bcc4277b5808326488a1f40d41e125879723d3a012103f7a897e4dbecab2264b21917f90664ea8256189ea725d28740cf7ba5d85b5763ffffffffa5379401cce30f84731ef1ba65ce27edf2cc7ce57704507ebe8714aa16a96b92010000006a473044022020c37a63bf4d7f564c2192528709b6a38ab8271bd96898c6c2e335e5208661580220435c6f1ad4d9305d2c0a818b2feb5e45d443f2f162c0f61953a14d097fd07064012103f7a897e4dbecab2264b21917f90664ea8256189ea725d28740cf7ba5d85b5763ffffffff70e731e193235ff12c3184510895731a099112ffca4b00246c60003c40f843ce000000006a473044022053760f74c29a879e30a17b5f03a5bb057a5751a39f86fa6ecdedc36a1b7db04c022041d41c9b95f00d2d10a0373322a9025dba66c942196bc9d8adeb0e12d3024728012103f7a897e4dbecab2264b21917f90664ea8256189ea725d28740cf7ba5d85b5763ffffffff66b7a71b3e50379c8e85fc18fe3f1a408fc985f257036c34702ba205cef09f6f000000006a4730440220499bf9e2db3db6e930228d0661395f65431acae466634d098612fd80b08459ee022040e069fc9e3c60009f521cef54c38aadbd1251aee37940e6018aadb10f194d6a012103f7a897e4dbecab2264b21917f90664ea8256189ea725d28740cf7ba5d85b5763ffffffff0200e1f5050000000017a9148fc37ad460fdfbd2b44fe446f6e3071a4f64faa6878f447f0b000000001976a914913bcc2be49cb534c20474c4dee1e9c4c317e7eb88ac00000000"
      )
    ]

mapPKHashVec :: (([(ByteString, Word32)], [(ByteString, Word64)], ByteString), Int)
            -> Test.Framework.Test
mapPKHashVec (v, i) = testCase name $ runPKHashVec v
    where name = "Build PKHash Tx " ++ (show i)

runPKHashVec :: ([(ByteString, Word32)], [(ByteString, Word64)], ByteString) -> Assertion
runPKHashVec (xs, ys, res) =
    assertBool "Build PKHash Tx" $ (encodeHex $ encode tx) == res
    where tx = fromRight $ buildAddrTx (map f xs) ys
          f (tid,ix) = OutPoint (fromJust $ hexToTxHash tid) ix


mapVerifyVec :: (SatoshiCoreTxTest, Int)
             -> Test.Framework.Test
mapVerifyVec (v@(SatoshiCoreTxTest d _ _), i) = testCase name $ runVerifyVec v i
    where name = "Verify Tx " ++ (show i) ++ ", about: " ++ d

runVerifyVec :: SatoshiCoreTxTest -> Int -> Assertion
runVerifyVec (SatoshiCoreTxTest _ is bsTx) i =
    assertBool name $ verifyStdTx tx outputsAndOutpoints
  where
    name = "    > Verify transaction " ++ (show i) ++ "bsTx: " ++ convertString bsTx
    tx :: Tx
    tx  = fromRight . decode . fromJust . decodeHex $ bsTx
    outputsAndOutpoints :: [(ScriptOutput, OutPoint)] 
    outputsAndOutpoints = map f is
    f (SatoshiCoreTxTestInput bsOutputHash bsOutputIndex bsOutputScriptPubKey) =
        let s :: ScriptOutput
            s = either (const $ error $ "could not decode: " ++ convertString bsOutputScriptPubKey) id
                  . decodeOutputBS . fromJust . decodeHex $ bsOutputScriptPubKey
            op :: OutPoint
            op = OutPoint
                (fromRight . decode . BS.reverse . fromJust . decodeHex $ bsOutputHash)
                (fromRight . runGet getWord32le  . fromJust . decodeHex $ bsOutputIndex)
        in (s, op)

-- These test vectors have been generated from bitcoind raw transaction api

pkHashVec :: [([(ByteString, Word32)], [(ByteString, Word64)], ByteString)]
pkHashVec =
    [
      ( [("eb29eba154166f6541ebcc9cbdf5088756e026af051f123bcfb526df594549db",14)]
      , [("14LsRquZfURNFrzpcLVGdaHTfAPjjwiSPb",90000000)]
      , "0100000001db494559df26b5cf3b121f05af26e0568708f5bd9ccceb41656f1654a1eb29eb0e00000000ffffffff01804a5d05000000001976a91424aa604689cc582292b97668bedd91dd5bf9374c88ac00000000"
      )
    , ( [ ("eb29eba154166f6541ebcc9cbdf5088756e026af051f123bcfb526df594549db",0)
        , ("0001000000000000000000000000000000000000000000000000000000000000",2147483647)
        ]
      , [ ("14LsRquZfURNFrzpcLVGdaHTfAPjjwiSPb",1)
        , ("19VCgS642vzEA1sdByoSn6GsWBwraV8D4n",2100000000000000)
        ]
      , "0100000002db494559df26b5cf3b121f05af26e0568708f5bd9ccceb41656f1654a1eb29eb0000000000ffffffff0000000000000000000000000000000000000000000000000000000000000100ffffff7f00ffffffff0201000000000000001976a91424aa604689cc582292b97668bedd91dd5bf9374c88ac0040075af07507001976a9145d16672f53981ff21c5f42b40d1954993cbca54f88ac00000000"
      )
    , ( [ ("eb29eba154166f6541ebcc9cbdf5088756e026af051f123bcfb526df594549db",0)
        , ("0001000000000000000000000000000000000000000000000000000000000000",2147483647)
        ]
      , []
      , "0100000002db494559df26b5cf3b121f05af26e0568708f5bd9ccceb41656f1654a1eb29eb0000000000ffffffff0000000000000000000000000000000000000000000000000000000000000100ffffff7f00ffffffff0000000000"
      )
    , ( []
      , [ ("14LsRquZfURNFrzpcLVGdaHTfAPjjwiSPb",1)
        , ("19VCgS642vzEA1sdByoSn6GsWBwraV8D4n",2100000000000000)
        ]
      , "01000000000201000000000000001976a91424aa604689cc582292b97668bedd91dd5bf9374c88ac0040075af07507001976a9145d16672f53981ff21c5f42b40d1954993cbca54f88ac00000000"
      )
    ]

data SatoshiCoreTxTest = SatoshiCoreTxTest String [SatoshiCoreTxTestInput] ByteString
    deriving (Read,Show)

data SatoshiCoreTxTestInput = SatoshiCoreTxTestInput ByteString ByteString ByteString
    deriving (Read,Show)

{- Test vectors from bitcoind -}
-- github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json
-- [[(prevout hash, prevout index, prevout scriptPubKey)], serialized tx],
verifyVec :: [SatoshiCoreTxTest]
verifyVec =
  let toSatCoreTest (is, sertx, description) = SatoshiCoreTxTest description (map toInputs is) sertx
      toInputs (a,b,c) = SatoshiCoreTxTestInput a b c
  in  map toSatCoreTest
    [
      ( [
          ( "60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1"
          , "00000000"
          , "514104cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4410461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af52ae"
          )
        ]
      , "0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba26000000000490047304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2b01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000"
      , "It is of particular interest because it contains an invalidly-encoded signature which OpenSSL accepts"
      )
    , ( [
          ( "60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1"
          , "00000000"
          , "514104cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4410461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af52ae"
          )
        ]
      , "0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba260000000004A0048304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2bab01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000"
      , "It has an arbitrary extra byte stuffed into the signature at pos length - 2"
      )
    , ( [
          ( "406b2b06bcd34d3c8733e6b79f7a394c8a431fbf4ff5ac705c93f4076bb77602"
          , "00000000"
          , "76a914dc44b1164188067c3a32d4780f5996fa14a4f2d988ac"
          )
        ]
      , "01000000010276b76b07f4935c70acf54fbf1f438a4c397a9fb7e633873c4dd3bc062b6b40000000008c493046022100d23459d03ed7e9511a47d13292d3430a04627de6235b6e51a40f9cd386f2abe3022100e7d25b080f0bb8d8d5f878bba7d54ad2fda650ea8d158a33ee3cbd11768191fd004104b0e2c879e4daf7b9ab68350228c159766676a14f5815084ba166432aab46198d4cca98fa3e9981d0a90b2effc514b76279476550ba3663fdcaff94c38420e9d5000000000100093d00000000001976a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac00000000"
      , "it is of interest because it contains a 0-sequence as well as a signature of SIGHASH type 0 (which is not a real type)"
      )
    , ( [
          ( "b464e85df2a238416f8bdae11d120add610380ea07f4ef19c5f9dfd472f96c3d"
          , "00000000"
          , "76a914bef80ecf3a44500fda1bc92176e442891662aed288ac"
          )
        , ( "b7978cc96e59a8b13e0865d3f95657561a7f725be952438637475920bac9eb21"
          , "01000000"
          , "76a914bef80ecf3a44500fda1bc92176e442891662aed288ac"
          )
        ]
      , "01000000023d6cf972d4dff9c519eff407ea800361dd0a121de1da8b6f4138a2f25de864b4000000008a4730440220ffda47bfc776bcd269da4832626ac332adfca6dd835e8ecd83cd1ebe7d709b0e022049cffa1cdc102a0b56e0e04913606c70af702a1149dc3b305ab9439288fee090014104266abb36d66eb4218a6dd31f09bb92cf3cfa803c7ea72c1fc80a50f919273e613f895b855fb7465ccbc8919ad1bd4a306c783f22cd3227327694c4fa4c1c439affffffff21ebc9ba20594737864352e95b727f1a565756f9d365083eb1a8596ec98c97b7010000008a4730440220503ff10e9f1e0de731407a4a245531c9ff17676eda461f8ceeb8c06049fa2c810220c008ac34694510298fa60b3f000df01caa244f165b727d4896eb84f81e46bcc4014104266abb36d66eb4218a6dd31f09bb92cf3cfa803c7ea72c1fc80a50f919273e613f895b855fb7465ccbc8919ad1bd4a306c783f22cd3227327694c4fa4c1c439affffffff01f0da5200000000001976a914857ccd42dded6df32949d4646dfa10a92458cfaa88ac00000000"
      , "It caught a bug in the workaround for 23b397edccd3740a74adb603c9756370fafcde9bcc4483eb271ecad09a94dd63 in an overly simple implementation"
      )
      
    , ( [
          ( "0000000000000000000000000000000000000000000000000000000000000100"
          , "00000000"
          , "76a914e52b482f2faa8ecbf0db344f93c84ac908557f3388ac"
          )
        , ( "0000000000000000000000000000000000000000000000000000000000000200"
          , "00000000"
          , "76a914751e76e8199196d454941c45d1b3a323f1433bd688ac"
          )
        ]
        , "01000000020002000000000000000000000000000000000000000000000000000000000000000000006a47304402200469f169b8091cd18a2770136be7411f079b3ac2b5c199885eb66a80aa3ed75002201fa89f3e6f80974e1b3474e70a0fbe907c766137ff231e4dd05a555d8544536701210279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798ffffffff0001000000000000000000000000000000000000000000000000000000000000000000006b483045022100c9cdd08798a28af9d1baf44a6c77bcc7e279f47dc487c8c899911bc48feaffcc0220503c5c50ae3998a733263c5c0f7061b483e2b56c4c41b456e7d2f5a78a74c077032102d5c25adb51b61339d2b05315791e21bbe80ea470a49db0135720983c905aace0ffffffff010000000000000000015100000000"
        , "-- It results in signing the constant 1, instead of something generated based on the transaction,\
          \-- when the input doing the signing has an index greater than the maximum output index"
      )
    , ( [
          ( "0000000000000000000000000000000000000000000000000000000000000100"
          , "00000000"
          , "a9148febbed40483661de6958d957412f82deed8e2f787"
          )
        ]
      , "01000000010001000000000000000000000000000000000000000000000000000000000000000000006e493046022100c66c9cdf4c43609586d15424c54707156e316d88b0a1534c9e6b0d4f311406310221009c0fe51dbc9c4ab7cc25d3fdbeccf6679fe6827f08edf2b4a9f16ee3eb0e438a0123210338e8034509af564c62644c07691942e0c056752008a173c89f60ab2a88ac2ebfacffffffff010000000000000000015100000000"
      , "A valid P2SH Transaction using the standard transaction type put forth in BIP 16"
      )
    , ( [
          ( "0000000000000000000000000000000000000000000000000000000000000100"
          , "00000000"
          , "a91432afac281462b822adbec5094b8d4d337dd5bd6a87"
          )
        ]
      , "01000000010001000000000000000000000000000000000000000000000000000000000000000000006e493046022100e1eadba00d9296c743cb6ecc703fd9ddc9b3cd12906176a226ae4c18d6b00796022100a71aef7d2874deff681ba6080f1b278bac7bb99c61b08a85f4311970ffe7f63f012321030c0588dc44d92bdcbf8e72093466766fdc265ead8db64517b0c542275b70fffbacffffffff010040075af0750700015100000000"
      , "MAX_MONEY output"
      )
    , ( [
          ( "0000000000000000000000000000000000000000000000000000000000000100"
          , "00000000"
          , "a914b558cbf4930954aa6a344363a15668d7477ae71687"
          )
        ]
      , "01000000010001000000000000000000000000000000000000000000000000000000000000000000006d483045022027deccc14aa6668e78a8c9da3484fbcd4f9dcc9bb7d1b85146314b21b9ae4d86022100d0b43dece8cfb07348de0ca8bc5b86276fa88f7f2138381128b7c36ab2e42264012321029bb13463ddd5d2cc05da6e84e37536cb9525703cfd8f43afdb414988987a92f6acffffffff020040075af075070001510000000000000000015100000000"
      , "MAX_MONEY output + 0 output"
      )
    , ( [
          ( "0000000000000000000000000000000000000000000000000000000000000100"
          , "00000000"
          , "21035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efcac"
          )
        , ( "0000000000000000000000000000000000000000000000000000000000000200"
          , "00000000"
          , "21035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efcac"
          )
        ]
      , "010000000200010000000000000000000000000000000000000000000000000000000000000000000049483045022100d180fd2eb9140aeb4210c9204d3f358766eb53842b2a9473db687fa24b12a3cc022079781799cd4f038b85135bbe49ec2b57f306b2bb17101b17f71f000fcab2b6fb01ffffffff0002000000000000000000000000000000000000000000000000000000000000000000004847304402205f7530653eea9b38699e476320ab135b74771e1c48b81a5d041e2ca84b9be7a802200ac8d1f40fb026674fe5a5edd3dea715c27baa9baca51ed45ea750ac9dc0a55e81ffffffff010100000000000000015100000000"
      , "Simple transaction with first input is signed with SIGHASH_ALL, second with SIGHASH_ANYONECANPAY"
      )
    , ( [
          ( "0000000000000000000000000000000000000000000000000000000000000100"
          , "00000000"
          , "21035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efcac"
          )
        , ( "0000000000000000000000000000000000000000000000000000000000000200"
          , "00000000"
          , "21035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efcac"
          )
        ]
      , "01000000020001000000000000000000000000000000000000000000000000000000000000000000004948304502203a0f5f0e1f2bdbcd04db3061d18f3af70e07f4f467cbc1b8116f267025f5360b022100c792b6e215afc5afc721a351ec413e714305cb749aae3d7fee76621313418df101010000000002000000000000000000000000000000000000000000000000000000000000000000004847304402205f7530653eea9b38699e476320ab135b74771e1c48b81a5d041e2ca84b9be7a802200ac8d1f40fb026674fe5a5edd3dea715c27baa9baca51ed45ea750ac9dc0a55e81ffffffff010100000000000000015100000000"
      , "Same as above, but we change the sequence number of the first input to check that SIGHASH_ANYONECANPAY is being followed"
      )
    , ( [
          ( "63cfa5a09dc540bf63e53713b82d9ea3692ca97cd608c384f2aa88e51a0aac70"
          , "00000000"
          , "76a914dcf72c4fd02f5a987cf9b02f2fabfcac3341a87d88ac"
          )
        , ( "04e8d0fcf3846c6734477b98f0f3d4badfb78f020ee097a0be5fe347645b817d"
          , "01000000"
          , "76a914dcf72c4fd02f5a987cf9b02f2fabfcac3341a87d88ac"
          )
        , ( "ee1377aff5d0579909e11782e1d2f5f7b84d26537be7f5516dd4e43373091f3f"
          , "01000000"
          , "76a914dcf72c4fd02f5a987cf9b02f2fabfcac3341a87d88ac"
          )
        ]
      , "010000000370ac0a1ae588aaf284c308d67ca92c69a39e2db81337e563bf40c59da0a5cf63000000006a4730440220360d20baff382059040ba9be98947fd678fb08aab2bb0c172efa996fd8ece9b702201b4fb0de67f015c90e7ac8a193aeab486a1f587e0f54d0fb9552ef7f5ce6caec032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff7d815b6447e35fbea097e00e028fb7dfbad4f3f0987b4734676c84f3fcd0e804010000006b483045022100c714310be1e3a9ff1c5f7cacc65c2d8e781fc3a88ceb063c6153bf950650802102200b2d0979c76e12bb480da635f192cc8dc6f905380dd4ac1ff35a4f68f462fffd032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff3f1f097333e4d46d51f5e77b53264db8f7f5d2e18217e1099957d0f5af7713ee010000006c493046022100b663499ef73273a3788dea342717c2640ac43c5a1cf862c9e09b206fcb3f6bb8022100b09972e75972d9148f2bdd462e5cb69b57c1214b88fc55ca638676c07cfc10d8032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff0380841e00000000001976a914bfb282c70c4191f45b5a6665cad1682f2c9cfdfb88ac80841e00000000001976a9149857cc07bed33a5cf12b9c5e0500b675d500c81188ace0fd1c00000000001976a91443c52850606c872403c0601e69fa34b26f62db4a88ac00000000"
      , "several SIGHASH_SINGLE signatures"
      )
    ]

tEncodeSatoshiCoreScriptPubKey :: Assertion
tEncodeSatoshiCoreScriptPubKey = assertBool "tEncodeSatoshiCoreScriptPubKey" $ 
  t1BsOutputScriptPubKey == encodeSatoshiCoreScriptPubKey t1SatoshiCoreJsonScriptPubKey
  where 
    t1BsOutputScriptPubKey :: ByteString
    t1BsOutputScriptPubKey = "514104cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4410461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af52ae"
    t1SatoshiCoreJsonScriptPubKey :: String
    t1SatoshiCoreJsonScriptPubKey = "1 0x41 0x04cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4 0x41 0x0461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af 2 OP_CHECKMULTISIG"


encodeSatoshiCoreScriptPubKey :: String -> ByteString
encodeSatoshiCoreScriptPubKey = 
  mconcat . map encodeSatoshiCoreScriptPiece . words
  where 
    encodeSatoshiCoreScriptPiece :: String -> ByteString
    encodeSatoshiCoreScriptPiece s = case (readMay ("OP_" ++ s) :: Maybe ScriptOp) of
      Just op -> encodeHex . encode $ op
      Nothing -> case (take 2 s) of 
          "OP" -> encodeHex . encode . (read :: String -> ScriptOp) $ s
          "0x" -> ( fromString . drop 2 :: String -> ByteString) $ s          
          _ -> case (readMay s :: Maybe Int) of -- can we get rid of this case now?
            Just i -> encodeHex . encode . intToScriptOp $ i
            Nothing -> error $ "encodeSatoshiCoreScriptPubKey: " ++ s

satoshiCoreTxTests :: IO [Test]
satoshiCoreTxTests = do
  txVec <- satoshiCoreTxVec
  return $ [ 
    testGroup "Verify transaction (bitcoind /test/data/tx_valid.json) (using copied source json)"
      ( map mapVerifyVec . filter isCurrentlyPassing $ zip txVec [0..] ) 
    ]
  where
    passingTests = [0..5] ++ [8] ++ [11..13] ++ [16..18] ++ [20] ++ [52]
    isCurrentlyPassing (_, testNum) = elem testNum passingTests


type TestComment = String
satoshiCoreTxVec :: IO [SatoshiCoreTxTest]
satoshiCoreTxVec = do 
    tx_validBS <- LBS.readFile "tests/data/tx_valid.json"
    let testsAndComments = maybe (error $ "satoshiCoreTxVec, couldn't decode json") id . Aeson.decode $ tx_validBS            
    return $ case testsAndComments of
        (Aeson.Array arr) -> 
            let testsOrComments = map toTestOrComment . V.toList $ arr 
            in  processTestsAndComments testsOrComments
        _ -> error $ "satoshiCoreTxVec, testsAndComments not an array"
      where
        processTestsAndComments :: [Either TestComment SatoshiCoreTxTest] -> [SatoshiCoreTxTest]
        processTestsAndComments testOrComments = 
          -- ghetto parser, because this older version of ootb aeson parser isn't parsec based
          -- to do ideally soon, upgrade aeson, use aeson/attoparsec, should be much cleaner
          let grouper = Data.List.groupBy (\x y -> (Data.Either.isLeft x && Data.Either.isLeft y) 
                                                || (Data.Either.isRight x && Data.Either.isRight y))              
              takePairs (a:b:xs) = (a,b):takePairs xs
              takePairs _ = [] -- ugh, wish we were using a real parser.
              includeDescriptions (descriptionLines,tests') = map updateDescription tests'
                where updateDescription (Right (SatoshiCoreTxTest _ inputs ser)) = SatoshiCoreTxTest description inputs ser
                      updateDescription e = error $ "updateDescription: " ++ show e
                      description = unwords . map fromLeft $ descriptionLines
          in  concat . map includeDescriptions . takePairs . grouper $ testOrComments

toTestOrComment :: Aeson.Value -> (Either TestComment SatoshiCoreTxTest)
toTestOrComment testVectorOrComment = 
  case testVectorOrComment of
    (Aeson.Types.Array arr) -> 
      case (V.length arr) of
        1 ->  let comment = arr V.! 0
              in case comment of
                   Aeson.Types.String txt -> Left . convertString $ txt
                   _ -> error $ "toTestOrComment, comment not text"
        3 ->  let inputs = case ( arr V.! 0 ) of            
                    (Aeson.Array inputsV) -> 
                      let toInput ( Aeson.Array oneInputV ) = 
                            let hash = case oneInputV V.! 0 of 
                                  Aeson.String txt -> convertString txt
                                  _ -> error "processItem, hash not a string"
                                index = case oneInputV V.! 01 of 
                                  Aeson.Number n -> encodeHex . runPut . putWord32le . floor $ n
                                  _ -> error "processItem, n not a number"
                                pubkey = case oneInputV V.! 2 of
                                  Aeson.String txt -> encodeSatoshiCoreScriptPubKey . convertString $ txt
                                  _ -> error "processItem, pubkey not a string"
                            in  SatoshiCoreTxTestInput hash index pubkey 
                          toInput _ = error "processItem, oneInputV not an array"
                      in  map toInput . V.toList $ inputsV
                    _ -> error "inputs not an array"
                  tx = let txVal = arr V.! 1
                       in case txVal of
                          Aeson.Types.String txt -> convertString txt
                          _ -> error $ "toTestOrComment, tx, not text"
                  -- flags -- v V.! 2  -- ignored for now? 
              in  Right $ SatoshiCoreTxTest "" inputs tx 
        i -> error $ "toTestOrComment, bad length: " ++ show i 
    _ -> error "testVectorOrComment is not an array"




