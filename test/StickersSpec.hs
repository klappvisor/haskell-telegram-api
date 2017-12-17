{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module StickersSpec (spec) where

import           Data.Maybe
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Paths_telegram_api
import           System.FilePath
import           System.Random
import           Test.Hspec
import           TestCore
import           Web.Telegram.API.Bot

spec :: Token -> ChatId -> Text -> Spec
spec token chatId _ = do
  manager <- runIO $ newManager tlsManagerSettings
  dataDir <- runIO getDataDir

  res <- runIO $ runTelegramClient token manager getMeM
  let testFile name = dataDir </> "test-data" </> name
      Right Response { result = meRes } = res
      botUsername = fromMaybe "???" $ user_username meRes
      ChatId chatId' = chatId
      userId :: Int = fromIntegral chatId'
      stickerFile1 = localFileUpload $ testFile "sticker_1.png"
      stickerFile2 = localFileUpload $ testFile "sticker_2.png"

  describe "/getStickerSet" $ do
    it "should get sticker set" $ do
      let stickerName = "non_existing_test_set_by_" <> botUsername
      res <- runTelegramClient token manager $ getStickerSetM stickerName
      nosuccess res

  describe "/uploadStickerFile" $ do
    it "should upload sticker PNG" $ do
      let uploadRequest = UploadStickerFileRequest userId stickerFile1
      Right res <- runTelegramClient token manager $ uploadStickerFileM uploadRequest
      (T.null . file_id . result) res `shouldBe` False

  describe "/createNewStickerSet" $ do
    it "should create sticker set" $ do
      rnd :: Integer <- randomRIO (10000, 99999)
      let stickerSetName = "set_" <> (showText rnd) <> "_by_" <> botUsername
          request = CreateNewStickerSetRequest userId stickerSetName "Haskell Bot API Test Set" stickerFile1 "😃" (Just True) Nothing
      res <- runTelegramClient token manager $ do
        _ <- createNewStickerSetM' request
        getStickerSetM stickerSetName
      success res
      let Right Response { result = set } = res
      stcr_set_name set `shouldBe` stickerSetName

  describe "StickerSet CRUD" $ do
    let setTitle = "Haskell Telegram Bot API Test"
        setName = "sticker_set_by_" <> botUsername
    runIO $ print setName
    _ <- runIO $ runTelegramClient token manager $ do
      uploadResult <- uploadStickerFileM $ UploadStickerFileRequest userId stickerFile2
      let fileId = (file_id . result) uploadResult
          request = CreateNewStickerSetRequest userId setName setTitle fileId "👍" (Just True) Nothing
      createNewStickerSetM request
    it "should add new sticker to the sicker set" $ do
      Right (set, setAfter) <- runTelegramClient token manager $ do
        let maskPosition = MaskPosition Eyes 0.0 0.0 0.0
            uploadRequest = AddStickerToSetRequest userId setName stickerFile1 "😃" (Just maskPosition)
        _ <- uploadStickerToSetM uploadRequest
        Response { result = set } <- getStickerSetM setName
        let firstId = sticker_file_id . head . stcr_set_stickers
        _ <- sendStickerM $ sendStickerRequest chatId $ firstId set
        let lastId = sticker_file_id . last . stcr_set_stickers
        _ <- deleteStickerFromSetM $ lastId set
        Response { result = setAfter } <- getStickerSetM setName
        return (set, setAfter)
      stcr_set_contains_masks set `shouldBe` True
      let stickerCount = length . stcr_set_stickers
      stickerCount set `shouldBe` 2
      stickerCount setAfter `shouldBe` 1



