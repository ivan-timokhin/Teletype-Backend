{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.TDLib.API
  ( TDLibClient
  , withClient
  , WithExtra(..)
  , Parameters(..)
  , AuthorizationState(..)
  , Update(..)
  , _AuthorizationState
  , Object(..)
  , _UpdateObj
  , recv
  , Function(..)
  , send
  , createClient
  , destroyClient
  ) where

import qualified Telegram.TDLib.API.AesonOptions as AOpt
import Telegram.TDLib.Bindings
  ( TDLibClient
  , createClient
  , destroyClient
  , recvJSON
  , sendJSON
  )

import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON, toEncoding)
  , (.:?)
  , eitherDecodeStrict'
  , encode
  )
import qualified Data.Aeson as A
import Data.Aeson.TH (deriveJSON)
import Control.Exception (bracket, Exception, throwIO)
import Data.Int (Int32, Int64)
import Data.ByteString.Lazy (toStrict)
import Control.Lens.TH (makePrisms)
import Data.Traversable (for)
import Data.ByteString (ByteString)
import Control.Monad (join)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)

withClient :: (TDLibClient -> IO a) -> IO a
withClient = bracket createClient destroyClient

data WithExtra a b = WithExtra
  { payload :: a
  , extra :: Maybe b
  } deriving (Show, Eq, Functor, Foldable, Traversable)

instance (FromJSON a, FromJSON b) => FromJSON (WithExtra a b) where
  parseJSON =
    A.withObject "WithExtra" $ \v ->
      WithExtra <$> parseJSON (A.Object v) <*> (join <$> v .:? "@extra")

instance (ToJSON a, ToJSON b) => ToJSON (WithExtra a b) where
  toJSON (WithExtra p e) =
    case toJSON p of
      A.Object o -> A.Object (HM.insert "@extra" (toJSON e) o)
      _ ->
        error "Whoops! WithExtra expects Object-like type as a first argument"

newtype LongNumber a = LongNumber
  { getLongNumber :: a
  } deriving ( Eq
             , Ord
             , Show
             , Enum
             , Num
             , Real
             , Fractional
             , RealFrac
             , Integral
             , Floating
             , Bounded
             )

instance Read a => FromJSON (LongNumber a) where
  parseJSON =
    A.withText "LongNumber" $ \txt ->
      case reads (T.unpack txt) of
        [(v, "")] -> pure (LongNumber v)
        _ -> fail $ "Could not parse " ++ show txt ++ " as a number."

instance Show a => ToJSON (LongNumber a) where
  toJSON = toJSON . show . getLongNumber
  toEncoding = toEncoding . show . getLongNumber

-- | Contains parameters for TDLib initialisation.
data Parameters = Parameters
  { useTestDC :: Bool -- ^ If set to true, the Telegram test
                      -- environment will be used instead of the
                      -- production environment.
  , databaseDirectory :: Text -- ^ The path to the directory for the
                              -- persistent database; if empty, the
                              -- current working directory will be
                              -- used.
  , filesDirectory :: Text -- ^ The path to the directory for storing
                           -- files; if empty, database_directory will
                           -- be used.
  , useFileDatabase :: Bool -- ^ If set to true, information about
                            -- downloaded and uploaded files will be
                            -- saved between application restarts.
  , useChatInfoDatabase :: Bool -- ^ If set to true, the library will
                                -- maintain a cache of users, basic
                                -- groups, supergroups, channels and
                                -- secret chats. Implies
                                -- use_file_database.
  , useMessageDatabase :: Bool -- ^ If set to true, the library will
                               -- maintain a cache of chats and
                               -- messages. Implies
                               -- useChatInfoDatabase.
  , useSecretChats :: Bool -- ^ If set to true, support for secret
                           -- chats will be enabled.
  , apiID :: Int32 -- ^ Application identifier for Telegram API
                   -- access, which can be obtained at
                   -- https://my.telegram.org.
  , apiHash :: Text -- ^ Application identifier hash for Telegram API
                    -- access, which can be obtained at
                    -- https://my.telegram.org.
  , systemLanguageCode :: Text -- ^ IETF language tag of the user's
                               -- operating system language; must be
                               -- non-empty.
  , deviceModel :: Text -- ^ Model of the device the application is
                        -- being run on; must be non-empty.
  , systemVersion :: Text -- ^ Version of the operating system the
                          -- application is being run on; must be
                          -- non-empty.
  , applicationVersion :: Text -- ^ Application version; must be
                               -- non-empty.
  , enableStorageOptimizer :: Bool -- ^ If set to true, old files
                                   -- will automatically be deleted.
  , ignoreFileNames :: Bool -- ^ If set to true, original file names
                            -- will be ignored. Otherwise, downloaded
                            -- files will be saved under names as
                            -- close as possible to the original
                            -- name.
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Parameters

data ConnectionState
  = ConnectionStateConnecting
  | ConnectionStateConnectingToProxy
  | ConnectionStateReady
  | ConnectionStateUpdating
  | ConnectionStateWaitingForNetwork
  deriving (Eq, Show, Ord, Enum, Bounded, Read)

deriveJSON AOpt.namedCtors ''ConnectionState

data OptionValue
  = OptionValueBoolean { _boolValue :: Bool }
  | OptionValueEmpty
  | OptionValueInteger { _intValue :: Int32 }
  | OptionValueString { _textValue :: Text }
  deriving (Eq, Show)

deriveJSON
  (AOpt.namedCtors
     { A.fieldLabelModifier =
         A.fieldLabelModifier AOpt.namedCtors . dropWhile (/= 'V')
     })
  ''OptionValue

-- A better idea may be to go Object route and have separate type for
-- each update alternative
data Ok =
  Ok
  deriving (Eq, Show, Ord, Enum, Bounded, Read)

deriveJSON AOpt.namedCtors ''Ok

data Error = Error
  { code :: Int32
  , message :: Text
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Error

newtype AccountTtl = AccountTtl
  { days :: Int32
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''AccountTtl

data LocalFile = LocalFile
  { path :: Text
  , canBeDownloaded :: Bool
  , canBeDeleted :: Bool
  , isDownloadingActive :: Bool
  , isDownloadingCompleted :: Bool
  , downloadedPrefixSize :: Int32
  , downloadedSize :: Int32
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''LocalFile

data RemoteFile = RemoteFile
  { _id :: Text
  , isUploadingActive :: Bool
  , isUploadingCompleted :: Bool
  , uploadedSize :: Int32
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''RemoteFile

data File = File
  { _id :: Int32
  , size :: Int32
  , expectedSize :: Int32
  , local :: LocalFile
  , remote :: RemoteFile
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''File

data PhotoSize = PhotoSize
  { _type :: Text
  , photo :: File
  , width :: Int32
  , height :: Int32
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''PhotoSize

data Animation = Animation
  { duration :: Int32
  , width :: Int32
  , height :: Int32
  , fileName :: Text
  , mimeType :: Text
  , thumbnail :: PhotoSize
  , animation :: File
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Animation

newtype Animations = Animations
  { animations :: Vector Animation
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Animations

data Audio = Audio
  { duration :: Int32
  , title :: Text
  , performer :: Text
  , fileName :: Text
  , mimeType :: Text
  , albumCoverThumbnail :: PhotoSize
  , audio :: File
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Audio

data AuthenticationCodeType
  = AuthenticationCodeTypeCall { length :: Int32 }
  | AuthenticationCodeTypeFlashCall { _pattern :: Text }
  | AuthenticationCodeTypeSms { length :: Int32 }
  | AuthenticationCodeTypeTelegramMessage { length :: Int32 }
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''AuthenticationCodeType

data AuthenticationCodeInfo = AuthenticationCodeInfo
  { phoneNumber :: Text
  , _type :: AuthenticationCodeType
  , nextType :: AuthenticationCodeType
  , timeout :: Int32
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''AuthenticationCodeInfo

data AuthorizationState
  = WaitTdlibParameters
  | WaitEncryptionKey { isEncrypted :: Bool }
  | WaitCode { isRegistered :: Bool
             , codeInfo :: AuthenticationCodeInfo }
  | WaitPassword { passwordHint :: Text
                 , hasRecoveryEmailAddress :: Bool
                 , recoveryEmailAddressPattern :: Text }
  | WaitPhoneNumber
  | Closed
  | Closing
  | LoggingOut
  | Ready
  deriving (Eq, Show)

deriveJSON (AOpt.prefixedCtors "authorizationState") ''AuthorizationState

data ChatMemberStatus
  = Administrator { canBeEdited :: Bool
                  , canChangeInfo :: Bool
                  , canPostMessages :: Bool
                  , canEditMessages :: Bool
                  , canDeleteMessages :: Bool
                  , canInviteUsers :: Bool
                  , canRestrictMembers :: Bool
                  , canPinMessages :: Bool
                  , canPromoteMembers :: Bool }
  | Banned { bannedUntilDate :: Int32 }
  | Creator { isMember :: Bool }
  | Left
  | Member
  | Restricted { isMember :: Bool
               , restrictedUntilDate :: Int32
               , canSendMessages :: Bool
               , canSendMediaMessages :: Bool
               , canSendOtherMessages :: Bool
               , canAddWebPagePreviews :: Bool }
  deriving (Eq, Show)

deriveJSON (AOpt.prefixedCtors "chatMemberStatus") ''ChatMemberStatus

data BasicGroup = BasicGroup
  { _id :: Int32
  , memberCount :: Int32
  , status :: ChatMemberStatus
  , everyoneIsAdministrator :: Bool
  , isActive :: Bool
  , upgradedToSupergroupId :: Int32
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''BasicGroup

data BotCommand = BotCommand
  { command :: Text
  , description :: Text
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''BotCommand

data BotInfo = BotInfo
  { description :: Text
  , commands :: Vector BotCommand
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''BotInfo

data ChatMember = ChatMember
  { userId :: Int32
  , inviterUserId :: Int32
  , joinedChatDate :: Int32
  , status :: ChatMemberStatus
  , botInfo :: Maybe BotInfo
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ChatMember

data BasicGroupFullInfo = BasicGroupFullInfo
  { creatorUserId :: Int32
  , members :: Vector ChatMember
  , inviteLink :: Text
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''BasicGroupFullInfo

data CallDiscardReason
  = Declined
  | Disconnected
  | Empty
  | HungUp
  | Missed
  deriving (Eq, Show, Ord, Enum, Bounded)

deriveJSON AOpt.namedCtors ''CallDiscardReason

data CallProtocol = CallProtocol
  { udpP2p :: Bool
  , udpReflector :: Bool
  , minLayer :: Int32
  , maxLayer :: Int32
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''CallProtocol

data CallConnection = CallConnection
  { _id :: LongNumber Int64
  , ip :: Text
  , ipv6 :: Text
  , port :: Int32
  , peerTag :: Text
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''CallConnection

data CallState
  = CallStateDiscarded { reason :: CallDiscardReason
                       , needRating :: Bool
                       , needDebugInformation :: Bool }
  | CallStateError { _error :: Error }
  | CallStateExchangingKeys
  | CallStateHangingUp
  | CallStatePending { isCreated :: Bool
                     , isReceived :: Bool }
  | CallStateReady { protocol :: CallProtocol
                   , connections :: Vector CallConnection
                   , config :: Text
                   , encryptionKey :: Text
                   , emojis :: Vector Text }
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''CallState

data Call = Call
  { _id :: Int32
  , userId :: Int32
  , isOutgoing :: Bool
  , state :: CallState
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Call

data CallbackQueryAnswer = CallbackQueryAnswer
  { text :: Text
  , showAlert :: Bool
  , url :: Text
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''CallbackQueryAnswer

data CallbackQueryPayload
  = CallbackQueryPayloadData { _data :: Text }
  | CallbackQueryPayloadGame { gameShortName :: Text }
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''CallbackQueryPayload

newtype CallId = CallId
  { _id :: Int32
  } deriving (Show, Eq)

deriveJSON AOpt.namedCtors ''CallId

data ChatType
  = ChatTypeBasicGroup { basicGroupId :: Int32 }
  | ChatTypePrivate { userId :: Int32 }
  | ChatTypeSecret { secretChatId :: Int32
                   , userId :: Int32 }
  | ChatTypeSupergroup { supergroupId :: Int32
                       , isChannel :: Bool }
  deriving (Show, Eq)

deriveJSON AOpt.namedCtors ''ChatType

data ChatPhoto = ChatPhoto
  { small :: File
  , big :: File
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ChatPhoto

data MessageSendingState
  = MessageSendingStateFailed
  | MessageSendingStatePending
  deriving (Eq, Show, Enum, Bounded, Ord)

deriveJSON AOpt.namedCtors ''MessageSendingState

data MessageForwardInfo
  = MessageForwardedFromUser { senderUserId :: Int32
                             , date :: Int32
                             , forwardedFromChatId :: LongNumber Int64
                             , forwardedFromMessageId :: LongNumber Int64 }
  | MessageForwardedPost { chatId :: LongNumber Int64
                         , authorSignature :: Text
                         , date :: Int32
                         , messageId :: LongNumber Int64
                         , forwardedFromChatId :: LongNumber Int64
                         , forwardedFromMessageId :: LongNumber Int64 }
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''MessageForwardInfo

data TextEntityType
  = TextEntityTypeBold
  | TextEntityTypeBotCommand
  | TextEntityTypeCashtag
  | TextEntityTypeCode
  | TextEntityTypeEmailAddress
  | TextEntityTypeHashtag
  | TextEntityTypeItalic
  | TextEntityTypeMention
  | TextEntityTypeMentionName { userId :: Int32 }
  | TextEntityTypePhoneNumber
  | TextEntityTypePre
  | TextEntityTypePreCode { language :: Text }
  | TextEntityTypeTextUrl { url :: Text }
  | TextEntityTypeUrl
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''TextEntityType

data TextEntity = TextEntity
  { offset :: Int32
  , length :: Int32
  , _type :: TextEntityType
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''TextEntity

data FormattedText = FormattedText
  { text :: Text
  , entities :: Vector TextEntity
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''FormattedText

data Photo = Photo
  { id :: LongNumber Int64
  , hasStickers :: Bool
  , sizes :: Vector PhotoSize
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Photo

data Contact = Contact
  { phoneNumber :: Text
  , firstName :: Text
  , lastName :: Text
  , userId :: Int32
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Contact

data Document = Document
  { fileName :: Text
  , mimeType :: Text
  , thumbnail :: PhotoSize
  , document :: File
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Document

data Game = Game
  { id :: LongNumber Int64
  , shortName :: Text
  , title :: Text
  , text :: FormattedText
  , description :: Text
  , photo :: Photo
  , animation :: Animation
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Game

data Location = Location
  { latitude :: Double
  , longitude :: Double
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Location

data ShippingAddress = ShippingAddress
  { countryCode :: Text
  , state :: Maybe Text
  , city :: Text
  , streetLine1 :: Text
  , streetLine2 :: Text
  , postalCode :: Text
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ShippingAddress

data OrderInfo = OrderInfo
  { name :: Text
  , phoneNumber :: Text
  , emailAddress :: Text
  , shippingAddress :: ShippingAddress
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''OrderInfo

data MaskPoint
  = MaskPointChin
  | MaskPointEyes
  | MaskPointForehead
  | MaskPointMouth
  deriving (Eq, Show, Ord, Enum, Bounded)

deriveJSON AOpt.namedCtors ''MaskPoint

data MaskPosition = MaskPosition
  { point :: MaskPoint
  , xShift :: Double
  , yShift :: Double
  , scale :: Double
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''MaskPosition

data Sticker = Sticker
  { setId :: LongNumber Int64
  , width :: Int32
  , height :: Int32
  , emoji :: Text
  , isMask :: Bool
  , maskPosition :: Maybe MaskPosition
  , thumbnail :: Maybe PhotoSize
  , sticker :: File
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Sticker

data Video = Video
  { duration :: Int32
  , width :: Int32
  , height :: Int32
  , fileName :: Text
  , mimeType :: Text
  , hasStickers :: Bool
  , supportsStreaming :: Bool
  , thumbnail :: Maybe PhotoSize
  , video :: File
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Video

data VideoNote = VideoNote
  { duration :: Int32
  , length :: Int32
  , thumbnail :: Maybe PhotoSize
  , video :: File
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''VideoNote

data VoiceNote = VoiceNote
  { duration :: Int32
  , waveform :: Text
  , mimeType :: Text
  , voice :: File
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''VoiceNote

data WebPage = WebPage
  { url :: Text
  , displayUrl :: Text
  , _type :: Text
  , siteName :: Text
  , title :: Text
  , description :: Text
  , photo :: Maybe Photo
  , embedUrl :: Text
  , embedType :: Text
  , embedWidth :: Int32
  , embedHeight :: Int32
  , duration :: Int32
  , author :: Text
  , animation :: Maybe Animation
  , audio :: Maybe Audio
  , document :: Maybe Document
  , sticker :: Maybe Sticker
  , video :: Maybe Video
  , videoNote :: Maybe VideoNote
  , voiceNote :: Maybe VoiceNote
  , hasInstantView :: Bool
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''WebPage

data Venue = Venue
  { location :: Location
  , title :: Text
  , address :: Text
  , provider :: Text
  , id :: Text
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Venue

data MessageContent
  = MessageAnimation { animation :: Animation
                     , caption :: FormattedText
                     , isSecret :: Bool }
  | MessageAudio { audio :: Audio
                 , caption :: FormattedText }
  | MessageBasicGroupChatCreate { title :: Text
                                , memberUserIds :: Vector Int32 }
  | MessageCall { discardReason :: CallDiscardReason
                , duration :: Int32 }
  | MessageChatAddMembers { memberUserIds :: Vector Int32 }
  | MessageChatChangePhoto { photo :: Photo }
  | MessageChatChangeTitle { title :: Text }
  | MessageChatDeleteMember { userId :: Int32 }
  | MessageChatDeletePhoto
  | MessageChatJoinByLink
  | MessageChatSetTtl { ttl :: Int32 }
  | MessageChatUpgradeFrom { title :: Text
                           , basicGroupId :: Int32 }
  | MessageChatUpgradeTo { supergroupId :: Int32 }
  | MessageContact { contact :: Contact }
  | MessageContactRegistered
  | MessageCustomServiceAction { text :: Text }
  | MessageDocument { document :: Document
                    , caption :: FormattedText }
  | MessageExpiredPhoto
  | MessageExpiredVideo
  | MessageGame { game :: Game }
  | MessageGameScore { gameMessageId :: LongNumber Int64
                     , gameId :: LongNumber Int64
                     , score :: Int32 }
  | MessageInvoice { title :: Text
                   , description :: Text
                   , _photo :: Maybe Photo
                   , currency :: Text
                   , totalAmount :: LongNumber Int64
                   , startParameter :: Text
                   , isTest :: Bool
                   , needShippingAddress :: Bool
                   , receiptMessageId :: LongNumber Int64 }
  | MessageLocation { location :: Location
                    , livePeriod :: Int32
                    , expiresIn :: Int32 }
  | MessagePaymentSuccessful { invoiceMessageId :: LongNumber Int64
                             , currency :: Text
                             , totalAmount :: LongNumber Int64 }
  | MessagePaymentSuccessfulBot { invoiceMessageId :: LongNumber Int64
                                , currency :: Text
                                , totalAmount :: LongNumber Int64
                                , invoicePayload :: Text
                                , shippingOptionId :: Text
                                , orderInfo :: Maybe OrderInfo
                                , telegramPaymentChargeId :: Text
                                , providerPaymentChargeId :: Text }
  | MessagePhoto { photo :: Photo
                 , caption :: FormattedText
                 , isSecret :: Bool }
  | MessagePinMessage { messageId :: LongNumber Int64 }
  | MessageScreenshotTaken
  | MessageSticker { sticker :: Sticker }
  | MessageSupergroupChatCreate { title :: Text }
  | MessageText { _text :: FormattedText
                , webPage :: Maybe WebPage }
  | MessageUnsupported
  | MessageVenue { venue :: Venue }
  | MessageVideo { video :: Video
                 , caption :: FormattedText
                 , isSecret :: Bool }
  | MessageVideoNote { videoNote :: VideoNote
                     , isViewed :: Bool
                     , isSecret :: Bool }
  | MessageVoiceNote { voiceNote :: VoiceNote
                     , caption :: FormattedText
                     , isListened :: Bool }
  | MessageWebsiteConnected { domainName :: Text }
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''MessageContent

data InlineKeyboardButtonType
  = InlineKeyboardButtonTypeBuy
  | InlineKeyboardButtonTypeCallback { _data :: Text }
  | InlineKeyboardButtonTypeCallbackGame
  | InlineKeyboardButtonTypeSwitchInline { query :: Text
                                         , inCurrentChat :: Bool }
  | InlineKeyboardButtonTypeUrl { url :: Text }
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''InlineKeyboardButtonType

data InlineKeyboardButton = InlineKeyboardButton
  { text :: Text
  , _type :: InlineKeyboardButtonType
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''InlineKeyboardButton

data KeyboardButtonType
  = KeyboardButtonTypeRequestLocation
  | KeyboardButtonTypeRequestPhoneNumber
  | KeyboardButtonTypeText
  deriving (Eq, Show, Ord, Enum, Bounded)

deriveJSON AOpt.namedCtors ''KeyboardButtonType

data KeyboardButton = KeyboardButton
  { text :: Text
  , _type :: KeyboardButtonType
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''KeyboardButton

data ReplyMarkup
  = ReplyMarkupForceReply { isPersonal :: Bool }
  | ReplyMarkupInlineKeyboard { rows :: Vector (Vector InlineKeyboardButton) }
  | ReplyMarkupRemoveKeyboard { isPersonal :: Bool }
  | ReplyMarkupShowKeyboard { _rows :: Vector (Vector KeyboardButton)
                            , resizeKeyboard :: Bool
                            , oneTime :: Bool
                            , isPersonal :: Bool }
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ReplyMarkup

data Message = Message
  { id :: LongNumber Int64
  , senderUserId :: Int32
  , chatId :: LongNumber Int64
  , sendingState :: Maybe MessageSendingState
  , isOutgoing :: Bool
  , canBeEdited :: Bool
  , canBeForwarded :: Bool
  , canBeDeletedOnlyForSelf :: Bool
  , canBeDeletedForAllUsers :: Bool
  , isChannelPost :: Bool
  , containsUnreadMention :: Bool
  , date :: Int32
  , editDate :: Int32
  , forwardInfo :: Maybe MessageForwardInfo
  , replyToMessageId :: LongNumber Int64
  , ttl :: Int32
  , ttlExpiresIn :: Double
  , viaBotUserId :: Int32
  , authorSignature :: Maybe Text
  , views :: Int32
  , mediaAlbumId :: Maybe (LongNumber Int64)
  , content :: MessageContent
  , replyMarkup :: Maybe ReplyMarkup
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Message

data NotificationSettings = NotificationSettings
  { muteFor :: Int32
  , sound :: Maybe Text
  , showPreview :: Bool
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''NotificationSettings

data InputFile
  = InputFileGenerated { originalPath :: Text
                       , conversion :: Text
                       , expectedSize :: Int32 }
  | InputFileId { id :: Int32 }
  | InputFileLocal { path :: Text }
  | InputFileRemote { _id :: Text }
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''InputFile

data InputThumbnail = InputThumbnail
  { thumbnail :: InputFile
  , width :: Int32
  , height :: Int32
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''InputThumbnail

data LabeledPricePart = LabeledPricePart
  { label :: Text
  , amount :: LongNumber Int64
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''LabeledPricePart

data Invoice = Invoice
  { currency :: Text
  , priceParts :: Vector LabeledPricePart
  , isTest :: Bool
  , needName :: Bool
  , needPhoneNumber :: Bool
  , needEmailAddress :: Bool
  , needShippingAddress :: Bool
  , sedPhoneNumberToProvider :: Bool
  , sendEmailAddressToProvider :: Bool
  , isFlexible :: Bool
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Invoice

data InputMessageContent
  = InputMessageAnimation { animation :: InputFile
                          , _thumbnail :: Maybe InputThumbnail
                          , duration :: Int32
                          , width :: Int32
                          , height :: Int32
                          , caption :: FormattedText }
  | InputMessageAudio { audio :: InputFile
                      , albumCoverThumbnail :: Maybe InputThumbnail
                      , duration :: Int32
                      , title :: Text
                      , performer :: Text
                      , caption :: FormattedText }
  | InputMessageContact { contact :: Contact }
  | InputMessageDocument { document :: InputFile
                         , _thumbnail :: Maybe InputThumbnail
                         , caption :: FormattedText }
  | InputMessageForwarded { fromChatId :: LongNumber Int64
                          , messageId :: LongNumber Int64
                          , inGameShare :: Bool }
  | InputMessageGame { botUserId :: Int32
                     , gameShortName :: Text }
  | InputMessageInvoice { invoice :: Invoice
                        , title :: Text
                        , description :: Text
                        , photoUrl :: Text
                        , photoSize :: Int32
                        , photoWidth :: Int32
                        , photoHeight :: Int32
                        , payload :: Text
                        , providerToken :: Text
                        , providerData :: Text
                        , startParameter :: Text }
  | InputMessageLocation { location :: Location
                         , livePeriod :: Int32 }
  | InputMessagePhoto { photo :: InputFile
                      , thumbnail :: InputThumbnail
                      , addedStickerFileIds :: Vector Int32
                      , width :: Int32
                      , height :: Int32
                      , caption :: FormattedText
                      , ttl :: Int32 }
  | InputMessageSticker { sticker :: InputFile
                        , _thumbnail :: Maybe InputThumbnail
                        , width :: Int32
                        , height :: Int32 }
  | InputMessageText { text :: FormattedText
                     , disableWebPagePreview :: Bool
                     , clearDraft :: Bool }
  | InputMessageVenue { venue :: Venue }
  | InputMessageVideo { video :: InputFile
                      , _thumbnail :: Maybe InputThumbnail
                      , addedStickerFileIds :: Vector Int32
                      , duration :: Int32
                      , width :: Int32
                      , height :: Int32
                      , supportsStreaming :: Bool
                      , caption :: FormattedText
                      , ttl :: Int32 }
  | InputMessageVideoNote { videoNote :: InputFile
                          , _thumbnail :: Maybe InputThumbnail
                          , duration :: Int32
                          , length :: Int32 }
  | InputMessageVoiceNote { voiceNote :: InputFile
                          , duration :: Int32
                          , waveform :: Text
                          , caption :: FormattedText }
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''InputMessageContent

data DraftMessage = DraftMessage
  { replyToMessageId :: LongNumber Int64
  , inputMessageText :: InputMessageContent
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''DraftMessage

data Chat = Chat
  { id :: LongNumber Int64
  , _type :: ChatType
  , title :: Text
  , photo :: Maybe ChatPhoto
  , lastMessage :: Maybe Message
  , order :: LongNumber Int64
  , isPinned :: Bool
  , canBeReported :: Bool
  , unreadCount :: Int32
  , lastReadInboxMessageId :: LongNumber Int64
  , lastReadOutboxMessageId :: LongNumber Int64
  , unreadMentionCount :: Int32
  , notificationSettings :: NotificationSettings
  , replyMarkupMessageId :: LongNumber Int64
  , draftMessage :: Maybe DraftMessage
  , clientData :: Text
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Chat

data ChatAction
  = ChatActionCancel
  | ChatActionChoosingContact
  | ChatActionChoosingLocation
  | ChatActionRecordingVideo
  | ChatActionRecordingVideoNote
  | ChatActionRecordingVoiceNote
  | ChatActionStartPlayingGame
  | ChatActionTyping
  | ChatActionUploadingDocument
  | ChatActionUploadingPhoto
  | ChatActionUploadingVideo
  | ChatActionUploadingVideoNote
  | ChatActionUploadingVoiceNote
  deriving (Eq, Show, Ord, Enum, Bounded)

deriveJSON AOpt.namedCtors ''ChatAction

data ChatEventAction
  = ChatEventDescriptionChanged { oldDescription :: Text
                                , newDescription :: Text }
  | ChatEventInvitesToggled { anyoneCanInvite :: Bool }
  | ChatEventIsAllHistoryAvailableToggled { isAllHistoryAvailable :: Bool }
  | ChatEventMemberInvited { userId :: Int32
                           , status :: ChatMemberStatus }
  | ChatEventMemberJoined
  | ChatEventMemberLeft
  | ChatEventMemberPromoted { userId :: Int32
                            , oldStatus :: ChatMemberStatus
                            , newStatus :: ChatMemberStatus }
  | ChatEventMemberRestricted { userId :: Int32
                              , oldStatus :: ChatMemberStatus
                              , newStatus :: ChatMemberStatus }
  | ChatEventMessageDeleted { message :: Message }
  | ChatEventMessageEdited { oldMessage :: Message
                           , newMessage :: Message }
  | ChatEventMessagePinned { message :: Message }
  | ChatEventMessageUnpinned
  | ChatEventPhotoChanged { oldPhoto :: Maybe ChatPhoto
                          , newPhoto :: Maybe ChatPhoto }
  | ChatEventSignMessagesToggled { signMessages :: Bool }
  | ChatEventStickerSetChanged { oldStickerSetId :: LongNumber Int64
                               , newStickerSetId :: LongNumber Int64 }
  | ChatEventTitleChanged { oldTitle :: Text
                          , newTitle :: Text }
  | ChatEventUsernameChanged { oldUsername :: Text
                             , newUsername :: Text }
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ChatEventAction

data ChatEvent = ChatEvent
  { id :: LongNumber Int64
  , date :: Int32
  , userId :: Int32
  , action :: ChatEventAction
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ChatEvent

data ChatEventLogFilters = ChatEventLogFilters
  { messageEdits :: Bool
  , messageDeletions :: Bool
  , messagePins :: Bool
  , memberJoins :: Bool
  , memberLeaves :: Bool
  , memberInvites :: Bool
  , memberPromotions :: Bool
  , memberRestrictions :: Bool
  , infoChanges :: Bool
  , settingChanges :: Bool
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ChatEventLogFilters

newtype ChatEvents = ChatEvents
  { events :: Vector ChatEvent
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ChatEvents

newtype ChatInviteLink = ChatInviteLink
  { inviteLink :: Text
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ChatInviteLink

data ChatInviteLinkInfo = ChatInviteLinkInfo
  { chatId :: LongNumber Int64
  , _type :: ChatType
  , photo :: Maybe ChatPhoto
  , memberCount :: Int32
  , memberUserIds :: Vector Int32
  , isPublic :: Bool
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ChatInviteLinkInfo

data ChatMembers = ChatMembers
  { totalCount :: Int32
  , members :: Vector ChatMember
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ChatMembers

data ChatReportReason
  = ChatReportReasonCustom { text :: Text }
  | ChatReportReasonPornography
  | ChatReportReasonSpam
  | ChatReportReasonViolence
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ChatReportReason

newtype ChatReportSpamState = ChatReportSpamState
  { canReportSpam :: Bool
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ChatReportSpamState

newtype Chats = Chats
  { chatIds :: Vector (LongNumber Int64)
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Chats

data CheckChatUsernameResult
  = CheckChatUsernameResultOk
  | CheckChatUsernameResultPublicChatsTooMuch
  | CheckChatUsernameResultPublicGroupsUnavailable
  | CheckChatUsernameResultUsernameInvalid
  | CheckChatUsernameResultUsernameOccupied
  deriving (Eq, Show, Ord, Bounded, Enum)

deriveJSON AOpt.namedCtors ''CheckChatUsernameResult

data ConnectedWebsite = ConnectedWebsite
  { id :: LongNumber Int64
  , domainName :: Text
  , botUserId :: Int32
  , browser :: Text
  , platform :: Text
  , logInDate :: Int32
  , lastActiveDate :: Int32
  , ip :: Text
  , location :: Text
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ConnectedWebsite

newtype ConnectedWebsites = ConnectedWebsites
  { websites :: Vector ConnectedWebsite
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ConnectedWebsites

newtype Count = Count
  { count :: Int32
  } deriving (Eq, Show, Ord, Num, Real, Integral, Enum, Bounded)

deriveJSON AOpt.namedCtors ''Count

newtype CustomRequestResult = CustomRequestResult
  { result :: Text
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''CustomRequestResult

data DeviceToken
  = DeviceTokenApplePush { deviceToken :: Text
                         , isAppSandbox :: Bool }
  | DeviceTokenApplePushVoIP { deviceToken :: Text
                             , isAppSandbox :: Bool }
  | DeviceTokenBlackBerryPush { token :: Text }
  | DeviceTokenGoogleCloudmessaging { token :: Text }
  | DeviceTokenMicrosoftPush { channelUri :: Text }
  | DeviceTokenMicrosoftPushVoIP { channelUri :: Text }
  | DeviceTokenSimplePush { endpoint :: Text }
  | DeviceTokenTizenPush { regId :: Text }
  | DeviceTokenUbuntuPush { token :: Text }
  | DeviceTokenWebPush { endpoint :: Text
                       , p256dhBase64url :: Text
                       , authBase64url :: Text }
  | DeviceTokenWindowsPush { accessToken :: Text }
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''DeviceToken

data FileType
  = FileTypeAnimation
  | FileTypeAudio
  | FileTypeDocument
  | FileTypeNone
  | FileTypePhoto
  | FileTypeProfilePhoto
  | FileTypeSecret
  | FileTypeSecretThumbnail
  | FileTypeSticker
  | FileTypeThumbnail
  | FileTypeUnknown
  | FileTypeVideo
  | FileTypeVideoNote
  | FileTypeVoiceNote
  | FileTypeWallpaper
  deriving (Eq, Show, Ord, Enum, Bounded)

deriveJSON AOpt.namedCtors ''FileType

data FoundMessages = FoundMessages
  { messages :: Vector Message
  , nextFromSearchId :: LongNumber Int64
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''FoundMessages

data GameHighScore = GameHighScore
  { position :: Int32
  , userId :: Int32
  , score :: Int32
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''GameHighScore

newtype GameHighScores = GameHighScores
  { scores :: Vector GameHighScore
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''GameHighScores

newtype Hashtags = Hashtags
  { hashtags :: Vector Text
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Hashtags

data ImportedContacts = ImportedContacts
  { userIds :: Vector Int32
  , importerCount :: Vector Int32
  } deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''ImportedContacts

data Update
  = AuthorizationState { authorizationState :: AuthorizationState }
  | Option { name :: Text
           , value :: OptionValue }
  | ConnectionState { state :: ConnectionState }
  deriving (Eq, Show)

deriveJSON (AOpt.prefixedCtors "update") ''Update
makePrisms ''Update

data Object
  = UpdateObj Update
  | ParametersObj Parameters
  | OkObj Ok
  | ConnectionStateObj ConnectionState
  | OptionValueObj OptionValue
  | ErrorObj Error
  | AccountTtlObj AccountTtl
  | LocalFileObj LocalFile
  | RemoteFileObj RemoteFile
  | FileObj File
  | PhotoSizeObj PhotoSize
  | AnimationObj Animation
  | AnimationsObj Animations
  | AudioObj Audio
  | AuthenticationCodeTypeObj AuthenticationCodeType
  | AuthenticationCodeInfoObj AuthenticationCodeInfo
  | AuthorizationStateObj AuthorizationState
  | ChatMemberStatusObj ChatMemberStatus
  | BasicGroupObj BasicGroup
  | BotCommandObj BotCommand
  | BotInfoObj BotInfo
  | ChatMemberObj ChatMember
  | BasicGroupFullInfoObj BasicGroupFullInfo
  | CallDiscardReasonObj CallDiscardReason
  | CallProtocolObj CallProtocol
  | CallConnectionObj CallConnection
  | CallStateObj CallState
  | CallObj Call
  | CallbackQueryAnswerObj CallbackQueryAnswer
  | CallbackQueryPayloadObj CallbackQueryPayload
  | CallIdObj CallId
  | ChatTypeObj ChatType
  | ChatPhotoObj ChatPhoto
  | MessageSendingStateObj MessageSendingState
  | MessageForwardInfoObj MessageForwardInfo
  | TextEntityTypeObj TextEntityType
  | TextEntityObj TextEntity
  | FormattedTextObj FormattedText
  | PhotoObj Photo
  | ContactObj Contact
  | DocumentObj Document
  | GameObj Game
  | LocationObj Location
  | ShippingAddressObj ShippingAddress
  | OrderInfoObj OrderInfo
  | MaskPointObj MaskPoint
  | MaskPositionObj MaskPosition
  | StickerObj Sticker
  | VideoObj Video
  | VideoNoteObj VideoNote
  | VoiceNoteObj VoiceNote
  | WebPageObj WebPage
  | VenueObj Venue
  | MessageContentObj MessageContent
  | InlineKeyboardButtonTypeObj InlineKeyboardButtonType
  | InlineKeyboardButtonObj InlineKeyboardButton
  | KeyboardButtonTypeObj KeyboardButtonType
  | KeyboardButtonObj KeyboardButton
  | ReplyMarkupObj ReplyMarkup
  | MessageObj Message
  | NotificationSettingsObj NotificationSettings
  | InputFileObj InputFile
  | InputThumbnailObj InputThumbnail
  | LabeledPricePartObj LabeledPricePart
  | InvoiceObj Invoice
  | InputMessageContentObj InputMessageContent
  | DraftMessageObj DraftMessage
  | ChatObj Chat
  | ChatActionObj ChatAction
  | ChatEventActionObj ChatEventAction
  | ChatEventObj ChatEvent
  | ChatEventLogFiltersObj ChatEventLogFilters
  | ChatEventsObj ChatEvents
  | ChatInviteLinkObj ChatInviteLink
  | ChatInviteLinkInfoObj ChatInviteLinkInfo
  | ChatMembersObj ChatMembers
  | ChatReportReasonObj ChatReportReason
  | ChatReportSpamStateObj ChatReportSpamState
  | ChatsObj Chats
  | CheckChatUsernameResultObj CheckChatUsernameResult
  | ConnectedWebsiteObj ConnectedWebsite
  | ConnectedWebsitesObj ConnectedWebsites
  | CountObj Count
  | CustomRequestResultObj CustomRequestResult
  | DeviceTokenObj DeviceToken
  | FileTypeObj FileType
  | FoundMessagesObj FoundMessages
  | GameHighScoreObj GameHighScore
  | GameHighScoresObj GameHighScores
  | HashtagsObj Hashtags
  | ImportedContactsObj ImportedContacts
  deriving (Eq, Show)

deriveJSON AOpt.anonymousCtors ''Object
makePrisms ''Object

data ResponseDecodingError = ResponseDecodingError
  { originalResponse :: ByteString
  , errorMessage :: String
  } deriving (Eq, Show)

instance Exception ResponseDecodingError

type Response = WithExtra Object

recv :: FromJSON a => TDLibClient -> Double -> IO (Maybe (Response a))
recv client timeout_ = do
  responseJSON <- recvJSON client timeout_
  for responseJSON $ \r ->
    case eitherDecodeStrict' r of
      Prelude.Left err ->
        throwIO $
        ResponseDecodingError {originalResponse = r, errorMessage = err}
      Right object -> pure object

data Function
  = SetTdlibParameters { parameters :: Parameters }
  | SetAuthenticationPhoneNumber { phoneNumber :: Text
                                 , allowFlashCall :: Bool
                                 , isCurrentPhoneNumber :: Bool }
  | CheckDatabaseEncryptionKey { encryptionKey :: Text }
  deriving (Eq, Show)

deriveJSON AOpt.namedCtors ''Function

type Request = WithExtra Function

send :: ToJSON a => TDLibClient -> Request a -> IO ()
send client request = sendJSON client (toStrict $ encode request)
