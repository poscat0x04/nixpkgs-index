{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Path
  ( Path,
    PathType (..),
    --
    fromRel,
    fromAbs,
    parseRel,
    parseAbs,
    --
    (</>),
    stripPrefix,
    isPrefixOf,
    parent,
    filename,
    --
    mkRel,
    mkAbs,
    absp,
    relp,
    unsafeToPath,
  )
where

import Codec.Serialise
import Data.Aeson
import qualified Data.Aeson.Types as A
import Data.Foldable
import Data.Hashable
import Data.Maybe
import Data.Sequence
import Data.Text (Text, pack)
import Data.Void
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Megaparsec

type Parser = Parsec Void Text

data PathSeg
  = Parent
  | PathSeg {-# UNPACK #-} !Text
  deriving (Show, Eq, Lift, Generic)
  deriving anyclass (Serialise, Hashable)

fromPathSeg :: PathSeg -> Text
fromPathSeg Parent = ".."
fromPathSeg (PathSeg p) = p

pathSeg :: Parser (Maybe PathSeg)
pathSeg = try parentP <|> try dot <|> normalSeg
  where
    parentP = do
      _ <- single '.'
      _ <- single '.'
      pure (Just Parent)
    normalSeg = Just . PathSeg <$> takeWhile1P Nothing (/= '/')
    dot = do
      _ <- single '.'
      pure Nothing

pathSeg' :: Parser (Maybe PathSeg)
pathSeg' = pathSeg <|> pure Nothing

relpath :: Parser (Path 'Rel)
relpath = do
  h <- pathSeg
  t <- many $ single '/' *> pathSeg'
  pure $ Path $ fromList $ catMaybes $ h : t

abspath :: Parser (Path 'Abs)
abspath = do
  _ <- single '/'
  l <- sepBy pathSeg' $ single '/'
  pure $ Path $ fromList $ catMaybes l

data PathType
  = Abs
  | Rel
  deriving (Show, Eq)

-- | A canonicalized file path
newtype Path (t :: PathType) = Path
  { unPath :: Seq PathSeg
  }
  deriving stock (Generic, Lift)
  deriving newtype (Eq, Show)
  deriving anyclass (Serialise)

instance Hashable (Path t) where
  hashWithSalt salt (Path s) = hashWithSalt salt (toList s)

relParser :: Text -> A.Parser (Path 'Rel)
relParser t =
  case parseRel t of
    Nothing -> fail "illformed relative path"
    Just p -> pure p

instance FromJSON (Path 'Rel) where
  parseJSON = withText "relative path" relParser

instance FromJSONKey (Path 'Rel) where
  fromJSONKey = FromJSONKeyTextParser relParser

absParser :: Text -> A.Parser (Path 'Abs)
absParser t =
  case parseAbs t of
    Nothing -> fail "illformed absolute path"
    Just p -> pure p

instance FromJSON (Path 'Abs) where
  parseJSON = withText "absolute path" absParser

instance FromJSONKey (Path 'Abs) where
  fromJSONKey = FromJSONKeyTextParser absParser

fromRel :: Path 'Rel -> Text
fromRel (fmap fromPathSeg . unPath -> l)
  | Empty <- l = "."
  | x :<| xs <- l = foldl' (\p s -> p <> "/" <> s) x xs

fromAbs :: Path 'Abs -> Text
fromAbs (fmap fromPathSeg . unPath -> l)
  | Empty <- l = "/"
  | x :<| xs <- l = "/" <> foldl' (\p s -> p <> "/" <> s) x xs

parseRel :: Text -> Maybe (Path 'Rel)
parseRel = parseMaybe relpath

parseAbs :: Text -> Maybe (Path 'Abs)
parseAbs = parseMaybe abspath

mkAbs :: Text -> Q Exp
mkAbs = lift . fromMaybe (error "illformed absolute path") . parseAbs

mkRel :: Text -> Q Exp
mkRel = lift . fromMaybe (error "illformed relative path") . parseRel

qq :: (Text -> Q Exp) -> QuasiQuoter
qq quoteExp' =
  QuasiQuoter
    { quoteExp = quoteExp' . pack,
      quotePat = \_ ->
        fail "illegal QuasiQuote (allowed as expression only, used as a pattern)",
      quoteType = \_ ->
        fail "illegal QuasiQuote (allowed as expression only, used as a type)",
      quoteDec = \_ ->
        fail "illegal QuasiQuote (allowed as expression only, used as a declaration)"
    }

absp :: QuasiQuoter
absp = qq mkAbs

relp :: QuasiQuoter
relp = qq mkRel

(</>) :: Path t -> Path 'Rel -> Path t
(Path p1) </> (Path p2) = Path (p1 <> p2)

stripPrefix :: Path t -> Path t -> Maybe (Path 'Rel)
stripPrefix (Path p1) (Path p2) =
  Path <$> stripPrefix' p1 p2
  where
    stripPrefix' Empty p = pure p
    stripPrefix' _ Empty = Nothing
    stripPrefix' (x :<| xs) (y :<| ys)
      | x == y = stripPrefix' xs ys
      | otherwise = Nothing

isPrefixOf :: Path t -> Path t -> Bool
isPrefixOf p1 p2 = isJust $ stripPrefix p1 p2

parent :: Path t -> Path t
parent (Path Empty) = Path Empty
parent (Path (xs :|> _)) = Path xs

filename :: Path t -> Path 'Rel
filename (Path Empty) = Path Empty
filename (Path (Empty :|> x)) = Path $ pure x
filename (Path (_ :<| xs)) = filename $ Path xs

unsafeToPath :: [Text] -> Path t
unsafeToPath = Path . fromList . fmap PathSeg
