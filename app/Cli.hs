{-# LANGUAGE QuasiQuotes #-}

module Cli
  ( Command (..),
    parseCmd,
  )
where

import Data.String.Interpolate
import Data.Text (Text)
import Data.Version
import GHC.Generics
import Options.Applicative
import Paths_nixpkgs_index

data Command
  = Index
      { indexFile :: Maybe FilePath,
        xml :: FilePath
      }
  | Query
      { static :: Bool,
        pkgconfig :: Bool,
        indexFile :: Maybe FilePath,
        libname :: Text
      }
  deriving (Show, Eq, Generic)

parseCmd :: IO Command
parseCmd = execParser cliParser

cliParser :: ParserInfo Command
cliParser =
  info
    (command' <**> versionHelper <**> helper)
    ( fullDesc
        <> progDesc "nixpkgs-index"
        <> header "A nixpkgs indexer"
    )

command' :: Parser Command
command' =
  hsubparser
    ( foldMap
        (\(cmd, parser, desc) -> command cmd (info (parser <**> versionHelper) (progDesc desc)))
        [ ( "index",
            index,
            "Index libraries contained in nixpkgs derivations and build a mapping between \
            \library names and derivations that contain them"
          ),
          ("query", query, "Query an existing index")
        ]
    )

index :: Parser Command
index =
  Index
    <$> optional indexOpt
    <*> xmlOpt
  where
    xmlOpt =
      strArgument
        ( metavar "XML INPUT"
            <> completer (bashCompleter "file")
        )

query :: Parser Command
query =
  Query
    <$> staticOpt
    <*> pkgconfigOpt
    <*> optional indexOpt
    <*> name
  where
    staticOpt =
      switch
        ( short 's'
            <> long "static"
            <> help "whether to query static libraries instead"
        )
    pkgconfigOpt =
      switch
        ( short 'p'
            <> long "pkgconfig"
            <> help "whether to query pkgconfig libraries instead"
        )
    name =
      strArgument
        ( metavar "LIB NAME"
        )

indexOpt :: Parser FilePath
indexOpt =
  strOption
    ( short 'i'
        <> long "index"
        <> metavar "INDEX FILE"
        <> help "The index file, defaults to ~/.cache/nixpkgs-index/library-index.cbor"
    )

versionHelper :: Parser (a -> a)
versionHelper =
  infoOption
    [i|nixpkgs-index version #{showVersion version}|]
    (long "version" <> help "print program version")
