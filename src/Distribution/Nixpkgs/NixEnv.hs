module Distribution.Nixpkgs.NixEnv where

import Data.ByteString (ByteString)
import Distribution.Nixpkgs.Packages
import Xeno.DOM

outputPaths :: Node -> Maybe [StorePath]
outputPaths node = do
  attr <- lookup "attrPath" $ attributes node
  let children' = children node
      buildStorePath node' = do
        let attrs = attributes node'
        output <- lookup "name" attrs
        let origin = PathOrigin {..}
        rawPath <- lookup "path" attrs
        parseStorePath origin rawPath
  traverse buildStorePath children'

allStorePaths :: Node -> Maybe [StorePath]
allStorePaths node = fmap concat $ traverse outputPaths $ children node

parseNixEnvOutput :: ByteString -> Maybe [StorePath]
parseNixEnvOutput bs =
  case parse bs of
    Left _ -> Nothing
    Right node -> allStorePaths node
