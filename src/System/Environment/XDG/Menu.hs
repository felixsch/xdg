module System.Environment.XDG.Menu
  (
  ) where


import Control.Applicative

import qualified Text.XML.Light as XML

import System.Environment.XDG.BaseDir


getUserMenuDir :: IO FilePath
getUserMenuDir
    = getUserMenuDir
