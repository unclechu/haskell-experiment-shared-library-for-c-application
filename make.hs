#!/usr/bin/env stack
{- stack runghc
 --resolver=lts-8.21
 --package base-unicode-symbols
 --package directory
 --package filepath
 --package process
 --package lens
 -}

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

import "base-unicode-symbols" Prelude.Unicode
import "base" System.Exit (die, exitWith, ExitCode (ExitSuccess))
import "base" System.Environment (getArgs)
import "base" System.IO.Error (isDoesNotExistError, catchIOError, ioError)
import "directory" System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import "process" System.Process (createProcess, proc, waitForProcess, ProcessHandle)
import "filepath" System.FilePath ((</>), (<.>))

import "base" Control.Monad (forM_)
import "lens" Control.Lens (_4, (^.))

import "base" Data.List (find)
import "base" Data.Maybe (fromMaybe)
import "base" Data.Char (toLower)


srcDir, buildDir, distDir, buildAppDir, buildLibDir ∷ String
srcDir      = "src"
buildDir    = "build"
distDir     = "dist"
buildAppDir = buildDir </> "app"
buildLibDir = buildDir </> "shared-library"


main ∷ IO ()
main = fmap (\x → if length x ≡ 0 then ["build"] else x) getArgs
  >>= \(action : _) → fromMaybe (unknown action) $ snd <$> find (\x → action ≡ fst x) taskMap

  where unknown = die ∘ ("Unexpected action: " ⧺)

        taskMap = [ ("clean",      cleanTask)
                  , ("build",      cleanTask >> buildTask)
                  , ("just-build", buildTask)
                  , ("run",        cleanTask >> buildTask >> runTask)
                  , ("help",       forM_ taskMap $ putStrLn ∘ fst)
                  ]


cleanTask ∷ IO ()
cleanTask = forM_ [buildDir, distDir] $ \dir →
  removeDirectoryRecursive dir `catchIOError`
    \e → if isDoesNotExistError e then pure () else ioError e


buildTask ∷ IO ()
buildTask = do

  exec "stack" ["build", "--only-dependencies"]
  createDirectoryIfMissing True buildAppDir
  createDirectoryIfMissing True distDir

  forM_ ["Baz/Qux", "Foo", "Bar"] $ \x →
    runGhc [ "-c", "-O", srcDir </> x <.> "hs"
           , "-i" ⧺ buildAppDir, "-outputdir", buildAppDir
           , "-Wall", "-O2", "-optc-O2"
           ]

  runGhc $ let ob x = buildAppDir </> x <.> "o"
            in [ "--make", "-no-hs-main", "-optc-O2"
               , "-optc-O", srcDir </> "main" <.> "c", ob "Foo", ob "Bar"
               , "-outputdir", buildAppDir, "-o", distDir </> "app"
               ]

  createDirectoryIfMissing True buildLibDir

  forM_ ["Baz/Qux", "Foo", "Bar"] $ \x →
    runGhc [ "-c", "-O", srcDir </> x <.> "hs"
           , "-i" ⧺ buildLibDir, "-outputdir", buildLibDir
           , "-Wall", "-O2", "-dynamic", "-fPIC", "-optc-O2"
           ]

  forM_ ["Foo", "Bar"] $ \x →
    runGhc [ "--make", "-dynamic", "-shared", "-fPIC", "-optc-O2"
           , buildLibDir </> x <.> "o", "-outputdir", buildLibDir
           , "-o", distDir </> "lib" ⧺ map toLower x <.> "so"
           ]


runTask ∷ IO ()
runTask = exec (distDir </> "app") []

exec ∷ FilePath → [String] → IO ()
exec bin args = failCheck $ fmap (^. _4) $ createProcess $ proc bin args

runGhc ∷ [String] → IO ()
runGhc = failCheck ∘ fmap (^. _4) ∘ createProcess ∘ proc "stack" ∘ \x → "ghc" : "--" : x

failCheck ∷ IO ProcessHandle → IO ()
failCheck m = m >>= waitForProcess >>= \x → if x ≡ ExitSuccess then pure () else exitWith x
