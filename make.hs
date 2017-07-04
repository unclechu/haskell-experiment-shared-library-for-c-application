#!/usr/bin/env stack
{- stack runghc
 --resolver=lts-8.21
 --package base-unicode-symbols
 --package directory
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
import "directory" System.Directory (createDirectory, removeDirectoryRecursive)
import "process" System.Process (createProcess, proc, waitForProcess, ProcessHandle)

import "base" Control.Monad (forM_)
import "lens" Control.Lens (_4, (^.))


main ∷ IO ()
main = fmap (\x → if length x ≡ 0 then ["build"] else x) getArgs
  >>= \(action : _) → case action of
                           "clean"      → cleanTask
                           "build"      → cleanTask >> buildTask
                           "just-build" → buildTask
                           "run"        → cleanTask >> buildTask >> runTask
                           "help"       → forM_ ["clean", "build", "just-build", "run"] putStrLn
                           _            → die $ "Unexpected action: " ⧺ action


cleanTask ∷ IO ()
cleanTask = removeDirectoryRecursive "build" `catchIOError`
              \e → if isDoesNotExistError e then pure () else ioError e


buildTask ∷ IO ()
buildTask = do

  exec "stack" ["build", "--only-dependencies"]
  createDirectory "build"

  forM_ ["Foo", "Bar"] $ \x →
    runGhc ["-c", "-O", "src/" ⧺ x ⧺ ".hs", "-outputdir", "build", "-Wall", "-O2"]

  runGhc [ "--make", "-no-hs-main", "-optc-O", "src/main.c"
         , "build/Foo.o", "build/Bar.o"
         , "-outputdir", "build", "-o", "build/main"
         ]


-- TODO shared library
-- ghc --make -dynamic -shared -fPIC Foo.hs -o libfoo.so


runTask ∷ IO ()
runTask = exec "build/main" []

exec ∷ FilePath → [String] → IO ()
exec bin args = failCheck $ fmap (^. _4) $ createProcess $ proc bin args

runGhc ∷ [String] → IO ()
runGhc = failCheck ∘ fmap (^. _4) ∘ createProcess ∘ proc "stack" ∘ \x → "ghc" : "--" : x

failCheck ∷ IO ProcessHandle → IO ()
failCheck m = m >>= waitForProcess >>= \x → if x ≡ ExitSuccess then pure () else exitWith x
