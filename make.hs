#!/usr/bin/env stack
{- stack runghc
 --resolver=lts-8.21
 --package base-unicode-symbols
 --package directory
 --package filepath
 --package process
 --package lens
 --package containers
 -}

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

import "base-unicode-symbols" Prelude.Unicode

import "base"     System.Exit (die, exitWith, ExitCode (ExitSuccess))
import "base"     System.Environment (getArgs, getEnvironment)
import "base"     System.IO.Error (isDoesNotExistError, catchIOError, ioError)
import "base"     System.IO (hGetLine)
import "filepath" System.FilePath (dropExtension, (</>), (<.>))

import "process" System.Process ( StdStream (CreatePipe)
                                , CreateProcess (env, std_out)
                                , ProcessHandle
                                , createProcess
                                , proc
                                , waitForProcess
                                )

import "directory" System.Directory ( createDirectoryIfMissing
                                    , removeDirectoryRecursive
                                    , removeFile
                                    )

import "base" Control.Monad (forM_, forM, mapM_, mapM, (>=>))
import "lens" Control.Lens (_4, (^.), (&))

import "base"       Data.List (find, intercalate)
import "base"       Data.Maybe (fromMaybe)
import "base"       Data.Char (toLower)
import "containers" Data.Map (fromList, toList, insert)


srcDir, buildDir, distDir, buildAppDir, buildLibDir ∷ String
srcDir      = "src"
buildDir    = "build"
distDir     = "dist"
buildAppDir = buildDir </> "app"
buildLibDir = buildDir </> "shared-library"


main ∷ IO ()
main = fmap (\x → if length x ≡ 0 then ["build"] else x) getArgs
  >>= \(action : opts) → fromMaybe (unknown action) $
          snd <$> find (\x → action ≡ fst x) (taskMap opts)

  where withDeps opts deps task = if "--no-deps" ∉ opts then deps >> task else task
        unknown = die ∘ ("Unexpected action: " ⧺)

        taskMap ∷ [String] → [(String, IO ())]
        taskMap o@(withDeps → t) =

          [ ("clean",        cleanTask)
          , ("clean-app",    cleanAppTask)
          , ("clean-lib",    cleanLibTask)

          , ("build",        t cleanTask (buildAppTask >> buildLibTask))
          , ("build-app",    t cleanAppTask buildAppTask)
          , ("build-lib",    t cleanLibTask buildLibTask)

          , ("run",          t (cleanTask >> buildAppTask >> buildLibTask)
                               (runAppTask >> runLibTestTask))

          , ("run-app",      t (cleanAppTask >> buildAppTask) runAppTask)
          , ("run-lib-test", t (cleanLibTask >> buildLibTask) runLibTestTask)

          , ("help",         mapM_ putStrLn $ "--no-deps" : map fst (taskMap o))
          ]

cleanTask, cleanAppTask, cleanLibTask ∷ IO ()
cleanTask = forM_ [buildDir, distDir] $ ignoreDoesNotExistsErr ∘ removeDirectoryRecursive

cleanAppTask = do
  ignoreDoesNotExistsErr $ removeDirectoryRecursive buildAppDir
  ignoreDoesNotExistsErr $ removeFile $ distDir </> "app"

cleanLibTask = do

  ignoreDoesNotExistsErr $ removeDirectoryRecursive buildLibDir

  ["libfoo" <.> "so", "libbar" <.> "so", "lib-test"]
    & mapM_ (ignoreDoesNotExistsErr ∘ removeFile ∘ (distDir </>))


buildAppTask ∷ IO ()
buildAppTask = do

  exec $ proc "stack" ["build", "--only-dependencies"]
  createDirectoryIfMissing True buildAppDir
  createDirectoryIfMissing True distDir

  forM_ ["Foo", "Bar"] $ \x →
    runGhc [ "-c", "-O", srcDir </> x <.> "hs"
           , "--make", "-i" ⧺ srcDir, "-outputdir", buildAppDir
           , "-Wall", "-O2", "-optc-O2"
           ]

  runGhc $ let ob x = buildAppDir </> x <.> "o"
            in [ "--make", "-no-hs-main", "-optc-O2"
               , "-optc-O", srcDir </> "main" <.> "c", ob "Foo", ob "Bar"
               , "-outputdir", buildAppDir, "-o", distDir </> "app"
               ]


buildLibTask ∷ IO ()
buildLibTask = do

  paths ← getPaths
  exec $ proc "stack" ["build", "--only-dependencies"]
  createDirectoryIfMissing True buildLibDir
  createDirectoryIfMissing True distDir

  forM_ ["Foo", "Bar"] $ \x →
    runGhc [ "--make", "-dynamic", "-shared", "-fPIC"
           , srcDir </> x <.> "hs"
           , "-o", distDir </> "lib" ⧺ map toLower x <.> "so"
           , "-i" ⧺ srcDir, "-outputdir", buildLibDir
           ]

  exec $ proc "gcc" $ ["-O2", "-I" ⧺ ghcIncludePath paths, "-L" ⧺ distDir, "-lfoo"{-, "-ldl"-}]
                    ⧺ [srcDir </> "lib-test" <.> "c", "-o", distDir </> "lib-test"]

                    ⧺ let reducer (dir, (link → l)) acc = ("-L" ⧺ dir) : ("-l" ⧺ l) : acc
                          link = drop 3 ∘ dropExtension

                       in foldr reducer [] (packagesLibsPaths paths)



runAppTask ∷ IO ()
runAppTask = do
  putStrLn "≡ Running 'app'… ≡"
  exec $ proc (distDir </> "app") []
  putStrLn "≡ End of 'app' ≡"


runLibTestTask ∷ IO ()
runLibTestTask = do

  paths ← getPaths
  putStrLn "≡ Running 'lib-test'… ≡"

  newEnv ← let ldDirs = intercalate ":" $ distDir : map fst (packagesLibsPaths paths)
            in insert "LD_LIBRARY_PATH" ldDirs ∘ fromList <$> getEnvironment

  exec (proc (distDir </> "lib-test") []) { env = Just $ toList newEnv }
  putStrLn "≡ End of 'lib-test' ≡"


exec ∷ CreateProcess → IO ()
exec = failCheck ∘ fmap (^. _4) ∘ createProcess

runGhc ∷ [String] → IO ()
runGhc = failCheck ∘ fmap (^. _4) ∘ createProcess ∘ proc "stack" ∘ \x → "ghc" : "--" : x

failCheck ∷ IO ProcessHandle → IO ()
failCheck = (>>= failProtect)

failProtect ∷ ProcessHandle → IO ()
failProtect = waitForProcess >=> \x → if x ≡ ExitSuccess then pure () else exitWith x

ignoreDoesNotExistsErr ∷ IO () → IO ()
ignoreDoesNotExistsErr = (`catchIOError` \e → if isDoesNotExistError e then pure () else ioError e)


getPaths ∷ IO Paths
getPaths = do

  programs ← getOutput "stack" ["path", "--programs"]
  ghcVer   ← getOutput "stack" ["exec", "ghc-pkg", "latest", "ghc"]

  let ghcDir = programs </> ghcVer </> "lib" </> ghcVer
      sfx = (⧺ '-' : filter (≢ '-') ghcVer)

  libs ← forM ["base", "integer-gmp", "ghc-prim"] $
    fmap (\x → (ghcDir </> x, sfx ("libHS" ⧺ x) <.> "so")) ∘
      getOutput "stack" ∘ (\x → ["exec", "ghc-pkg", "latest", x])

  return Paths { ghcIncludePath    = ghcDir </> "include"
               , packagesLibsPaths = (ghcDir </> "rts", sfx "libHSrts" <.> "so") : libs
               }

  where getOutput ∷ FilePath → [String] → IO String
        getOutput bin args = do
          (_, Just hOut, _, hProc) ← createProcess (proc bin args) { std_out = CreatePipe }
          failProtect hProc
          hGetLine hOut

data Paths = Paths { ghcIncludePath    ∷ String
                   , packagesLibsPaths ∷ [(String, String)]
                   }
                     deriving (Show, Eq)
