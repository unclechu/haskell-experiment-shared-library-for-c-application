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
{-# LANGUAGE ViewPatterns #-}

import "base-unicode-symbols" Prelude.Unicode
import "base" System.Exit (die, exitWith, ExitCode (ExitSuccess))
import "base" System.Environment (getArgs)
import "base" System.IO.Error (isDoesNotExistError, catchIOError, ioError)
import "process" System.Process (ProcessHandle, createProcess, proc, waitForProcess)
import "filepath" System.FilePath ((</>), (<.>))

import "directory" System.Directory ( createDirectoryIfMissing
                                    , removeDirectoryRecursive
                                    , removeFile
                                    )

import "base" Control.Monad (forM_, mapM_)
import "lens" Control.Lens (_4, (^.), (&))

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

          , ("help",         forM_ ("--no-deps" : map fst (taskMap o)) putStrLn)
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

  exec "stack" ["build", "--only-dependencies"]
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

  exec "stack" ["build", "--only-dependencies"]
  createDirectoryIfMissing True buildLibDir
  createDirectoryIfMissing True distDir

  forM_ ["Foo", "Bar"] $ \x →
    runGhc [ "--make", "-dynamic", "-shared", "-fPIC"
           , srcDir </> x <.> "hs"
           , "-o", distDir </> "lib" ⧺ map toLower x <.> "so"
           , "-i" ⧺ srcDir, "-outputdir", buildLibDir
           ]

  exec "gcc" $ [ "-O2"
               , "-I/home/unclechu/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/include"
               , "-L" ⧺ distDir
               ]

               ⧺ map (\x → '-' : 'L' : x) lDirs ⧺

               [ "-lHSrts-ghc8.0.2"
               , "-lHSbase-4.9.1.0-ghc8.0.2"
               , "-lHSghc-prim-0.5.0.0-ghc8.0.2"
               , "-lHSinteger-gmp-1.0.0.1-ghc8.0.2"

               , "-lfoo"
               , srcDir </> "lib-test" <.> "c"
               , "-o", distDir </> "lib-test"
               , "-ldl"
               ]


runAppTask ∷ IO ()
runAppTask = do
  putStrLn $ "≡ Running 'app'… ≡"
  exec (distDir </> "app") []
  putStrLn $ "≡ End of 'app' ≡"


runLibTestTask ∷ IO ()
runLibTestTask = do

  putStrLn $ "≡ Running 'lib-test'… ≡"

  exec "env" [ "LD_LIBRARY_PATH=" ⧺ distDir ⧺ ":"
                 ⧺ foldr (\x acc → x ⧺ (':' : acc)) "" lDirs
                 ⧺ "/usr/local/lib64:/usr/lib64"

             , distDir </> "lib-test"
             ]

  putStrLn $ "≡ End of 'lib-test' ≡"


exec ∷ FilePath → [String] → IO ()
exec bin args = failCheck $ fmap (^. _4) $ createProcess $ proc bin args

runGhc ∷ [String] → IO ()
runGhc = failCheck ∘ fmap (^. _4) ∘ createProcess ∘ proc "stack" ∘ \x → "ghc" : "--" : x

failCheck ∷ IO ProcessHandle → IO ()
failCheck m = m >>= waitForProcess >>= \x → if x ≡ ExitSuccess then pure () else exitWith x

ignoreDoesNotExistsErr ∷ IO () → IO ()
ignoreDoesNotExistsErr = (`catchIOError` \e → if isDoesNotExistError e then pure () else ioError e)

lDirs ∷ [String]
lDirs = [ "/home/unclechu/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/base-4.9.1.0"
        , "/home/unclechu/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/integer-gmp-1.0.0.1"
        , "/home/unclechu/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/ghc-prim-0.5.0.0"
        , "/home/unclechu/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/rts"
        ]
