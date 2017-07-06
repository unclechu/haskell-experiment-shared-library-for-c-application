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
import "process" System.Process (ProcessHandle, createProcess, proc, waitForProcess)
import "filepath" System.FilePath ((</>), (<.>))

import "directory" System.Directory ( createDirectoryIfMissing
                                    , removeDirectoryRecursive
                                    , withCurrentDirectory
                                    -- , getPermissions
                                    -- , setPermissions
                                    -- , setOwnerExecutable
                                    )

import "base" Control.Monad (forM_)
import "lens" Control.Lens (_4, (^.), (&))

import "base" Data.List (find)
import "base" Data.Maybe (fromMaybe)
import "base" Data.Char (toLower)


srcDir, buildDir, distDir, buildAppDir, buildLibDir ∷ String
srcDir             = "src"
buildDir           = "build"
distDir            = "dist"
buildAppDir        = buildDir </> "app"
buildLibDir        = buildDir </> "shared-library"
buildLibUseTestDir = buildDir </> "lib-use-test"


main ∷ IO ()
main = fmap (\x → if length x ≡ 0 then ["build"] else x) getArgs
  >>= \(action : _) → fromMaybe (unknown action) $ snd <$> find (\x → action ≡ fst x) taskMap

  where unknown = die ∘ ("Unexpected action: " ⧺)

        taskMap = [ ("clean",      cleanTask)
                  , ("build",      cleanTask >> buildTask)
                  , ("just-build", buildTask)
                  , ("run",        cleanTask >> buildTask >> runAppTask >> runLibUseTestTask)
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

  createDirectoryIfMissing True buildLibDir

  forM_ ["Foo", "Bar"] $ \x →
    runGhc [ "--make", "-dynamic", "-shared", "-fPIC"
           , srcDir </> x <.> "hs"
           , "-o", distDir </> "lib" ⧺ map toLower x <.> "so"
           , "-i" ⧺ srcDir, "-outputdir", buildLibDir
           ]

  createDirectoryIfMissing True buildLibUseTestDir

  withCurrentDirectory buildLibUseTestDir $
    let up x = ".." </> ".." </> x
     in exec "gcc" $ [ "-O2"
                     , "-I/home/unclechu/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/include"
                     , "-L" ⧺ up distDir
                     ]

                     ⧺ map (\x → '-' : 'L' : x) lDirs ⧺

                     [ "-lHSrts-ghc8.0.2"
                     , "-lHSbase-4.9.1.0-ghc8.0.2"
                     , "-lHSghc-prim-0.5.0.0-ghc8.0.2"
                     , "-lHSinteger-gmp-1.0.0.1-ghc8.0.2"

                     , "-lfoo"
                     , up (srcDir </> "lib-use-test" <.> "c")
                     , "-o", up (distDir </> "lib-use-test")
                     , "-ldl"
                     ]

  -- distDir </> "lib-use-test" & \f →
  --   setOwnerExecutable True <$> getPermissions f >>= setPermissions f


runAppTask ∷ IO ()
runAppTask = do
  putStrLn $ "≡ Running 'app'… ≡"
  exec (distDir </> "app") []
  putStrLn $ "≡ End of 'app' ≡"


runLibUseTestTask ∷ IO ()
runLibUseTestTask = do
  putStrLn $ "≡ Running 'lib-use-test'… ≡"
  exec "env" [ "LD_LIBRARY_PATH=" ⧺ distDir ⧺ ":"
               ⧺ foldr (\x acc → x ⧺ (':' : acc)) "" lDirs
               ⧺ "/usr/local/lib64:/usr/lib64"

             , distDir </> "lib-use-test"
             ]
  putStrLn $ "≡ End of 'lib-use-test' ≡"


exec ∷ FilePath → [String] → IO ()
exec bin args = failCheck $ fmap (^. _4) $ createProcess $ proc bin args

runGhc ∷ [String] → IO ()
runGhc = failCheck ∘ fmap (^. _4) ∘ createProcess ∘ proc "stack" ∘ \x → "ghc" : "--" : x

failCheck ∷ IO ProcessHandle → IO ()
failCheck m = m >>= waitForProcess >>= \x → if x ≡ ExitSuccess then pure () else exitWith x

lDirs ∷ [String]
lDirs = [ "/home/unclechu/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/base-4.9.1.0"
        , "/home/unclechu/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/integer-gmp-1.0.0.1"
        , "/home/unclechu/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/ghc-prim-0.5.0.0"
        , "/home/unclechu/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/rts"
        ]
