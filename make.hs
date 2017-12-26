#!/usr/bin/env stack
{- stack runghc
 --resolver=lts-10.0
 --install-ghc
 --package base-unicode-symbols
 --package directory
 --package filepath
 --package process
 --package lens
 --package containers
 --package qm-interpolated-string
 -}

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

import "base-unicode-symbols" Prelude.Unicode

import "base"     System.Exit (die, exitWith, ExitCode (ExitSuccess))
import "base"     System.Environment (getArgs, getEnvironment)
import "base"     System.IO.Error (isDoesNotExistError, catchIOError, ioError)
import "base"     System.IO (hGetLine, hGetContents)
import "filepath" System.FilePath (dropExtension, takeDirectory, (</>), (<.>))

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

import "base" Control.Arrow ((&&&))
import "base" Control.Monad (forM_, forM, mapM_, mapM, (>=>))
import "lens" Control.Lens (_4, (^.), (&), (<&>))

import "base"       Data.List (find, intercalate, isSuffixOf, isInfixOf)
import "base"       Data.Maybe (fromMaybe)
import "base"       Data.Char (toLower)
import "containers" Data.Map (fromList, toList, insert)

import "qm-interpolated-string" Text.InterpolatedString.QM (qm)


srcDir, buildDir, distDir, buildAppDir, buildLibDir ∷ String
srcDir      = "src"
buildDir    = "build"
distDir     = "dist"
buildAppDir = buildDir </> "app"
buildLibDir = buildDir </> "shared-library"


main ∷ IO ()
main = getArgs <&> (\case [] → ["build"] ; x → x)
         >>= \(action : opts) → maybe (unknown action) snd
                              $ find (fst • (≡ action)) (taskMap opts)

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
cleanTask = do
  forM_ [buildDir, distDir] $ ignoreDoesNotExistsErr ∘ removeDirectoryRecursive
  ignoreDoesNotExistsErr $ removeFile "a.out"

cleanAppTask = do
  ignoreDoesNotExistsErr $ removeDirectoryRecursive buildAppDir
  ignoreDoesNotExistsErr $ removeFile $ distDir </> "app"

cleanLibTask = do

  ignoreDoesNotExistsErr $ do
    removeFile "a.out"
    removeDirectoryRecursive buildLibDir

  mapM_ (ignoreDoesNotExistsErr ∘ removeFile ∘ (distDir </>))
        ["libfoo" <.> "so", "libbar" <.> "so", "lib-test"]


buildAppTask ∷ IO ()
buildAppTask = do

  -- exec $ proc "stack" ["build", "--only-dependencies"]
  createDirectoryIfMissing True buildAppDir
  createDirectoryIfMissing True distDir

  forM_ ["Foo", "Bar"] $ \x →
    runGhc [ "-c", "-O", srcDir </> x <.> "hs"
           , "--make", "-i" ⧺ srcDir, "-outputdir", buildAppDir
           , "-Wall", "-O2"
           ]

  runGhc $ let ob x = buildAppDir </> x <.> "o"
            in [ "--make", "-no-hs-main", "-optc-O2"
               , "-optc-O", srcDir </> "main" <.> "c", ob "Foo", ob "Bar"
               , "-outputdir", buildAppDir, "-o", distDir </> "app"
               ]


buildLibTask ∷ IO ()
buildLibTask = do

  paths ← getPaths
  let ghc = runGhc ∘ ([qm|-package-db={cabalSandboxPkgPath paths}|] :) ∘ ("-package=ghc" :)

  -- exec $ proc "stack" ["build", "--only-dependencies"]
  createDirectoryIfMissing True buildLibDir
  createDirectoryIfMissing True distDir

  forM_ ["Foo", "Bar"] $ \x → do

    ghc [ "-static", "-shared", "-fPIC"
        , "-optc-O2"
        , "-optc-DMODULE=" ⧺ x
        , srcDir </> "lib-autoinit" <.> "c"
        , "-outputdir", buildLibDir
        ]

    ghc [ "--make", "-static", "-shared", "-fPIC"
        -- , "-package", "ghc"
        , srcDir </> x <.> "hs"
        , buildLibDir </> srcDir </> "lib-autoinit" <.> "o"
        , "-o", distDir </> "lib" ⧺ map toLower x <.> "so"
        , "-i" ⧺ srcDir, "-outputdir", buildLibDir
        , "-Wall", "-O2"
        , "-optl-Wl,-s", "-funfolding-use-threshold=16", "-optc-O3", "-optc-ffast-math"
        ]

{-
  let libsFlags = let reducer (dir, link → l) acc = ("-L" ⧺ dir) : ("-l" ⧺ l) : acc
                      link = drop 3 ∘ dropExtension

                   in foldr reducer [] (packagesLibsPaths paths)

  forM_ [ ("lib-test",   ["-L" ⧺ distDir, "-lfoo"])
        , ("lib-test-2", ["-ldl"])
        ] $ \(file, args) →

    exec $ proc "gcc" $ [ "-O2", "-I" ⧺ ghcIncludePath paths
                        , srcDir </> file <.> "c"
                        , "-o", distDir </> file
                        ] ⧺ args ⧺ libsFlags
-}



runAppTask ∷ IO ()
runAppTask = do
  logRun True "app"
  exec $ proc (distDir </> "app") []
  logRun False "app"


runLibTestTask ∷ IO ()
runLibTestTask = do

  pure ()
  {-
  paths ← getPaths

  newEnv ← let ldDirs = intercalate ":" $ distDir : map fst (packagesLibsPaths paths)
            in insert "LD_LIBRARY_PATH" ldDirs ∘ fromList <$> getEnvironment

  forM_ ["lib-test", "lib-test-2"] $ \x → do
    logRun True x
    exec (proc (distDir </> x) []) { env = Just $ toList newEnv }
    logRun False x
  -}


exec ∷ CreateProcess → IO ()
exec = createProcess • fmap (^. _4) • failCheck

runGhc ∷ [String] → IO ()
runGhc = proc "ghc" • createProcess • fmap (^. _4) • failCheck

failCheck ∷ IO ProcessHandle → IO ()
failCheck = (>>= failProtect)

failProtect ∷ ProcessHandle → IO ()
failProtect = waitForProcess >=> \x → if x ≡ ExitSuccess then pure () else exitWith x

ignoreDoesNotExistsErr ∷ IO () → IO ()
ignoreDoesNotExistsErr = (`catchIOError` \e → if isDoesNotExistError e then pure () else ioError e)


getPaths ∷ IO Paths
getPaths = do

  {-
  ghcBin ← getOutput "stack" ["path", "--compiler-exe"]
  ghcVer ← getOutput "stack" ["exec", "ghc-pkg", "latest", "ghc"]

  let ghcDir = (takeDirectory ∘ takeDirectory) ghcBin </> "lib" </> ghcVer
      sfx = (⧺ '-' : filter (≢ '-') ghcVer)

  libs ← forM ["base", "integer-gmp", "ghc-prim"]
       $ (\x → ["exec", "ghc-pkg", "latest", x]) -- lastest version of package
       • getOutput "stack"
       • fmap (\x → (ghcDir </> x, sfx ("libHS" ⧺ x) <.> "so"))

  return Paths { ghcIncludePath    = ghcDir </> "include"
               , packagesLibsPaths = (ghcDir </> "rts", sfx "libHSrts" <.> "so") : libs
               }
  -}

  [pkg] ←
    getOutput "cabal" ["sandbox", "hc-pkg", "list"]
       <&> filter ((isSuffixOf "packages.conf.d" &&& isInfixOf "/.cabal-sandbox/") • uncurry (&&))

  pure Paths { cabalSandboxPkgPath = pkg }

  where getOutput ∷ FilePath → [String] → IO [String]
        getOutput bin args = do
          (_, Just hOut, _, hProc) ← createProcess (proc bin args) { std_out = CreatePipe }
          failProtect hProc
          hGetContents hOut <&> lines <&> \x → if null (last x) then init x else x

data Paths
  = Paths
  -- { ghcIncludePath    ∷ String
  -- , packagesLibsPaths ∷ [(String, String)]
  { cabalSandboxPkgPath ∷ String
  } deriving (Show, Eq)


logRun ∷ Bool → String → IO ()
logRun isStart appName =
  putStrLn
    [qm| {if isStart then lb else ""}
         ≡ {if isStart then "Running" else "End of"} '{appName}'{if isStart then "…" else ""} ≡
         {if isStart then "" else lb} |]
  where lb = "\n"

(•) ∷ (a → b) → (b → c) → (a → c) ; (•) = flip (∘) ; infixl 9 • ; {-# INLINE (•) #-}
