#!/usr/bin/env perl
@pkg = grep {/packages\.conf\.d/ && /\/\.cabal-sandbox\//} `cabal sandbox hc-pkg list`;
die 'cannot get cabal sandbox packages dirs' if $? || scalar(@pkg) < 1;
system 'runhaskell', "--ghc-arg=-package-db=$pkg[0]", './make.hs', @ARGV;
