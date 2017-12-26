# How to

```bash
docker build -t ghc-with-fpic .
```

## Run docker container

SELinux policy might require to do this first in project root:
```bash
chcon -Rt svirt_sandbox_file_t .
```

Go inside container
```bash
docker run -it --volume="`pwd`:/mnt" --name=some-name ghc-with-fpic
```

### Inside docker container

```bash
cd /mnt
cabal update
cabal sandbox init
cabal install --ghc-option=-fPIC happy alex
cabal install --ghc-option=-fPIC base-unicode-symbols filepath process directory lens containers qm-interpolated-string
```

Build libs (`make.pl` is just a wrapper for `make.hs` to start with package database from
cabal-sandbox):
```bash
./make.pl build-lib
```

## Warning

Bulding this container of **GHC-8.2.2** with `-fPIC` flag may take few ours and requires
between *3–3.5GiB* of disk memory.
