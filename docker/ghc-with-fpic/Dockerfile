FROM debian:stretch

RUN apt-get update -y \
	&& apt-get install -y \
		locales wget curl git \
		build-essential cabal-install \
	&& sed -i -e \
		's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' \
		/etc/locale.gen \
	&& locale-gen

ENV LANG='en_US.UTF-8' LANGUAGE='en_US:en' LC_ALL='en_US.UTF-8'

# see https://stackoverflow.com/a/28131655/774228
# for info about customizations in 'build.mk'
COPY my-build.mk /my-build.mk

RUN mkdir /compile && cd /compile \
	&& wget https://downloads.haskell.org/~ghc/8.2.2/ghc-8.2.2-src.tar.xz \
	&& tar -xvf ghc-8.2.2-src.tar.xz \
	&& rm ghc-8.2.2-src.tar.xz \
	&& cd ghc-8.2.2/ \
	&& ./configure --prefix=/ghc-8.2.2-fpic \
		--disable-library-profiling \
		--enable-shared \
	&& cp /my-build.mk mk/build.mk \
	&& make install \
	&& rm -rf /compile /my-build.mk \
	&& cd /usr/local/bin \
	&& ls /ghc-8.2.2-fpic/bin/ | xargs -I{} ln -s /ghc-8.2.2-fpic/bin/{}
