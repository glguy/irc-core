FROM haskell:8.6
RUN apt-get update \
    && apt-get install -y libssl-dev \
    && rm -rf /var/lib/apt/lists/* \
    && cabal v2-update

WORKDIR /opt/glirc
ADD dist-newstyle/sdist/glirc-2.31.tar.gz ./
ADD dist-newstyle/sdist/irc-core-2.7.0.tar.gz ./
ADD dist-newstyle/sdist/irc-core-bot-0.1.0.0.tar.gz ./
ADD dist-newstyle/sdist/hookup-0.2.3.tar.gz ./
ADD dist-newstyle/sdist/glirc-2.31.tar.gz ./
ADD dist-newstyle/sdist/config-schema-0.5.0.1.tar.gz ./
ADD dist-newstyle/sdist/config-value-0.6.3.1.tar.gz ./
RUN cd glirc-2.31 && cabal v2-build --only-dependencies
RUN cd glirc-2.31 && cabal v2-build
