FROM haskell:9.8.1
RUN cabal update
RUN cabal install cabal-install
RUN apt-get update
RUN apt-get install vim -y
WORKDIR /hscanner
COPY . .