FROM haskell:8.10

WORKDIR /opt/nove-caudas

RUN cabal update

COPY nove-caudas.cabal /opt/nove-caudas/nove-caudas.cabal

RUN cabal build --only-dependencies -j4

COPY . /opt/nove-caudas
RUN cabal install

CMD ["nove-caudas-bot"]
