FROM frolvlad/alpine-ghc as build
# install build tools
# cache dependencies
# build
RUN apk add cabal wget zlib zlib-dev
RUN mkdir /cabal-build
RUN mkdir /cabal-bins
RUN cabal update
RUN cabal install --global req warp


ARG PKG=test
ARG EXE=test

ADD $PKG/$PKG.cabal .
RUN cabal build --builddir=/cabal-build --enable-static --enable-executable-static --only-dependencies $EXE
ADD $PKG/ .

RUN cabal install --builddir=/cabal-build --installdir=/cabal-bins --install-method=copy\
    --enable-static --enable-executable-static $EXE

# copy artifacts to a clean image
FROM alpine
RUN apk add libffi gmp zlib zlib-dev
ADD https://github.com/aws/aws-lambda-runtime-interface-emulator/releases/latest/download/aws-lambda-rie /usr/bin/aws-lambda-rie
RUN chmod 755 /usr/bin/aws-lambda-rie
COPY entry.sh /
RUN chmod 755 /entry.sh
COPY --from=build /cabal-bins /cabal-bins
ENTRYPOINT [ "/entry.sh" ]
