FROM frolvlad/alpine-ghc as build
# install build tools
# cache dependencies
# build
RUN apk add cabal wget
RUN mkdir /cabal-build
RUN mkdir /cabal-bins
RUN cabal update
ADD test/ .
RUN cabal install --builddir=/cabal-build --installdir=/cabal-bins --install-method=copy\
    --enable-static --enable-executable-static test

# copy artifacts to a clean image
FROM alpine
RUN apk add libffi gmp
COPY --from=build /cabal-bins /cabal-bins

ADD https://github.com/aws/aws-lambda-runtime-interface-emulator/releases/latest/download/aws-lambda-rie /usr/bin/aws-lambda-rie
RUN chmod 755 /usr/bin/aws-lambda-rie
COPY entry.sh /
RUN chmod 755 /entry.sh
ENTRYPOINT [ "/entry.sh" ]
