#!/bin/sh
if [ -z "${AWS_LAMBDA_RUNTIME_API}" ]; then
  exec /usr/bin/aws-lambda-rie "/cabal-bins/$@"
else
  exec "/cabal-bins/$@"
fi
