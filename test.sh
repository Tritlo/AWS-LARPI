#!/usr/bin/env bash
sudo docker build . -t "larpi"
sudo docker rm -f larpi || true
sudo docker run --rm --detach -p 9000:8080 --name larpi -it larpi test

curl -XPOST "http://localhost:9000/2015-03-31/functions/function/invocations" -d '{"hello": "ok"}'


