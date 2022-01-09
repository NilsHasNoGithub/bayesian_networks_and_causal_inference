#!/usr/bin/env bash

docker build -t bn_r . && \
docker run -it -v .:/workdir --rm bn_r