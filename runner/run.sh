#!/bin/sh
docker run  -it  \
  --rm --name trex-runner \
  -e RUNNER_NAME="trex-runner" \
  -e RUNNER_LABELS="debian,self-hosted" \
  -e GITHUB_URL="https://github.com/clickpaddle/clickceler" \
  -e GITHUB_TOKEN="${TOKEN}" \
--entrypoint /bin/bash \
  -v runner-data:/home/runner/actions-runner/  ${REGISTRY_HOST}:5000/arctrex:1.2 
