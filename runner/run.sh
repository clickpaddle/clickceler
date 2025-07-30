#!/bin/sh
docker run   \
  --rm --name trex-runner \
  --user 1001:1001 \
  -e RUNNER_NAME=trex-runner \
  -e RUNNER_LABELS=debian,self-hosted \
  -e GITHUB_URL=https://github.com/clickpaddle/clickceler \
  -e GITHUB_TOKEN="${TOKEN}" \
  -v runner-data:/home/runner/actions-runner/_work  ${REGISTRY_HOST}:5000/arctrex:1.0
