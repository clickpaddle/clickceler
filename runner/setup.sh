#!/bin/bash
# Install arc-runners in K8S cluster 
NAMESPACE="arc-systems"
helm uninstall  arc -n arc-systems
helm uninstall arc-runner-set -n arc-runners

helm upgrade --install arc \
    --namespace "${NAMESPACE}" \
    --create-namespace  --version 0.12.1 \
    oci://ghcr.io/actions/actions-runner-controller-charts/gha-runner-scale-set-controller

INSTALLATION_NAME="arc-runner-set"
NAMESPACE="arc-runners"
GITHUB_CONFIG_URL="https://github.com/clickpaddle/clickceler"
GITHUB_PAT=$GITHUB_PAT
helm upgrade --install "${INSTALLATION_NAME}" \
    --namespace "${NAMESPACE}" \
    --create-namespace --version 0.12.1\
    --set githubConfigUrl="${GITHUB_CONFIG_URL}" \
    --set githubConfigSecret.github_token="${GITHUB_PAT}" \
    oci://ghcr.io/actions/actions-runner-controller-charts/gha-runner-scale-set -f gha-runner-scale-set/values.yaml

