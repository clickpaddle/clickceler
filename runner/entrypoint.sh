#!/bin/bash
set -e

if [ -n "$ACTIONS_RUNNER_INPUT_JITCONFIG" ]; then
    echo "Using JIT config from controller..."
    # Décoder le JSON base64
    JIT_JSON=$(echo "$ACTIONS_RUNNER_INPUT_JITCONFIG" | base64 --decode)
    RUNNER_URL=$(echo "$JIT_JSON" | jq -r '.runner.url')
    REGISTRATION_TOKEN=$(echo "$JIT_JSON" | jq -r '.runner.token')
    RUNNER_NAME=$(echo "$JIT_JSON" | jq -r '.runner.name')
    RUNNER_LABELS=$(echo "$JIT_JSON" | jq -r '.runner.labels // empty')

    ./config.sh --unattended --url "$RUNNER_URL" --token "$REGISTRATION_TOKEN" \
        --name "$RUNNER_NAME" --labels "$RUNNER_LABELS" --work "_work" --replace
else
    echo "Fetching registration token from GitHub API..."
    REGISTRATION_TOKEN=$(curl -s -X POST \
      -H "Authorization: token $GITHUB_TOKEN" \
      -H "Accept: application/vnd.github+json" \
      "$GITHUB_URL/actions/runners/registration-token" | jq -r '.token')

    ./config.sh --unattended --url "$GITHUB_URL" --token "$REGISTRATION_TOKEN" \
      --name "$RUNNER_NAME" --labels "$RUNNER_LABELS" --work "_work" --replace
fi

# Lancer le runner
./run.sh
