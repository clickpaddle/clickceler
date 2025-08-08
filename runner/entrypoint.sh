#!/bin/bash
set -e


echo "Fetching registration token from GitHub API..."

# 1. Obtenir le registration token depuis l'API GitHub Actions
REGISTRATION_TOKEN=$(curl -s -X POST \
  -H "Authorization: token $GITHUB_TOKEN" \
  -H "Accept: application/vnd.github+json" \
  "$GITHUB_URL/actions/runners/registration-token" | jq -r '.token')

if [ -z "$REGISTRATION_TOKEN" ] || [ "$REGISTRATION_TOKEN" == "null" ]; then
  echo "Failed to get registration token from GitHub API."
  exit 1
fi

echo "Registration token obtained."

# 2. Configurer le runner (remplace work directory si besoin)
./config.sh --unattended --url "$GITHUB_URL" --token "$REGISTRATION_TOKEN" \
  --name "$RUNNER_NAME" --labels "$RUNNER_LABELS" --work "_work" --replace

# 3. Lancer le runner
./run.sh

