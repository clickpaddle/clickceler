#!/bin/bash

# Vérification des variables d'environnement requises
: "${GITHUB_URL:?Variable d'environnement GITHUB_URL non définie}"
: "${GITHUB_TOKEN:?Variable d'environnement GITHUB_TOKEN non définie}"
: "${RUNNER_NAME:?Variable d'environnement RUNNER_NAME non définie}"
: "${RUNNER_WORKDIR:=_work}"  # Valeur par défaut _work si non définie

set -e

echo "Registering runner..."

# Configuration du runner en mode unattended
./config.sh --unattended \
  --url "$GITHUB_URL" \
  --token "$GITHUB_TOKEN" \
  --name "$RUNNER_NAME" \
  --work "$RUNNER_WORKDIR" \
  --replace

cleanup() {
  echo "Removing runner..."
  # Supprime proprement le runner lors de l'arrêt du container
  ./config.sh remove --unattended --token "$GITHUB_TOKEN"
}

# Capturer les signaux SIGINT (Ctrl+C) et SIGTERM pour un nettoyage propre
trap 'cleanup; exit 130' INT
trap 'cleanup; exit 143' TERM

echo "Starting runner..."
./run.sh

