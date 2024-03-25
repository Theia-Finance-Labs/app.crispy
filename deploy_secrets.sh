#!/bin/bash

# Check for correct number of arguments
if [ "$#" -ne 5 ]; then
    echo "Usage: $0 POSTGRES_USERNAME POSTGRES_PASSWORD POSTGRES_HOST POSTGRES_PORT POSTGRES_DB"
    exit 1
fi

POSTGRES_USERNAME=$(echo -n "${1}" | base64)
POSTGRES_PASSWORD=$(echo -n "${2}" | base64)
POSTGRES_HOST=$(echo -n "${3}" | base64)
POSTGRES_PORT=$(echo -n "${4}" | base64)
POSTGRES_DB=$(echo -n "${5}" | base64)

# Substitute variables in k8s.yaml and apply
kubectl apply -f <( sed -e "s|\${POSTGRES_USERNAME}|${POSTGRES_USERNAME}|g" \
    -e "s|\${POSTGRES_PASSWORD}|${POSTGRES_PASSWORD}|g" \
    -e "s|\${POSTGRES_HOST}|${POSTGRES_HOST}|g" \
    -e "s|\${POSTGRES_PORT}|${POSTGRES_PORT}|g" \
    -e "s|\${POSTGRES_DB}|${POSTGRES_DB}|g" k8s-secrets.yaml )
