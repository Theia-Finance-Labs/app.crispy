#!/bin/bash

# Check for correct number of arguments
if [ "$#" -ne 3 ]; then
    echo "Usage: $0 MY_REGISTRY_URL MY_REGISTRY_USERNAME MY_REGISTRY_PASSWORD"
    exit 1
fi

MY_REGISTRY_URL=$(echo -n "$1" | base64)
MY_REGISTRY_USERNAME=$(echo -n "$2" | base64)
MY_REGISTRY_PASSWORD=$(echo -n "$3" | base64)

# Substitute variables in k8s.yaml and apply
kubectl apply -f <(sed -e "s|\${MY_REGISTRY_URL}|${MY_REGISTRY_URL}|g" \
    -e "s|\${MY_REGISTRY_USERNAME}|${MY_REGISTRY_USERNAME}|g" \
    -e "s|\${MY_REGISTRY_PASSWORD}|${MY_REGISTRY_PASSWORD}|g" k8s.yaml)

