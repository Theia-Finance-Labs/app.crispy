#!/bin/bash

# Check for correct number of arguments
if [ "$#" -ne 10 ]; then
    echo "Usage: $0 S3_URL S3_ACCESS_KEY S3_SECRET_KEY S3_BUCKET S3_REGION POSTGRES_USERNAME POSTGRES_PASSWORD POSTGRES_HOST POSTGRES_PORT POSTGRES_DB"
    exit 1
fi

# Assigning arguments to variables
S3_URL=$(echo -n "$1" | base64)
S3_ACCESS_KEY=$(echo -n "$2" | base64)
S3_SECRET_KEY=$(echo -n "$3" | base64)
S3_BUCKET=$(echo -n "$4" | base64)
S3_REGION=$(echo -n "$5" | base64)

POSTGRES_USERNAME=$(echo -n "$6" | base64)
POSTGRES_PASSWORD=$(echo -n "$7" | base64)
POSTGRES_HOST=$(echo -n "$8" | base64)
POSTGRES_PORT=$(echo -n "$9" | base64)
POSTGRES_DB=$(echo -n "${10}" | base64)

# Substitute variables in the k8s-trisk-api and apply
sed -e "s|\${POSTGRES_USERNAME}|${POSTGRES_USERNAME}|g" \
    -e "s|\${POSTGRES_PASSWORD}|${POSTGRES_PASSWORD}|g" \
    -e "s|\${POSTGRES_HOST}|${POSTGRES_HOST}|g" \
    -e "s|\${POSTGRES_PORT}|${POSTGRES_PORT}|g" \
    -e "s|\${POSTGRES_DB}|${POSTGRES_DB}|g" \ 
    -e "s|\${S3_URL}|${S3_URL}|g" \
    -e "s|\${S3_ACCESS_KEY}|${S3_ACCESS_KEY}|g" \
    -e "s|\${S3_SECRET_KEY}|${S3_SECRET_KEY}|g" \
    -e "s|\${S3_BUCKET}|${S3_BUCKET}|g" \
    -e "s|\${S3_REGION}|${S3_REGION}|g" k8s-trisk-api.yaml > k8s-trisk-api.yaml

kubectl apply -f k8s-trisk-api.yaml
