#!/usr/bin/env bash

set -e

stack build --stack-yaml stack/stack-9.0.2.yaml
stack build --stack-yaml stack/stack-9.2.8.yaml
stack build --stack-yaml stack/stack-9.4.8.yaml
stack build --stack-yaml stack/stack-9.6.6.yaml
stack build --stack-yaml stack/stack-9.8.4.yaml
stack build --stack-yaml stack/stack-9.10.2.yaml
stack build --stack-yaml stack/stack-9.12.2.yaml
