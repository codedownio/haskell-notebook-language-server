#!/usr/bin/env bash

set -e

stack build --stack-yaml stack-8.10.7.yaml
stack build --stack-yaml stack-9.0.2.yaml
stack build --stack-yaml stack-9.2.8.yaml
stack build --stack-yaml stack-9.4.7.yaml
stack build --stack-yaml stack-9.6.3.yaml
