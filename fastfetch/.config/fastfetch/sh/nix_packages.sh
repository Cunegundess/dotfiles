#!/bin/sh
count="nix-store --query --requisites /run/current-system/sw | wc -l"
echo "$(count)"
