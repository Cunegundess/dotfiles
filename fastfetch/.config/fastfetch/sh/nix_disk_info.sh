#!/bin/sh
df -h / | awk 'NR==2 {print $3 " used, " $4 " free"}'
