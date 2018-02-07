#!/bin/bash

# Clean twitter data

# 1. Keep only rows with 10 semi colons
# 2. Use R to convert to csv

awk -F'|' '{print gsub(/;/,";"), $0}' Twitter_flutweets/flutweets_semicolon-delim.csv | grep "^10" | cut -d " " -f 2- > temp
Rscript -e "write.csv(data.table::fread('temp', sep=';', header=TRUE, quote=''), 'Twitter_flutweets/flu.csv')"


awk -F'|' '{print gsub(/;/,";"), $0}' Twitter_nhstweets/nhstweets_semicolon-delim.csv | grep "^10" | cut -d " " -f 2- > temp
Rscript -e "write.csv(data.table::fread('temp', sep=';', header=TRUE, quote=''), 'Twitter_nhstweets/nhs.csv')"


