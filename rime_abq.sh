#!/bin/bash

# Check if the required arguments are provided
while getopts ":b:q:p:i:f:o:" opt; do
  case $opt in
    b) BAIT="$OPTARG";;
    q) QUOTENB="$OPTARG";;
    p) PROJECTID="$OPTARG";;
    i) INSTITUTION="$OPTARG";;
    f) FOLDER="$OPTARG";;
    o) ORG="$OPTARG";;
    \?) echo "Invalid option: -$OPTARG" >&2
        exit 1;;
    :) echo "Option -$OPTARG requires an argument." >&2
       exit 1;;
  esac
done

# Check if all required arguments are provided
if [ -z "$BAIT" ] || [ -z "$QUOTENB" ] || [ -z "$PROJECTID" ] || [ -z "$INSTITUTION" ] || [ -z "$FOLDER" ] || [ -z "$ORG" ]; then
  echo "Usage: $0 -b BAIT -q QUOTENB -p PROJECTID -i INSTITUTION -f FOLDER -o ORG"
  exit 1
fi

# make enriched protein list
Rscript rime_abq.R -b "$BAIT" -q "$QUOTENB" -p "$PROJECTID" -i "$INSTITUTION"

# generate report
Rscript -e "rmarkdown::render('rime_report_abq.Rmd', params = list(folder='$FOLDER', projectid='$PROJECTID', quotenb='$QUOTENB', bait='$BAIT', org='$ORG'))"

output_folder="Q-${QUOTENB}_RIMEabq"

mkdir -p "$output_folder"

mv rime_report_abq.html "$output_folder/"
