#!/bin/bash

# Set the current working directory and output file
benchmarkCacheDir=$(pwd)
outputFile="$benchmarkCacheDir/benchmark_history.json"

# Initialize an empty array to store JSON objects
allJson=()

# Iterate through all JSON files in the benchmark cache directory
for file in "$benchmarkCacheDir"/*.json; do
  # Exclude the output file
  if [[ "$file" != "$outputFile" ]]; then
    echo "Processing $file"

        # Capture the root-level fields (branch and date) and merge them with each item in data[]
    jsonString=$( jq 'select(.branch == "main") | {branch: .branch, date: .date, commit: .commit, host: .host } as $meta | .data[] | {benchmark: .benchmark, score: .primaryMetric.score, params: .params, scoreLowerConfidence: .primaryMetric.scoreConfidence[0],scoreUpperConfidence: .primaryMetric.scoreConfidence[1], scoreUnit: .primaryMetric.scoreUnit} + $meta' "$file" )


    # Check if the JSON data was extracted successfully
    if [[ -n "$jsonString" && "$jsonString" != "[]" ]]; then
      allJson+=("$jsonString")
    fi
  fi
done

# Combine all JSON objects into a single JSON array and write to the output file
if [[ ${#allJson[@]} -gt 0 ]]; then
  printf "%s\n" "${allJson[@]}" | jq -s '.' > "$outputFile"
  echo "Combined JSON written to $outputFile"
else
  echo "No valid JSON data to write."
fi