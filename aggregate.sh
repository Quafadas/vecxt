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
    
    # Read the JSON content
    jsonString=$(cat "$file")
    
    # Check if the branch is "main" and add to allJson array
    branch=$(echo "$jsonString" | jq -r '.branch')
    if [[ "$branch" == "main" ]]; then
      allJson+=("$jsonString")
    fi
  fi
done

# Combine all JSON objects into a single JSON array and write to the output file
jq -s '.' <<< "${allJson[@]}" > "$outputFile"

echo "Combined JSON written to $outputFile"
