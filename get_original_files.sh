#!/bin/bash

# Check if the base directory is provided as the first argument
if [ -z "$1" ]; then
    echo "please write as first argument the path to the game folder which contains the 0000 subfolder"
    exit 1
fi

# Assign base directory from the first argument
base_dir="$1"

# Ensure the base directory ends with a '/'
[[ "${base_dir}" != */ ]] && base_dir="${base_dir}/"

# Check if base_dir contains the 0000 subfolder
if [ ! -d "${base_dir}0000" ]; then
    echo "The specified directory does not contain the 0000 subfolder."
    exit 1
fi

# Create the destination directory if it doesn't exist
cd ./tools/assetstudiocli
destination_dir="../../original_game_files"
rm -rf "$destination_dir"
mkdir -p "$destination_dir"

# Get the total number of files to process for the progress calculation
total_files=$(find "$base_dir" -type f | wc -l)
processed_files=0
success_count=0

echo "Scanning all game files to copy CARD-related ones. ENSURE THE MOD IS NOT INSTALLED. This shouldn't take too long. (The progress % shows the total game files, but the relevant game files should be found long before we scan all of them.)"
# Find, sort, and loop through each subfile by last-modified-date
while IFS= read -r subfile; do
    # Run "./AssetStudioModCLI" on each subfile and check for success
    if ./AssetStudioModCLI "$subfile" -t textAsset --filter-by-name CARD_Desc --filter-by-name CARD_Indx --filter-by-name CARD_Name --filter-by-name Card_Part --filter-by-name Card_Pidx | grep -q "Exported"; then
        # Extract subpath relative to base directory
        subpath_to_file="${subfile#$base_dir}"
        # Create the destination subdirectory if it doesn't exist
        mkdir -p "$(dirname "$destination_dir/$subpath_to_file")"
        # Copy the file if "./AssetStudioModCLI" is successful
        cp "$subfile" "$destination_dir/$subpath_to_file"
        echo "Copied  "$destination_dir/$subpath_to_file""

        # Increment the success counter
        ((success_count++))
        # NOTE: the AssetStudioModCLI command will match on both CARD_Name and CARD_Named, and IDK how to get it to tell me what the actual found name of the asset is, so we just fetch all 6 even though we don't need CARD_Named
        if [ "$success_count" -ge 6 ]; then
            printf "\r| Progress: %6.2f%% - Successful operations: %d | \n" "$(bc <<< "scale=2; $processed_files * 100 / $total_files")" "$success_count"
            echo "Found all 6 files, stopping now."
            break
        fi
    fi
    # Increment the processed files counter
    ((processed_files++))
    # Update progress percentage
    printf "\r| Progress: %6.2f%% - Successful operations: %d | " "$(bc <<< "scale=2; $processed_files * 100 / $total_files")" "$success_count"
done < <(find "$base_dir" -type f -printf '%T@ %p\n' | sort -nr | cut -d' ' -f2-)

echo "Processing complete. Total files processed: $processed_files"
