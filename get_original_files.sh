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
cp -r "$destination_dir" "${destination_dir}_backup"
rm -rf "$destination_dir"
mkdir -p "$destination_dir"

# Get the total number of files to process
total_files=$(find "$base_dir" -type f | wc -l)
current_file=0

echo "Scanning all game files to copy CARD-related ones. ENSURE THE MOD IS NOT INSTALLED. This can take a while, so hang tight. (This could be done faster manually, but this is the automated way for now)"
# Loop through each subfile recursively in the base directory
find "$base_dir" -type f | while read -r subfile; do
    # Update the progress
    ((current_file++))
    progress=$(echo "scale=2; $current_file * 100 / $total_files" | bc)
    printf "\rProcessing files... %6.2f%% completed. " "$progress"

    # Run "foobar" on each subfile
    if ./AssetStudioModCLI "$subfile" -t textAsset --filter-by-name CARD_ --filter-by-name Card_ | grep "Exported"; then
        # Extract subpath relative to base directory
        subpath_to_file="${subfile#$base_dir}"
        # Create the destination subdirectory if it doesn't exist
        mkdir -p "$(dirname "$destination_dir/$subpath_to_file")"
        # Copy the file if "foobar" is successful
        cp "$subfile" "$destination_dir/$subpath_to_file"
    fi
done
cd ../..

# Print a newline after finishing the loop
echo -e "\nProcessing complete."
