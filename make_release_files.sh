#!/bin/bash
set -e

# this file isn't DRY at all lol

ver="$1"
date=$(date +%Y-%m-%d)

echo "Ensure the files at ./original_game_files are up to date! You can update them with ./get_original_files.sh (THE MOD SHOULD NOT BE INSTALLED WHEN YOU DO THIS)"

echo "STEP 1/6 | exporting game file assets as bytes"
cd ./tools/rebundle/
rm -f bytes/*
python ./unpack.py
cd ../..

echo "STEP 2/6 | decoding bytes and turning names/descs into jsons for the Haskell program"
cd ./tools/rnduser0/
rm -f CARD_Desc.bytes CARD_Desc.bytes.dec CARD_Desc.bytes.dec.json CARD_Indx.bytes CARD_Indx.bytes.dec CARD_Name.bytes CARD_Name.bytes.dec CARD_Name.bytes.dec.json Card_Part.bytes Card_Part.bytes.dec Card_Pidx.bytes Card_Pidx.bytes.dec
cp -r ../rebundle/bytes/* .
python ./_CARD_decrypt_and_split.py
cd ../..
cp "./tools/rnduser0/CARD_Name.bytes.dec.json" "./data/CARD_Name.bytes.dec.json"
cp "./tools/rnduser0/CARD_Desc.bytes.dec.json" "./data/CARD_Desc.bytes.dec.json"
cp "./tools/rnduser0/Card_Part.bytes.dec" "./data/Card_Part.bytes.dec"
cp "./tools/rnduser0/Card_Pidx.bytes.dec" "./data/Card_Pidx.bytes.dec"

echo "STEP 3/6 | running the haskell program (this can take up to 2 minutes)"
cabal run

echo "STEP 4/6 | encrypting the result"
mkdir -p ./data/release_bytes
cp data/CARD_Desc.bytes.dec.json.new tools/rnduser0/CARD_Desc.bytes.dec.json
cp data/Card_Part.bytes.dec.new tools/rnduser0/Card_Part.bytes.dec
cd tools/rnduser0/ && python _CARD_merge_and_encrypt.py && cd ../..
cp tools/rnduser0/CARD_Desc.bytes ./data/release_bytes/
cp tools/rnduser0/CARD_Indx.bytes ./data/release_bytes/
cp tools/rnduser0/Card_Part.bytes ./data/release_bytes/

mkdir -p ./data/release_bytes_with_empty_lines
cp data/CARD_Desc.bytes.dec.json.withemptylines.new tools/rnduser0/CARD_Desc.bytes.dec.json
cp data/Card_Part.bytes.dec.withemptylines.new tools/rnduser0/Card_Part.bytes.dec
cd tools/rnduser0/ && python _CARD_merge_and_encrypt.py && cd ../..
cp tools/rnduser0/CARD_Desc.bytes ./data/release_bytes_with_empty_lines/
cp tools/rnduser0/CARD_Indx.bytes ./data/release_bytes_with_empty_lines/
cp tools/rnduser0/Card_Part.bytes ./data/release_bytes_with_empty_lines/

mkdir -p ./data/release_bytes_numbering_only
cp data/CARD_Desc.bytes.dec.json.numberingOnly.new tools/rnduser0/CARD_Desc.bytes.dec.json
cp data/Card_Part.bytes.dec.numberingOnly.new tools/rnduser0/Card_Part.bytes.dec
cd tools/rnduser0/ && python _CARD_merge_and_encrypt.py && cd ../..
cp tools/rnduser0/CARD_Desc.bytes ./data/release_bytes_numbering_only/
cp tools/rnduser0/CARD_Indx.bytes ./data/release_bytes_numbering_only/
cp tools/rnduser0/Card_Part.bytes ./data/release_bytes_numbering_only/

mkdir -p ./data/release_bytes_numbering_and_newlines
cp data/CARD_Desc.bytes.dec.json.numberingAndNewlines.new tools/rnduser0/CARD_Desc.bytes.dec.json
cp data/Card_Part.bytes.dec.numberingAndNewlines.new tools/rnduser0/Card_Part.bytes.dec
cd tools/rnduser0/ && python _CARD_merge_and_encrypt.py && cd ../..
cp tools/rnduser0/CARD_Desc.bytes ./data/release_bytes_numbering_and_newlines/
cp tools/rnduser0/CARD_Indx.bytes ./data/release_bytes_numbering_and_newlines/
cp tools/rnduser0/Card_Part.bytes ./data/release_bytes_numbering_and_newlines/


echo "STEP 5/6 | bundling the encrypted files"
cd ./tools/rebundle
python pack.py
cd ../..
rm -rf release
mkdir release
cp -r ./original_game_files ./release/uninstall
cp -r ./tools/rebundle/install ./release/install
cp -r ./tools/rebundle/install_with_empty_lines ./release/install_with_empty_lines
cp -r ./tools/rebundle/install_with_numbering_only ./release/install_with_numbering_only
cp -r ./tools/rebundle/install_with_numbering_and_empty_lines_only ./release/install_with_numbering_and_empty_lines_only
cp ./data/decoded_cards.updated.json ./release/effects.json
cp ./data/difference_with_last_release.json ./release/difference_with_last_release.json

echo "STEP 6/6 | running font size adjuster per variant and merging outputs"
repo_root="$(pwd)"
adjuster_dir="${repo_root}/tools/font-size-adjuster"
adjuster_script="${adjuster_dir}/Font_size_adjuster_for_Readability_Mod--multiprocessing.py"
variants=(install install_with_empty_lines install_with_numbering_only install_with_numbering_and_empty_lines_only)
for v in "${variants[@]}"; do
    echo "--- font size adjuster: ${v} ---"
    variant_dir="${repo_root}/release/${v}"
    (cd "${adjuster_dir}" && python "${adjuster_script}" "${variant_dir}")

    # Merge modded 0000 output into the release variant's install folder (ignore AssetBundle on purpose)
    if [ -d "${adjuster_dir}/4 modded files/0000" ]; then
        mkdir -p "${variant_dir}/0000"
        cp -r "${adjuster_dir}/4 modded files/0000/." "${variant_dir}/0000/"
    fi
done

if [ -d "${adjuster_dir}/1 copied game files/0000" ]; then
    mkdir -p "${repo_root}/release/uninstall/0000"
    cp -rn "${adjuster_dir}/1 copied game files/0000/." "${repo_root}/release/uninstall/0000/"
fi

cd release
zip -r "./readable-card-effects-release-${date}-${ver}.zip" ./uninstall ./install ./install_with_empty_lines ./install_with_numbering_only ./install_with_numbering_and_empty_lines_only ./difference_with_last_release.json
cd ..

echo "the release files are in ./release, also zipped to ./release/"
