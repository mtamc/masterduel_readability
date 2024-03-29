#!/bin/bash
set -e

echo "Ensure the files at ./original_game_files are up to date! You can update them with ./get_original_files.sh (THE MOD SHOULD NOT BE INSTALLED WHEN YOU DO THIS)"

echo "STEP 1/5 | exporting game file assets as bytes"
cd ./tools/rebundle/
python ./unpack.py
cd ../..

echo "STEP 2/5 | decoding bytes and turning names/descs into jsons for the Haskell program"
cd ./tools/rnduser0/
rm -f CARD_Desc.bytes CARD_Desc.bytes.dec CARD_Desc.bytes.dec.json CARD_Indx.bytes CARD_Indx.bytes.dec CARD_Name.bytes CARD_Name.bytes.dec CARD_Name.bytes.dec.json Card_Part.bytes Card_Part.bytes.dec Card_Pidx.bytes Card_Pidx.bytes.dec
cp -r ../rebundle/bytes/* .
python ./_CARD_decrypt_and_split.py
cd ../..
cp "./tools/rnduser0/CARD_Name.bytes.dec.json" "./data/CARD_Name.bytes.dec.json"
cp "./tools/rnduser0/CARD_Desc.bytes.dec.json" "./data/CARD_Desc.bytes.dec.json"
cp "./tools/rnduser0/Card_Part.bytes.dec" "./data/Card_Part.bytes.dec"
cp "./tools/rnduser0/Card_Pidx.bytes.dec" "./data/Card_Pidx.bytes.dec"

echo "STEP 3/5 | running the haskell program (this can take up to 2 minutes)"
cabal run

echo "STEP 4/5 | encrypting the result"
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

echo "STEP 5/5 | bundling the encrypted files"
cd ./tools/rebundle
python pack.py
cd ../..
rm -rf release
mkdir release
cp -r ./original_game_files ./release/uninstall
cp -r ./tools/rebundle/install ./release/install
cp -r ./tools/rebundle/install_with_empty_lines ./release/install_with_empty_lines
dat=$(date +%Y-%m-%d)
cd release
zip -r "./release-$dat.zip" ./uninstall ./install ./install_with_empty_lines
cd ..

echo "the release files are in ./release, also zipped to ./release/"
