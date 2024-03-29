import os
import UnityPy
from typing import Any
import shutil

# i suck at python

def getFilesList(path: str) -> list[str]:
    res: list[str] = []
    for root, _, files in os.walk(path):
        for file in files:
            relative_path = os.path.relpath(os.path.join(root, file), start=path)
            res.append(relative_path)
    return res

shutil.rmtree('./install/')
shutil.copytree('../../original_game_files/', './install/', dirs_exist_ok=True)
for filepath in getFilesList("./install/"):
    env = UnityPy.load("./install/" + filepath)
    for obj in env.objects:
        if obj.type.name == "TextAsset":
            data: Any = obj.read()
            name = data.name.upper()
            if name == "CARD_PART":
                file = open('../../data/release_bytes/Card_Part.bytes', 'rb')
                binary_data = file.read()
                file.close()
                data.script = binary_data
                data.save()
            if name == "CARD_DESC":
                file = open('../../data/release_bytes/CARD_Desc.bytes', 'rb')
                binary_data = file.read()
                file.close()
                data.script = binary_data
                data.save()
            if name == "CARD_INDX":
                file = open('../../data/release_bytes/CARD_Indx.bytes', 'rb')
                binary_data = file.read()
                file.close()
                data.script = binary_data
                data.save()
        with open("./install/" + filepath, "wb") as f:
            f.write(env.file.save())

shutil.rmtree( './install_with_empty_lines/')
shutil.copytree('../../original_game_files/', './install_with_empty_lines/', dirs_exist_ok=True)
for filepath in getFilesList("./install_with_empty_lines/"):
    env = UnityPy.load("./install_with_empty_lines/" + filepath)
    for obj in env.objects:
        if obj.type.name == "TextAsset":
            data: Any = obj.read()
            name = data.name.upper()
            if name == "CARD_PART":
                file = open('../../data/release_bytes_with_empty_lines/Card_Part.bytes', 'rb')
                binary_data = file.read()
                file.close()
                data.script = binary_data
                data.save()
            if name == "CARD_DESC":
                file = open('../../data/release_bytes_with_empty_lines/CARD_Desc.bytes', 'rb')
                binary_data = file.read()
                file.close()
                data.script = binary_data
                data.save()
            if name == "CARD_INDX":
                file = open('../../data/release_bytes_with_empty_lines/CARD_Indx.bytes', 'rb')
                binary_data = file.read()
                file.close()
                data.script = binary_data
                data.save()
        with open("./install_with_empty_lines/" + filepath, "wb") as f:
            f.write(env.file.save())
