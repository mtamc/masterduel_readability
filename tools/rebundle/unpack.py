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

shutil.copytree('../../original_game_files/', './original/', dirs_exist_ok=True)
os.makedirs("bytes", exist_ok=True)
for filepath in getFilesList("./original"):
    env = UnityPy.load("./original/" + filepath)
    for obj in env.objects:
        if obj.type.name == "TextAsset":
            data: Any = obj.read()
            with open("./bytes/" + data.name + ".bytes", "wb") as f:
                f.write(bytes(data.script))
