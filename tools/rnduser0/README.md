# Yu-Gi-Oh! Master Duel - Improve Card Text Readibility mod
This mod adds line breaks to card effect text to separate multiple effects from each other.

Original:
![Supreme King Z-ARC_1_before](https://github.com/RndUser0/YGOMD-Improve_Card_Text_Readibility/assets/29837561/abcd4d63-2f1b-4c6e-8a55-8e96318602f8)

Modded:
![Supreme King Z-ARC](https://github.com/RndUser0/YGOMD-Improve_Card_Text_Readibility/assets/29837561/a5c789bb-3c58-4fe0-bc5c-1b176f83a0a3)

## Installing the mod from the Releases page
### Introduction
The mod contains only text files but to keep the risk of being warned or banned as minimal as possible, you should note the following while using this mod:
* Don't show your Master Duel, Konami, Steam or Google account information in videos or images that you publish.
* When you post anything regarding Master Duel mods, don't use your main Master Duel, Konami, Steam or Google account for that.
* There's a pattern at the top right of the screen when you're logged into Master Duel which you should censor if you publish images or videos of Master Duel because this pattern differs for every account and could be used to identify your account.

### Requirements for using Master Duel mods
* Install Yu-Gi-Oh! Master Duel on your PC via Steam.
* Play through the tutorial.
* Let the game download all files after that.

### Installing/Uninstalling the mod:
1. Close the game if it's running.
2. Open the "Install mod" or "Uninstall mod" folder from the ZIP archive in Windows Explorer.
3. Select the folder "0000" by clicking on it.
4. Copy the selected folder to the clipboard by pressing [Ctrl] + [C].
5. Go to your Steam library.
6. Right-click "Yu-Gi-Oh! Master Duel"
7. Go to "Manage → Browse local files".
8. Open the folder "LocalData".
9. Inside that folder is another folder whose name varies but it always has 8 alphanumeric characters, for example 1a23bc4d, open it.
10. Paste the folder from the clipboard by pressing [Ctrl] + [V].
11. Choose the option to overwrite/replace all files and let the process finish.

## Manual modding
### A. Required tools for card text replacement
* [Python 3](https://www.python.org/downloads/)
* [AssetStudio](https://github.com/Perfare/AssetStudio/releases) - Tool for extracting Unity assets
* [UABEA](https://github.com/nesrak1/UABEA/releases) - Tool for importing a modded file back into a game file

### B. Location of the Unity files which contain the English Card_* files used by the game:
* CARD_Desc: .\LocalData\????????\0000\95\95e73521
* CARD_Indx: .\LocalData\????????\0000\e4\e43e4f67
* CARD_Name: .\LocalData\????????\0000\c0\c071e773
* Card_Pidx: .\LocalData\????????\0000\fd\fd071f0b
* Card_Part: .\LocalData\????????\0000\e6\e63ab74f

(If this list is out of date, refer to the [Modding guide](https://www.nexusmods.com/yugiohmasterduel/articles/3?BH=0) which is updated more frequently.)

### C. Preparation
1. Click on the green **Code** button in the top right and then on **Download ZIP**.
2. Extract the ZIP file you've just dowloaded into a new folder.
3. Copy the above Unity files into the same folder as the Python files.
4. If it's the first time you're reading this guide or you get a "ModuleNotFoundError", install the requirements with the file "!install_requirements.bat" or use the command ```python -m pip install -r requirements.txt``` to install them.

### D. Extracting the Card_* files from the Unity files
1. Load all 5 **Card_\*** files into **Asset Studio** by using drag'n'drop or **File** → **Load Folder**.
2. Click on the **Asset List** tab.
3. Click on **Filter Type** → **TextAsset**.
4. Select the 5 files, then right-click one of them.

![AssetStudio_export](https://github.com/RndUser0/YGOMD-Improve_Card_Text_Readibility/assets/29837561/c0674e92-7949-45f8-a809-37b6fc3e0fc7)

5. Select **Export selected assets**.
6. Choose a location and click on **Select folder**.
7. Run the file **_CARD_decrypt_and_split.py** to decrypt all 5 **Card_\*** files and split the **CARD_Name** and **CARD_Desc** files to JSON.

### E. Replacing the card text and reconverting the Card_* files 
1. Run the file **_CARD_Desc_replace.py** to replace the card text and modify the decrypted **Card_Part** file.
2. Run the file **_CARD_merge+encrypt.py** to reconvert and reencrypt the files, so the game can read them.

### F. Importing the modified Card_* files back into the Unity files.
1. Create a backup of the original Unity file containing the **CARD_Desc** file .
2. Load the original Unity file containing the **CARD_Desc** file into **UABEA** by using drag'n'drop or **File** → **Open**.
3. If the file you opened is compressed, a new window named **Message Box** will appear. Click on **Memory** there.
4. Click on **Info** in the main **UABEA** window.
5. In the **Assets Info** window, select the row with the name **CARD_Desc** which has the **TextAsset** type.

![UABEA_import_txt](https://github.com/RndUser0/YGOMD-Improve_Card_Text_Readibility/assets/29837561/a84268a4-601a-408b-86be-e5343be6b97f)
  
6. Click on **Plugins** → **Import .txt** → **Ok**.
7. If the **CARD_Desc** file isn't displayed in the list, click on the drop down box to the right saying **bytes files (\*.bytes)** and select **All types (\*.*)**.
8. Select the **CARD_Desc(.bytes)** file and click **Open** or double-click the file.
9. Click on **File** → **Save** → **OK**.
10. Close the **Assets Info** window.
11. In the main **UABEA** window click on **File** → **Save**.
12. Repeat steps 1 to 11 for the **CARD_Indx(.bytes)** and **Card_Part(.bytes)** files.

### G. Credits
* [akintos](https://gist.github.com/akintos) for [the original decryption script](https://gist.github.com/akintos/04e2494c62184d2d4384078b0511673b)
* [AmidoriA](https://github.com/AmidoriA) for [the original guide](https://github.com/AmidoriA/Master-Duel-Effect)
* [crazydoomy](https://github.com/crazydoomy) for [the original encryption script](https://discord.com/channels/747402959117353022/938180052984659979/959192997667422228)
* [thenobletheif](https://www.nexusmods.com/yugiohmasterduel/users/26473124) for the RegEx replacement instructions and some regular instructions.
* [timelic](https://github.com/timelic) for [the JSON split and merge scripts](https://github.com/timelic/master-duel-chinese-translation-switch)
