'''
Credits:
- ULTIMATE CARD TEXT MOD - DIY Kit by FoodUnderKeyboard: https://www.nexusmods.com/yugiohmasterduel/mods/2087
- Multiprocessing modification and optimizations by RndUser: https://www.nexusmods.com/profile/RndUser
'''

import UnityPy, os, shutil, re, sys, json, subprocess, time, pickle, errno, multiprocessing as mp#, pathlib
from PIL import ImageFont
from datetime import datetime
from ctypes import c_bool, c_int, c_char_p

# Step 0.0: Common defs

def clear_folder(folder):
    for filename in os.listdir(folder):
        file_path = os.path.join(folder, filename)
        try:
            if os.path.isfile(file_path) or os.path.islink(file_path):
                os.unlink(file_path)
            elif os.path.isdir(file_path):
                shutil.rmtree(file_path)
        except Exception as e:
            print('Failed to delete %s. Reason: %s' % (file_path, e))

def silent_remove(filename):
    try:
        os.remove(filename)
    except OSError as e:
        if e.errno != errno.ENOENT: # errno.ENOENT = no such file or directory
            raise # re-raise exception if a different error occurred

def silent_copy(source_path, destination_path):
    try:
        shutil.copy2(source_path, destination_path)
    except OSError as e:
        if e.errno != errno.ENOENT:
            raise

def copy_and_replace(source_path, destination_path):
    silent_remove(destination_path)
    silent_copy(source_path, destination_path)

#This Python script's location (for common defs, not the script that calls this)
def script_folder():
    return os.path.dirname(os.path.abspath(__file__))

def file_walker(source_folder):
    for root, dirs, files in os.walk(source_folder):
        for file_name in files:
            yield os.path.join(root, file_name)        

def sort_file_list_by_mtime(list):
    return sorted(list, key = os.path.getmtime, reverse = True)

'''JSON stuff'''

def ReadJSON(file_path):
    with open(file_path, encoding="utf-8") as f:
        arr = json.load(f)
    f.close()
    print("Read JSON file:", file_path)
    return arr

def WriteJSON(l: list, file_path: str):
    with open(file_path, 'w', encoding='utf8') as f:
        json.dump(l, f, ensure_ascii=False, indent=4)
    f.close()
    print("Wrote JSON file to:", file_path)

'''Running other scripts stuff'''

def copy_scripts_to_folder(scripts_folder, script_names, destination_folder):
    for file_name in script_names:
        file_path = os.path.join(scripts_folder, file_name)
        copy_and_replace(file_path, os.path.join(destination_folder, file_name))
    print("Copied scripts to", destination_folder)

def run_script_on_other_files(script_path, todo_bytes, target_folder):
    for file_name in todo_bytes:
        bytes_path = os.path.join(target_folder, file_name)
        if os.path.exists(bytes_path):
            subprocess.call(["python", script_path, bytes_path])
    print("Ran script on other bytes files")

'''Braces stuff'''

def string_insert(orig_str, str_to_insert, index):
    return orig_str[:index]+str_to_insert+orig_str[index:]

def insert_braces(effect_text, parts_arr):
    #some sub-effects in Japanese don't exist in English (e.g. Solemn Judgment has 2 sub-effects in Japanese)
    parts_arr = [(a,b) for (a,b) in parts_arr if a < b]
    
    #Figure out where we will insert curly braces and how many
    L, R = '{', '}'
    insertion_dict = {}
    for (a,b) in parts_arr:
        if a     not in insertion_dict:
            insertion_dict[a  ] = {L:0, R:0}
        if (b+1) not in insertion_dict:
            insertion_dict[b+1] = {L:0, R:0}
        insertion_dict[a  ][L] += 1
        insertion_dict[b+1][R] += 1

    #Insert curly braces
    ans = effect_text.encode()#Have to do it in bytes
    indices_list = list(insertion_dict.keys())
    indices_list.sort(reverse=True)
    for index in indices_list:
        inserted_braces = insertion_dict[index][R]*b'}' + insertion_dict[index][L]*b'{'
        ans = string_insert(ans, inserted_braces, index)
    return ans.decode()#Return bytes to regular string

#Doesn't PERFECTLY give the indices; some of them are off by 1 from the game
#Other functions correct for this
#Output sorted (by construction)
def unbraced_brace_indices(in_str):
    ans = []
    j = -1
    in_str = in_str.encode()
    for i,c in enumerate(in_str):
        is_L = c == 123 #left brace; tried '{' and b'{' but neither worked, they're numbers for some reason
        is_R = c == 125 #right brace (no, it isn't 124, that's pipe | )
        if is_L:
            ans.append(-1)
        elif is_R:
            ans.append(j)
        else:
            j += 1
            for k in range(len(ans)-1,-1,-1):
                if ans[k] != -1: break
                ans[k] = j
    return ans

'''Part/Pidx stuff'''

def get_pidx_table(pidx_dec_path):
    with open(pidx_dec_path, 'rb') as pidx_bytes:
        pidx = pidx_bytes.read()
    pidx_bytes.close()

    pidx_table = [None]*(len(pidx)//4)
    pidx_table[0] = (0,0,0) #First 4 bytes are just zero and not related to any card
    for i in range(4, len(pidx), 4):
        index_in_Card_Part_file = pidx[i+0] + pidx[i+1]*256 #Little-Endian
        main_effect_part_count, sub_n_pend_effect_part_count = divmod(pidx[i+3], 16)
        pidx_table[i//4] = (index_in_Card_Part_file, main_effect_part_count, sub_n_pend_effect_part_count)
    print("Made Pidx table")
    return pidx_table[1:]

def get_part_table(part_dec_path, pidx_table):
    with open(part_dec_path, 'rb') as part_bytes:
        part = part_bytes.read()
    part_bytes.close()

    part_table = [[] for _ in range(len(pidx_table))]
    for i,(a,b,c) in enumerate(pidx_table):
        if a == b == c == 0: continue
        for j in range(a, a+b+c):
            k = 4*j
            part_table[i].append((part[k+0]+part[k+1]*256, part[k+2]+part[k+3]*256)) #these are INCLUSIVE bounds for the effect text
    print("Made Part table")
    return part_table

def part_to_4_bytes(part):
    a,b = part
    return a.to_bytes(2, 'little') + b.to_bytes(2, 'little')

def write_part_file(part_file_path, part_table):
    indices = [None for _ in part_table]
    with open(part_file_path, 'wb') as file:
        file.write(4*nul)
        index = 1 #Because the first zeros are at index 0
        for j,changed_part_arr in enumerate(part_table):
            indices[j] = index
            for part in changed_part_arr:
                file.write(part_to_4_bytes(part))
                index += 1
    file.close()
    print("Wrote part file")
    return indices

'''
#No file actually uses this... yet
def write_pidx(pidx_file_path, pidx_table, indices=None):
    if indices:
        for i,(a,b,c) in enumerate(pidx_table):
            #Only apply to cards with effects
            if a == b == c == 0: continue
            pidx_table[i] = (indices[i],b,c)
    with open(pidx_file_path, 'wb') as file:
        file.write(4*nul)
        for (a,b,c) in pidx_table:
            if a == b == c == 0: #No effect = all zeros
                file.write(4*nul)
                continue
            out = a.to_bytes(2, 'little') + nul + (16*b+c).to_bytes()
            file.write(out)
    file.close()
    print("Wrote pidx file")
'''

'''Font stuff'''

def get_3400_and_30000_ID_line_index():
    json_data = ReadJSON(font_asset_path)
    norm_font = json_data['m_CardidNormalFontSizePairDatas']
    start_line_index = no_go_line_index = 0     
    for i in range(len(norm_font)):
        if norm_font[i]['cardid'] >=  3400: start_line_index = i+1 #HACKY SOLUTION
        if norm_font[i]['cardid'] >= 30000: no_go_line_index = i+1 #HACKY SOLUTION
        return start_line_index, no_go_line_index
    raise Exception("Not supposed to reach here")

'''Word/Widx stuff'''

def get_widx_table():
    with open(widx_dec_path, 'rb') as widx_bytes:
        widx = widx_bytes.read()
    widx_bytes.close()
    widx_table = [None]*(len(widx)//4)
    for i in range(0, len(widx), 4):
        widx_table[i//4] = int.from_bytes(widx[i:i+4], 'little') #Little-Endian
    return widx_table

def get_word_table():
    widx_table = get_widx_table()
    with open(word_dec_path, 'rb') as word_bytes:
        word = word_bytes.read()
    word_bytes.close()
    word_table = [None]*(len(widx_table)-1)
    for j in range(len(widx_table)-1):
        a = widx_table[j]
        b = widx_table[j+1]
        word_table[j] = word[a:b]
    return word_table

def nul_pad(b):
    return b + ((-len(b))%4)*nul

'''
def add_nomi(word_table): #Hard-coded values, so as to not be language-dependent
    nn = nomi_name.encode()
    if not nn: return
    s = 200 #Index of "/Normal"
    shifts = [27,28,29,30,31,40]
    for i in shifts:
        word_table[s+i] = nn + word_table[s+i]
        #This file almost never gets updated, so we need to make sure that we're not adding nomi to it multiple times
        while (2*nn in word_table[s+i]):
            word_table[s+i] = word_table[s+i].replace(2*nn, nn)
        word_table[s+i] = nul_pad(word_table[s+i])
'''

def write_widx_table(widx_list):
    with open(changed_widx_path, 'wb') as f:
        for i in range(len(widx_list)):
            index = widx_list[i]
            f.write(index.to_bytes(4, 'little'))
    f.close()
    
def write_word_table(word_table):
    widx_list = [0]
    with open(changed_word_path, 'wb') as f:
        for word in word_table:
            f.write(word)
            widx_list.append(widx_list[-1] + len(word))
    f.close()
    write_widx_table(widx_list)

def get_pend_stripped_marker():
    global pend_stripped_marker
    if not pend_stripped_marker:
        word_table = get_word_table()
        pend_stripped_marker = word_table[3].replace(nul, b'').decode()
    return pend_stripped_marker

def get_escaped_pend_marker():
    pend_stripped_marker = get_pend_stripped_marker()
    return "\\n"+pend_stripped_marker+"\\n"

def get_unescaped_pend_marker():
    pend_stripped_marker = get_pend_stripped_marker()
    return "\n"+pend_stripped_marker+"\n"

'''
#Nomi name
nomi_name = "/Nomi"
'''

#Folders
change_list_folder  = os.path.join(script_folder(), "0b change files")
scripts_folder      = os.path.join(script_folder(), "0a utils")

#Steps folders
copied_files_folder = os.path.join(script_folder(), "1 copied game files")
decrypt_folder      = os.path.join(script_folder(), "2 decrypted assets")
changed_folder      = os.path.join(script_folder(), "3 changed assets")
modded_folder       = os.path.join(script_folder(), "4 modded files")

#Decrypted files
part_dec_path   = os.path.join(decrypt_folder, "Card_Part.bytes.dec")
pidx_dec_path   = os.path.join(decrypt_folder, "Card_Pidx.bytes.dec")
prop_dec_path   = os.path.join(decrypt_folder, "CARD_Prop.bytes.dec")
word_dec_path   = os.path.join(decrypt_folder, "WORD_Text.bytes.dec")
widx_dec_path   = os.path.join(decrypt_folder, "WORD_Indx.bytes.dec")
font_asset_path = os.path.join(decrypt_folder, "CardPictureFontSetting.json")

#Decrypted files jsons
name_dec_path    = os.path.join(decrypt_folder, "CARD_Name.bytes.dec.json")
desc_dec_path    = os.path.join(decrypt_folder, "CARD_Desc.bytes.dec.json")
braced_save_path = os.path.join(decrypt_folder, "!Braced CARD_Desc.bytes.dec.json")

#Edited files
changed_braced_path   = os.path.join(changed_folder, "!Changed !Braced CARD_Desc.bytes.dec.json")
unbraced_changed_path = os.path.join(changed_folder, "!Unbraced !Changed CARD_Desc.bytes.dec.json")
changed_part_path     = os.path.join(changed_folder, "!Changed Card_Part.bytes.dec")
changed_fontsize_path = os.path.join(changed_folder, "!Changed CardPictureFontSetting.json")
changed_word_path     = os.path.join(changed_folder, "!Changed WORD_Text.bytes.dec")
changed_widx_path     = os.path.join(changed_folder, "!Changed WORD_Indx.bytes.dec")
changed_font_path     = os.path.join(changed_folder, "!Changed YGO_Card_NA.ttf")

# Step 0.1: Defs for "game file getter"

def is_correct_file(path, obj, search_term):
    data = obj.read()
    return data.m_Name == search_term

#Go directly to the expected file to see if it's the one
def named_search(path, search_term, expected_filename):
    expected_file_path = os.path.join(path, expected_filename[:2], expected_filename)
    # print expected file path
    print("Looking for", search_term, "in expected file path:", expected_file_path)
    if os.path.isfile(expected_file_path):
        print("Found file at expected path; checking if it's correct...")
        try:
            env = UnityPy.load(expected_file_path)
            print("Successfully loaded file at expected path:", expected_file_path)
            print(env.container.items())
        except Exception:
            print("Failed to get", expected_file_path, "by name search")
            return None
        names = []
        paths = []
        for path, obj in env.container.items():
            try:
                names.append(obj.read().m_Name)
            except Exception:
                names.append("<unreadable>")
            try:
                paths.append(path)
            except Exception:
                paths.append("<ungettable>")
            if is_correct_file(path, obj, search_term):
                return expected_file_path
            # log any important thing about obj
            print(obj.read())
        print("Names in", expected_file_path, ":", names)
        print("Paths in", expected_file_path, ":", paths)
    return None

#Sort the files in 0000 by closest size then look for the file
def size_search(path, search_term, expected_size):
    files_list = []
    for file_path in file_walker(path):
        files_list.append((abs(os.path.getsize(file_path) - expected_size), file_path))
    files_list.sort()
    for _,file_path in files_list:
        try:
            env = UnityPy.load(file_path)
        except Exception:
            continue
        for path, obj in env.container.items():
            if is_correct_file(path, obj, search_term):
                return file_path

def brute_force_search(path, search_term):
    for file_path in sort_file_list_by_mtime(file_walker(path)):
        try:
            env = UnityPy.load(file_path)
        except Exception:
            continue
        for path,obj in env.container.items():
            if is_correct_file(path, obj, search_term):
                return file_path

def search(search_term, bundle_folder_name, expected_filename, expected_size):
    if bundle_folder_name == "0000":
        path1 = path_0000
        path2 = path_AssetBundle
    else:
        path1 = path_AssetBundle
        path2 = path_0000

    #Try supplied directory first (if any)
    if supplied_dir:
        supplied_candidates = []
        primary = os.path.join(supplied_dir, bundle_folder_name)
        secondary = os.path.join(supplied_dir, "AssetBundle" if bundle_folder_name == "0000" else "0000")
        for cand in (primary, secondary):
            if os.path.isdir(cand):
                supplied_candidates.append(cand)
        for cand in supplied_candidates:
            r = named_search(cand, search_term, expected_filename)
            if r: return r
        if expected_size > 0:
            for cand in supplied_candidates:
                r = size_search(cand, search_term, expected_size)
                if r: return r
        for cand in supplied_candidates:
            r = brute_force_search(cand, search_term)
            if r: return r

    search1 = named_search(path1, search_term, expected_filename)
    if search1: return search1
    else:
        search1 = named_search(path2, search_term, expected_filename)
        if search1: return search1

    if expected_size > 0:
        print("Could not find", search_term, "with filename search method; searching by size...")

        search2 = size_search(path1, search_term, expected_size)
        if search2: return search2
        else:
            search2 = size_search(path2, search_term, expected_size)
            if search2: return search2

    print("Could not find", search_term, "with smart search methods; searching by modified time with brute force...")

    search3 = brute_force_search(path1, search_term)
    if search3: return search3
    else:
        search3 = brute_force_search(path2, search_term)
        if search3: return search3

    print("Could not find", search_term)

def multi_search(search_quadruples_list):
    ans = [None for _ in search_quadruples_list]
    for i, (search_term, bundle_folder_name, expected_filename, expected_size) in enumerate(search_quadruples_list):
        ans[i] = search(search_term, bundle_folder_name, expected_filename, expected_size)
    for file_path in ans:
        print("Found", file_path)
    return ans

#This Python script's location
def script_folder():
    return os.path.dirname(os.path.abspath(__file__))

def get_bundle_folder_name(path):
    return os.path.join(os.path.basename((os.path.dirname((os.path.dirname(path))))))

def copy_game_files(search_results):
    for found_path in search_results:
        if not found_path: continue
        found_file_name = os.path.basename(found_path)
        bundle_folder_name = get_bundle_folder_name(found_path)
        destination_folder = os.path.join(copied_files_folder, bundle_folder_name, found_file_name[:2])
        os.makedirs(destination_folder, exist_ok=True)
        copied_file_path = os.path.join(destination_folder, found_file_name)
        copy_and_replace(found_path, copied_file_path)
    print("Copied game files")

def config_path():
    return os.path.join(script_folder(), scripts_folder, "!_step_1_config.txt")

def get_config_data():
    with open(config_path(), encoding='utf-8') as config:
        lines = config.readlines()
    config.close()
    lines = [line.strip() for line in lines if not (line.strip().startswith("###") or line.strip() == '')]
    path_0000 = lines[0]
    search_quadruples_list = [line.split() for line in lines[1:]]
    
    if len(search_quadruples_list[0]) == 4:
        search_quadruples_list = [(a,b,c,int(d)) for (a,b,c,d) in search_quadruples_list]
    else:
        for i in range(len(search_quadruples_list)):
            search_quadruples_list[i] = [search_quadruples_list[i][0], "", "", 0]
    
    return path_0000, search_quadruples_list

def update_config(search_quadruples_list, search_results):
    with open(config_path()) as config:
        lines = config.readlines()
    config.close()
    line_indices = [i for i,line in enumerate(lines) if not (line.strip().startswith("###") or line.strip() == '')]
    line_indices = line_indices[1:]
    for j,found_path in enumerate(search_results):
        if not found_path: continue
        #Skip entries that came from supplied_dir — they aren't representative of installed-game cache
        if supplied_dir and os.path.abspath(found_path).startswith(supplied_dir):
            continue
        search_term = search_quadruples_list[j][0]
        bundle_folder_name = get_bundle_folder_name(found_path)
        expected_filename = os.path.basename(found_path)
        expected_size = os.path.getsize(found_path)
        lines[line_indices[j]] = " ".join([search_term, bundle_folder_name, expected_filename, str(expected_size)])+"\n"*(line_indices[j] != len(lines)-1)
    with open(config_path(), 'w', encoding='utf-8') as config:
        config.writelines(lines)
    config.close()

#Step 0.2: Defs for "asset decryptor"

#This Python script's location
def script_folder():
    return os.path.dirname(os.path.abspath(__file__))

def unpack_all_assets(source_folder: str, destination_folder: str):
    for file_path in file_walker(source_folder):
        env = UnityPy.load(file_path)
        for obj in env.objects:
            # process specific object types
            if obj.type.name == "MonoBehaviour":
                if not obj.serialized_type.node: continue
                tree = obj.read_typetree()
                path = os.path.join(destination_folder, f"{tree['m_Name']}.json")
                with open(path, "wt", encoding = "utf8") as f:
                    json.dump(tree, f, ensure_ascii = False, indent = 4)
                f.close()
            if obj.type.name == "TextAsset":
                data = obj.read()
                path = os.path.join(destination_folder, f"{data.m_Name}.bytes")
                with open(path, "wb") as f:
                    f.write(data.m_Script.encode("utf-8", "surrogateescape"))
                f.close()
            if obj.type.name == "Font":
                data = obj.read()
                if not data.m_FontData: continue
                extension = ".ttf"
                if data.m_FontData[0:4] == b"OTTO":
                    extension = ".otf"
                path = os.path.join(destination_folder, f"{data.m_Name}"+extension)
                with open(path, "wb") as f:
                    f.write(bytes(data.m_FontData))
                f.close()
    print("Extracted assets")

def get_bytes_files_names(decrypt_folder):
    file_names = []
    for file_name in os.listdir(decrypt_folder):
        lowered = file_name.lower()
        if not os.path.isfile(os.path.join(decrypt_folder, file_name)): continue
        if not ((lowered.startswith("card") or lowered.startswith("word")) and lowered.endswith(".bytes")):
            continue
        file_names.append(file_name)
    return file_names

def decrypt_and_split_names_and_descs(split_script_path):
    os.chdir(decrypt_folder)
    subprocess.call(["python", split_script_path])
    os.chdir(script_folder())
    print("Decrypted and split the name and desc files")

def make_braced_json(pidx_table, part_table, descs, save_path):
    braced_descs = [insert_braces(descs[i], part_table[i]) for i in range(len(descs))]
    print("Applied braces")

    #Write new json with braces indicating where effects start and end
    WriteJSON(braced_descs, save_path)
    print("Wrote braced JSON to:", save_path)

    return braced_descs

#Step 0.3: Defs for "name fixer"

def is_section_start(line):
    return line.startswith(section_start)

def is_target_start(line):
    return line.startswith(target_start)

def is_label(line):
    return is_section_start(line) or is_target_start(line)

def get_end_index(index, lines_list):
    for i in range(index, len(lines_list)):
        if not is_label(lines_list[i]): continue
        return i
    raise Exception("[get_end_index]: You're not supposed to reach here; something went TERRIBLY wrong")

'''
#For a later feature [TODO]
#Returns True if you can search the new text with the old text
#Should give it the concatenated name and desc
def fundamentally_changed(before, after):
    before, after = before.lower(), after.lower()
    for token in before.split():
        if token not in after:
            return True
    return False
'''

### APPLYING CHANGES ###

def parse_targeted_signature(section_signature):
    section_signature = section_signature.strip()
    section_args = [x.strip() for x in section_signature.split(target_start)[1:]]
    if len(section_args) == 4:
        section_args[3] = section_args[3][1:-1] #remove the quotes
        section_args[3] = section_args[3]
    expected_lines = 2 if len(section_args) <= 2 else 1
    return section_args, expected_lines

def split_delims(line, name, where):
    has_top_text = pend_stripped_marker in line
    name, where = name.lower(), where.lower()
    if where in ["name", "normaltextbox", "pendulumtextbox"]:
        side_L = side_R = '\"'
        if where == "normaltextbox":
            if has_top_text: side_R = escaped_pend_marker
        elif where == "pendulumtextbox":
            if has_top_text: side_L = escaped_pend_marker
            else: raise Exception("No Pendulum text box found for " + name)
        L = line.index(side_L)+len(side_L)
        R = line.rindex(side_R)

    elif where.startswith("normaleffect"):
        effect_number = int(where.split()[-1])
        effect_index = effect_number-1
        temp_line = line
        if has_top_text: temp_line = line.split(pend_stripped_marker)[0]
        L_brace_inds = get_main_left_brace_indices(temp_line)
        R_brace_inds = get_main_right_brace_indices(temp_line)
        try:
            L = L_brace_inds[effect_index]+1
            R = R_brace_inds[effect_index]
        except:
            print( "Left brace indices for", name+":", L_brace_inds)
            print("Right brace indices for", name+":", R_brace_inds)
            raise Exception("Failed to find effect number "+str(effect_number)+" for "+name)

    elif where.startswith("pendulumeffect"):
        effect_number = int(where.split()[-1])
        effect_index = effect_number-1
        temp_line = line
        if has_top_text: temp_line = line.split(pend_stripped_marker)[1]
        L_brace_inds = get_main_left_brace_indices(temp_line)
        R_brace_inds = get_main_right_brace_indices(temp_line)
        shift = len(line.split(pend_stripped_marker)[0]) + len(pend_stripped_marker)
        L = L_brace_inds[effect_index]+shift+1
        R = R_brace_inds[effect_index]+shift

    else: raise Exception("Unrecognized 'where' name: " + where)

    return line[:L], line[L:R], line[R:]

def apply_targeted_change(line, targeted_args, expected_lines, arg1, arg2=""): #Assume the raw line is given
    delim_L, actual_line, delim_R = split_delims(line, targeted_args[0], targeted_args[1])
    if   expected_lines == 2:
        old_line = actual_line
        new_line = actual_line.replace(arg1, arg2)
        if old_line == new_line:
            print("\nNothing changed for", targeted_args[0], "with the targeted change", targeted_args)
            print("Argument 1:", arg1)
            print("Argument 2:", arg2)
            print("Part of interest:", actual_line)
            print("\n")
        return delim_L + new_line + delim_R
    elif expected_lines == 1:
        action = targeted_args[2].lower()
        if   action == "replace": return delim_L + arg1 + delim_R
        elif action == "prefix" : return delim_L + arg1 + targeted_args[3] + actual_line + delim_R
        elif action == "suffix" : return delim_L + actual_line + targeted_args[3] + arg1 + delim_R
    raise Exception("Wrong number of expected lines: " + str(expected_lines))

def where_desc_valid(where):
    cond1 = where in ["normaltextbox", "pendulumtextbox"]
    cond2 = where.startswith("normaleffect")
    cond3 = where.startswith("pendulumeffect")
    return cond1 or cond2 or cond3

def edit_file(file_path, changes_list, file_type, name_line_index_dict, name_arr):
    file_name = os.path.basename(file_path)
    print("Editing", file_name, "...")
    
    #Read the file
    #It's the wrong way to do it, but it SHOULD be done this way to avoid changing unintended cards
    #(by using the JSON quotes as delimiters)
    with open(file_path, 'r', encoding="utf-8") as file:
        file_lines = file.readlines()
    file.close()

    #Apply changes to read lines
    changed_indices = set() #indices of lines that were changed
    changed_count = [0 for _ in changes_list] #how many lines (names or effects) each entry changed

    start_line_index, no_go_line_index = get_3400_and_30000_ID_line_index()
    for i in range(start_line_index, no_go_line_index):
        line = file_lines[i]
    #for i, line in enumerate(file_lines):
    #    if i >= no_go_line_index: break
        newline = line
        this_cards_name = name_arr[i]
        for j, change_tuple in enumerate(changes_list):
            if len(change_tuple) == 2: #Probably shouldn't do these based on number
                (bfr, aft) = change_tuple
                bfr = bfr.replace(card_name_string, this_cards_name)
                aft = aft.replace(card_name_string, this_cards_name)
                newline = line.replace(bfr, aft)
            elif len(change_tuple) in [3,4]: #Probably shouldn't do these based on number
                (targeted_args, expected_lines, arg1), arg2 = change_tuple[:3], ""
                if len(change_tuple) == 4: arg2 = change_tuple[3] #Probably shouldn't do these based on number
                arg1 = arg1.replace(card_name_string, this_cards_name)
                arg2 = arg2.replace(card_name_string, this_cards_name)
                name  = targeted_args[0]
                where = targeted_args[1].lower()
                name_check = (file_type == "Name") and (where == "name")
                desc_check = (file_type == "Desc") and where_desc_valid(where)
                if (name_check or desc_check) and (i in name_line_index_dict[name]):
                    try:
                        newline = apply_targeted_change(line, targeted_args, expected_lines, arg1, arg2)
                    except:
                        print("Targeted change failed for "+name+" in "+where+" with the arguments:\n"+str(targeted_args)+" "+str(arg1)+" "+str(arg2))
            if line != newline:
                changed_indices.add(i)
                changed_count[j] += 1
            line = newline
        file_lines[i] = line

    #Write applied changes
    with open(file_path, 'w', encoding="utf-8") as file:
        file.writelines(file_lines)
    file.close()

    #print("Done editing")#, file_name, "into !Changed", file_name)
    changed_indices = list(changed_indices)
    changed_indices.sort()
    return changed_indices, changed_count

def get_changes_table(lines_list):
    table = []
    lines_list = lines_list+[section_start] #adding sentinel to make things easy
    index = 0
    
    while index < len(lines_list)-1: #This is AFTER adding the sentinel
        line = lines_list[index]
        
        if is_target_start(line):
            section_signature = line
            section_args, expected_lines = parse_targeted_signature(section_signature)
            
            arg1 = lines_list[index+1].replace(empty_char, "")
            if is_label(arg1):     raise Exception("Targeted change with no 1st argument under: "+section_signature)
            
            if   expected_lines == 1:
                table.append((section_signature, [(section_args, expected_lines, arg1      )])) #Putting it in a list to be compatible with the unpacking; NOT how this should be done
                index += 2
            elif expected_lines == 2:
                arg2 = lines_list[index+2].replace(empty_char, "")
                if is_label(arg2): raise Exception("Targeted change with no 2nd argument under: "+section_signature)
                table.append((section_signature, [(section_args, expected_lines, arg1, arg2)])) #Putting it in a list to be compatible with the unpacking; NOT how this should be done
                index += 3

        else:
            section_name = "Unnamed section"
            if is_section_start(line):
                section_name = line.replace(section_particle, "").strip()
                index += 1
            section_pairs_list = []
            end_index = get_end_index(index, lines_list)
            if (end_index - index) % 2 != 0:
                raise Exception("Odd number of entries in the following section:\n" + section_name)
            for i in range(index, end_index, 2):
                x = lines_list[i  ].replace(empty_char, "")
                y = lines_list[i+1].replace(empty_char, "")
                section_pairs_list.append((x, y))
            table.append((section_name, section_pairs_list))
            index = end_index
            
    return table

def pad(s, n):
    return s+(n-len(s))*" "

'''
#TODO take the targeted change into account
def list_proposed_changes(table, text_file):#CMD width is 211 characters in full-screen, so maybe take that into account
    lim = 64
    change_count = sum(len(pair[1]) for pair in table)
    print("Found the below", change_count, "proposed changes in", text_file, ":\n")
    for section_name, section_pairs in table:
        if section_name:
            print("\t" + section_name + ":")
        for x,y in section_pairs:
            y = y if y else "[[[EMPTY]]]"
            if len(x) <= lim and len(y) <= lim: #Write on the same line if short enough
                print(pad(x, lim)+" --->\t"+y)
            else: #Write on separate lines if either too long
                print(""+x+"\n\t-->\t"+y+"")
        print("\n")
    print("\nFound the above", change_count, "proposed changes in", text_file)
'''

def get_name_line_index_dict(name_file_path):
    name_line_index_dict = {}
    with open(name_file_path, encoding="utf-8") as f:
        lines = f.readlines()
    f.close()
    name_arr = ["" for line in lines]
    for i, line in enumerate(lines):
        if '\"' not in line: continue
        L, R = line.index('\"')+1, line.rindex('\"')
        name_arr[i] = line[L:R]
        name = name_arr[i].replace('\\"', '\"') ######## WARNING: THIS UN-ESCAPES THE QUOTES FROM THE JSON; THERE COULD BE OTHER THINGS TO ESCAPE IN THE FUTURE
        if name in name_line_index_dict:
            name_line_index_dict[name].append(i)
        else:
            name_line_index_dict[name] = [i] #Doing it with arrs because of alt arts having the same name
    return name_line_index_dict, name_arr

def apply_changes(change_name_path, change_desc_path, text_file, name_line_index_dict, name_arr, choice=""):
    #Read the changes
    with open(text_file, encoding="utf-8") as changes_file: #Ignore comments and empty lines
        main_lines_list = [x.strip() for x in changes_file.readlines() if not (x.strip().startswith(comment_start) or x.strip() == "")]
    changes_file.close()

    #Initial processing
    changes_table = get_changes_table(main_lines_list)
    changes_list = []
    for section_name,section_pairs_list in changes_table:
        changes_list.extend(section_pairs_list)

    #List the proposed changes in the terminal
    #list_proposed_changes(changes_table, text_file)

    #Apply changes (or not)
    print("Doing proposed changes from " + os.path.basename(file_path))
    '''
    while choice not in ['y','n']:
        choice = input("\nApply them? [Y/N]\n").lower()

    if choice != 'y':
        print("Changes not applied")
        return
    '''
    
    names_changed_indices, names_changed_count = edit_file(change_name_path, changes_list, "Name", name_line_index_dict, name_arr)
    print(len(names_changed_indices), "names changed")
    descs_changed_indices, descs_changed_count = edit_file(change_desc_path, changes_list, "Desc", name_line_index_dict, name_arr)
    print(len(descs_changed_indices), "card descriptions changed")

    #Write out amount each entry changed, maybe with card ID's
    #(but NOT which lines were changed, since order of lines gets changed by Konami when new cards are added)[TODO]
    #Call those "!Delta " files
    #Later create a function to compare !Delta files to see if there were more cards affected

### PART FILE STUFF ###

#Takes list of pairs and makes them into one SORTED list
def concatenated_pairs(pairs_list):
    ans = []
    for a,b in pairs_list:
        if a == b: continue
        ans.extend([a,b])
    ans.sort()
    return ans

def get_diff(arr_old, arr_new): #Assume they're sorted
    if arr_old == arr_new: return None
    return [arr_old[i]-arr_new[i] for i in range(max(len(arr_old),len(arr_new)))]

def make_map(i, part_table, braced_descs, changed_braced_descs):
    unbraced_arr = concatenated_pairs(part_table[i])
    braced_arr = unbraced_brace_indices(braced_descs[i])
    arr_dif = get_diff(unbraced_arr, braced_arr)
    changed_braced_arr = unbraced_brace_indices(changed_braced_descs[i])
    ans = {}
    if arr_dif:
        for j in range(max(len(arr_dif), len(changed_braced_arr))):
            changed_braced_arr[j] += arr_dif[j]
    for j in range(max(len(unbraced_arr), len(changed_braced_arr))):
        ans[unbraced_arr[j]] = changed_braced_arr[j]
    return ans

def apply_map(part_arr, part_map):
    ans = []
    f = part_map
    for (a,b) in part_arr:
        if a >= b:
            ans.append((a,b))
            continue
        ans.append((f[a], f[b]))
    return ans

def adjust_part_table(part_table, braced_descs, changed_braced_descs):
    part_table = [part_arr for part_arr in part_table] #Just copying
    for i in range(len(part_table)):
        part_map = make_map(i, part_table, braced_descs, changed_braced_descs)
        part_table[i] = apply_map(part_table[i], part_map)
    return part_table

def make_changed_part_file(changed_part_path, part_table, braced_descs, changed_braced_descs):
    part_table = adjust_part_table(part_table, braced_descs, changed_braced_descs)
    write_part_file(changed_part_path, part_table)
    print("Made !Changed Card_Part.bytes.dec")

def make_unbraced_changed(changed_braced_path, unbraced_changed_path):
    with open(changed_braced_path, 'r', encoding='utf-8') as file:
        lines = file.readlines()
    file.close()
    for i,line in enumerate(lines):
        lines[i] = line.replace('{','').replace('}','')
    with open(unbraced_changed_path, 'w', encoding='utf-8') as file:
        file.writelines(lines)
    file.close()
    print("Made !Unbraced !Changed CARD_Desc.bytes.dec.json")

### NEW REGEX STUFF ###

def get_replacement_line(desc_line, regex, repl, matchcase):
    matches = [x for x in re.finditer(regex, desc_line)]
    for i in range(len(matches)-1, -1, -1):
        Match = matches[i]
        start,end = Match.span()
        start_char = desc_line[start]
        desc_line = desc_line[:start] + re.sub(regex, repl, desc_line[start:end]) + desc_line[end:]
        if matchcase:
            if start_char.isupper():
                desc_line = desc_line[:start] + desc_line[start].upper() + desc_line[start+1:]
            elif start_char.islower():
                desc_line = desc_line[:start] + desc_line[start].lower() + desc_line[start+1:]
    return desc_line

'''
def parse_regex_file(regex_path):
    with open(regex_path, encoding="utf-8") as f: #Skip empty lines and comments
        regex_lines = [x.strip() for x in f.readlines() if not (x.strip().startswith(comment_start) or x.strip() == "")]
    f.close()
    regex_lines = regex_lines+[section_start] #adding sentinel to make things easy

    table = []
    index = 0
    while index < len(regex_lines)-1: #This is AFTER adding the sentinel
        line = regex_lines[index]
        section_name = "Unnamed section"
        if is_section_start(line):
            section_name = line.replace(section_particle, "").strip()
            index += 1
        section_trips_list = []
        end_index = get_end_index(index, regex_lines)
        if (end_index - index) % 3 != 0:
            raise Exception("Number of entries not divisible by 3 in the following section:\n" + section_name)
        for i in range(index, end_index, 3):
            x = regex_lines[i  ].replace(empty_char, "")
            y = regex_lines[i+1].replace(empty_char, "")
            z = regex_lines[i+2].replace(empty_char, "")
            section_trips_list.append((x,y,z))
        table.append((section_name, section_trips_list))
        index = end_index
    return table

def apply_regex_new(change_desc_path, regex_path, name_arr, approve_choice=""):
    #Get the regex changes
    regex_table = parse_regex_file(regex_path)
    regex_list = []
    for section_name, section_trips_list in regex_table:
        regex_list.extend(section_trips_list)

    #Should they be applied?
    print("\nDoing proposed changes from " + os.path.basename(regex_path))
    while approve_choice not in ['y','n']:
        approve_choice = input("\nApply them? [Y/N]\n").lower()
    if approve_choice != 'y':
        print("Changes not applied")
        return

    #Read the desc file
    print("Editing", os.path.basename(change_desc_path), "...")
    with open(change_desc_path, 'r', encoding="utf-8") as file:
        desc_arr = file.readlines()
    file.close()
    desc_arr_copy = [x for x in desc_arr]

    for i in range(len(desc_arr)):
        name = name_arr[i]
        for (before, after, match_case) in regex_list: #matchcase can be changed to "flags" later if needed
            before = before.replace(card_name_string, name)
            after  = after.replace(card_name_string, name)
            match_case = match_case.lower() == "matchcase"
            desc_arr[i] = get_replacement_line(desc_arr[i], before, after, match_case)
        while ',,' in desc_arr[i]:
            desc_arr[i] = desc_arr[i].replace(',,', ',') #ad hoc; should be done a better way
        desc_arr[i] = desc_arr[i].replace(',.', '.') #ad hoc; should be done a better way
    with open(change_desc_path, 'w', encoding="utf-8") as file:
        file.writelines(desc_arr)
    file.close()
    print(sum(desc_arr[i] != desc_arr_copy[i] for i in range(len(desc_arr))), "card descriptions changed")
'''

# Step 0.3p5: Defs for font size adjuster for readability mod

def wrap_text(text, font, max_width):
    lines = []
    main_lines = text.split('\n')
    for subline in main_lines:
        if len(subline) == 0:
            lines.append(subline)
            continue
        words = subline.split()
        current_line = ""
        for word in words:
            test_line = current_line + " " + word if current_line else word
            width = font.getbbox(test_line)[2]
            if width <= max_width:
                current_line = test_line
            else:
                lines.append(current_line)
                current_line = word
        if current_line:
            lines.append(current_line)
    return lines

def find_max_font_size_multiline(text, font_sizes_nums, max_width, max_height, has_type_line, shrink_factor=1.0, shift_down=0, type_line_factor=0.11):
    def text_fits(font):
        lines = wrap_text(text, font, max_width)
        line_height = font.getbbox("A")[3]*(shrink_factor if has_type_line else 1) #get line height
        total_height = line_height * (len(lines) + type_line_factor*has_type_line) #type line is bigger
        return (total_height <= max_height), len(lines)

    best_size_idx = min_size_idx = 0
    max_size_idx = len(font_sizes_nums)-1
    line_counts_list = [0]*len(font_sizes_nums)

    while min_size_idx <= max_size_idx:
        mid_idx = (min_size_idx + max_size_idx) // 2
        fits, line_counts_list[mid_idx] = text_fits(font_sizes_nums[mid_idx][1])
        if fits:
            best_size_idx = mid_idx
            min_size_idx = mid_idx + 1
        else:
            max_size_idx = mid_idx - 1
    ans = font_sizes_nums[best_size_idx][0]
    return ans - shift_down

def clean_text(text):
    removal_symbols = ["<i>", "</i>", "<b>", "</b>", "<u>", "</u>", "</rotate>", "</color>"]
    for symbol in removal_symbols:
        text = text.replace(symbol, "")
    removal_patterns = ['<rotate=\\"[0-9]+\\">', "<color=([a-zA-Z]+|#[a-fA-F0-9]{6})>"]
    for pattern in removal_patterns:
        text = re.sub(pattern, "", text)
    return text

#box_type: 0 for main box, 1 for pend box
def get_correct_font_size(text, font_sizes_nums, box_type=0, has_type_line=True):
    text = clean_text(text)
    #have to figure out through the Card_Prop file if a card is a spell/trap
    if has_type_line: text = "[TYPE LINE]\n" + text
    val = 0
    if box_type == 0:
        val = find_max_font_size_multiline(text, font_sizes_nums, 562, round(131 if has_type_line else 152), has_type_line) #accounting for stat line, NOT THE TYPE LINE
    elif box_type == 1:
        val = find_max_font_size_multiline(text, font_sizes_nums, 450, 92, False)
    return min(28, val)
    
def norm_index_to_pend_index_dict(norm_font, pend_font):
    ID_to_pend_font_index = {pend_font[i]['cardid']:i       for i in range(len(pend_font))}
    return {i:ID_to_pend_font_index[norm_font[i]['cardid']] for i in range(len(norm_font)) if norm_font[i]['cardid'] in ID_to_pend_font_index}

'''Prop stuff'''

def zero_pad(s, n):
    return (n-len(s))*"0"+s

def get_bit_range(bits, frm, to):
    return zero_pad(bits,64)[64-to:64-frm]

def get_prop_list():
    with open(prop_dec_path, 'rb') as file:
        prop_bytes = file.read()
    prop_list = [zero_pad(bin(int.from_bytes(prop_bytes[i:i+8], byteorder='little'))[2:], 64) for i in range(8,len(prop_bytes),8)]
    return prop_list

#bits [22:26) are the attribute bits, and the values 8 and 9 are spells and traps (respectively)
def is_backrow(prop_bit_string):
    return int(get_bit_range(prop_bit_string,22,26), 2) in [8,9]

def is_pend(prop_bit_string): #Might need to adjust the list with more Pends in the future; probably should put it in a config
    return int(get_bit_range(prop_bit_string,16,22), 2) in [25,26,33,35,35,36,40,41,44,45,52]

def remove_nums(text):
    for c in "⓪①②③④⑤⑥⑦⑧⑨⑩":
        text = text.replace(c+' ', '')
    return text

def get_changed_lines_indices(post_change_arr, ignore_if_only_numbered=True):
    #Read the pre- and post- change desc files as jsons
    with open(desc_dec_path, encoding="utf-8") as pre_change_desc_json:
        pre_change_arr  = json.load(pre_change_desc_json)
    pre_change_desc_json.close()
    changed_norm_lines_indices = []
    changed_pend_lines_indices = []
    for i in range(len(pre_change_arr)):
        #If I'm ignoring the effects that only got numbers added to them, then just remove the numbers and compare
        if ignore_if_only_numbered and (remove_nums(pre_change_arr[i]) == remove_nums(post_change_arr[i])):
            continue
        #if pre_change_arr[i] == post_change_arr[i]: continue #Change 1/3
        if unescaped_pend_marker in pre_change_arr[i]:
            pre_bottom_text ,  pre_top_text =  pre_change_arr[i].split(unescaped_pend_marker)
            post_bottom_text, post_top_text = post_change_arr[i].split(unescaped_pend_marker)

            if ignore_if_only_numbered:
                post_bottom_text = remove_nums(post_bottom_text)
                post_top_text    = remove_nums(post_top_text   )
            
            if True: #pre_bottom_text != post_bottom_text: #Change 2/3
                changed_norm_lines_indices.append(i)
            if True: #pre_top_text    != post_top_text   : #Change 3/3
                changed_pend_lines_indices.append(i)
        else:
            changed_norm_lines_indices.append(i)
    return changed_norm_lines_indices, changed_pend_lines_indices, pre_change_arr

def get_prop_file():
    with open(prop_dec_path, 'rb') as file:
        prop = file.read()
    file.close()
    prop = [bin(int.from_bytes(prop[i:i+8], byteorder='little'))[2:] for i in range(0,len(prop),8)]
    prop = [zero_pad(x,64) for x in prop[1:]]
    return prop

def fix_precision(norm_font, pend_font):
    for i in range(len(norm_font)):
        norm_font[i]["fontsize"] = round(norm_font[i]["fontsize"], 2)
    for j in range(len(pend_font)):
        pend_font[j]["fontsize"] = round(pend_font[j]["fontsize"], 2)

def should_be_resized(old_text, new_text): #the new text is longer or has more lines
    return ((new_text.count('\n') > old_text.count('\n')) or (len(new_text) > len(old_text))) or True #remove this "or True"

def get_pickle_path(name):
    return os.path.join(common_defs.scripts_folder, name+".pickle")

def id_to_index_dict(font_list):
    return {font_list[i]['cardid']:i for i in range(len(font_list))}

def apply_loaded_fonts(loaded_font_list, font_list):
    id_idx = id_to_index_dict(font_list)
    loaded_indices_list = [False]*len(font_list)
    for entry in loaded_font_list:
        if entry['cardid'] not in id_idx:
            continue
        idx = id_idx[entry['cardid']]
        font_list[idx] = entry
        loaded_indices_list[idx] = True
    return loaded_indices_list

'''Main loop defs'''

def calc_font_sizes(
prop_range_start,
prop_range_end,
prop,
prop_len,
bool_main_changed_indices,
bool_pend_changed_indices,
post_change_arr,
pre_change_arr,
unescaped_pend_marker,
norm_font,
pend_font,
norm_to_pend_index,
font_sizes_nums,
processed_cards
):
    for i in range(prop_range_start, prop_range_end):
        
        #If nothing changed: Continue
        if not (bool_main_changed_indices[i] or bool_pend_changed_indices[i]): continue
        #if loaded_norm_indices_list[i]: continue

        #New and old texts
        full_text = post_change_arr[i]
        OLD_text  =  pre_change_arr[i]
        
        has_top_text = unescaped_pend_marker in full_text
        if has_top_text:
            bottom_text, top_text   = full_text.split(unescaped_pend_marker)
            OLD_b_text , OLD_t_text =  OLD_text.split(unescaped_pend_marker)

        #If it doesn't have a Pendulum effect
        if (not has_top_text) and should_be_resized(OLD_text, full_text):
            guess = get_correct_font_size(full_text, font_sizes_nums, 0, not is_backrow(prop[i]))

            temp = norm_font[i]
            temp.pop("fontsize")
            temp["fontsize"] = guess
            norm_font[i] = temp
            
        #If it has a Pendulum effect
        if has_top_text and should_be_resized(OLD_b_text, bottom_text):
            guess = get_correct_font_size(bottom_text, font_sizes_nums, 0, True)

            temp = norm_font[i]
            temp.pop("fontsize")
            temp["fontsize"] = guess
            norm_font[i] = temp
            
        #If it has a Pendulum effect
        if has_top_text and should_be_resized(OLD_t_text, top_text):
            guess = get_correct_font_size(top_text, font_sizes_nums, 1, False)

            temp = pend_font[norm_to_pend_index[i]]
            temp.pop("fontsize")
            temp["fontsize"] = guess
            pend_font[norm_to_pend_index[i]] = temp  
    
        processed_cards.value += 1
        sys.stdout.flush()
        sys.stdout.write('{:5.1f}%\r'.format(processed_cards.value / prop_len * 100))    
    #return "Done with thread "+str(prop_range_start)

def calc_font_sizes_multiprocessing():
  
    #Initialize shared memory variables
    prop_range_start = mp.Value(c_int, 0) #segment start boundary
    prop_range_end = mp.Value(c_int, 0) #segment end boundary
    processed_cards = mp.Value(c_int, 0) #for status info
 
    core_count = os.cpu_count() or 1 #use all available logical CPU cores

    segment = prop_len // core_count #define the size of each segment by floor dividing the total item number by the number of CPU cores
    processes = []

    for i in range(core_count):
        prop_range_start = i * segment
        if i == core_count - 1:
            prop_range_end = prop_len  # Ensure the last segment goes up to the end
        else:
            prop_range_end = prop_range_start + segment
        # Creating a process for each segment        
        process = mp.Process(target=calc_font_sizes, args=(
        prop_range_start,
        prop_range_end,
        prop,
        prop_len,
        bool_main_changed_indices,
        bool_pend_changed_indices,
        post_change_arr,
        pre_change_arr,
        unescaped_pend_marker,
        norm_font,
        pend_font,
        norm_to_pend_index,
        font_sizes_nums,
        processed_cards
        ))

        processes.append(process)
        process.start()
        
    for process in processes:
        process.join()
        
    return norm_font,pend_font

#Step 0.4: Defs for "mod the files"

#####################################################
### ALMOST EVERYTHING IN  THIS FILE IS HARD-CODED ###
### THIS SHOULD PROBABLY BE CHANGED IN THE FUTURE ###
#####################################################

#This Python script's location
def script_folder():
    return os.path.dirname(os.path.abspath(__file__))

def encrypt_and_merge_names_and_descs(merge_script_path, modded_folder):
    os.chdir(modded_folder)
    subprocess.call(["python", merge_script_path])
    os.chdir(script_folder())
    print("Encrypted and merged the name and desc files")

#################
# Main Function #
#################

if __name__ == '__main__':
    start_time = time.time()
    print("Script started at", datetime.fromtimestamp(start_time).strftime("%Y-%m-%d %H:%M:%S"))
    
    #Global variables
    nul = b'\x00'
    pend_stripped_marker = ""

    #Optional supplied directory (first CLI arg). Searched before installed game files.
    supplied_dir = None
    if len(sys.argv) > 1 and sys.argv[1].strip():
        candidate = os.path.abspath(sys.argv[1])
        if os.path.isdir(candidate):
            supplied_dir = candidate
            print("Using supplied directory (searched before installed game files):", supplied_dir)
        else:
            print("Warning: supplied path is not a directory; ignoring:", candidate)

    #Shorten multiprocessing terms
    mp_context = mp.get_context('spawn')
    manager = mp_context.Manager()
    
    #Create and clear input and output folders
    os.makedirs(copied_files_folder, exist_ok=True)
    os.makedirs(modded_folder, exist_ok=True)
    clear_folder(copied_files_folder)
    clear_folder(modded_folder)
    
    # Step 1: game file getter

    path_0000, search_quadruples_list = get_config_data()
    print("Read config data")    

    #os.chdir(os.path.join(path_0000,os.pardir,os.pardir,os.pardir,'masterduel_Data/StreamingAssets/AssetBundle'))
    #path_AssetBundle = os.getcwd()
    path_AssetBundle = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(path_0000))),'masterduel_Data/StreamingAssets/AssetBundle')
    
    '''
    #Determine total number of files in bundle folders
    total_files = 0    
    for _, _, files in os.walk(path_0000):
        total_files += len(files)    
    print("\nFound", total_files, "files in 0000")
    for _, _, files in os.walk(path_AssetBundle):
        total_files += len(files)
    print("Found", total_files, "files in AssetBundle\n")
    '''
    
    search_results = multi_search(search_quadruples_list)
    print("Found", len([x for x in search_results if x]), "files out of", len(search_quadruples_list),"\n")
    
    update_config(search_quadruples_list, search_results)
    print("Updated config file")
      
    copy_game_files(search_results)

    #Step 2: asset decryptor

    #Make decrypt folder    
    os.makedirs(decrypt_folder, exist_ok=True)

    #Extract the assets to decrypt folder
    unpack_all_assets(copied_files_folder, decrypt_folder)

    #Copy the decryption scripts there too
    script_names = ["_CARD_decrypt_Desc+Indx+Name_and_split_Desc+Name.py",
                    "_CARD_decrypt.py",
                    "!CryptoKey.txt"]
    copy_scripts_to_folder(scripts_folder, script_names, decrypt_folder)

    #Decrypt and split the name and desc files
    split_script_path = os.path.join(decrypt_folder, script_names[0])
    decrypt_and_split_names_and_descs(split_script_path)

    #Decrypt the other stuff
    decrypt_script_path = os.path.join(decrypt_folder, script_names[1])
    todo_bytes = [file_name for file_name in get_bytes_files_names(decrypt_folder) if file_name.lower() not in ["card_desc.bytes", "card_indx.bytes", "card_name.bytes"]]
    os.chdir(decrypt_folder)
    run_script_on_other_files(decrypt_script_path, todo_bytes, decrypt_folder)
    os.chdir(script_folder())

    #Remove the copied scripts
    silent_remove(decrypt_script_path)
    silent_remove(split_script_path)
    shutil.move(os.path.join(decrypt_folder, script_names[2]), os.path.join(scripts_folder, script_names[2]))

    #Get Part and Pidx tables
    pidx_table = get_pidx_table(pidx_dec_path)
    part_table = get_part_table(part_dec_path, pidx_table)

    #Get JSON
    descs = ReadJSON(desc_dec_path)

    #Brace and write the JSON
    braced_descs = make_braced_json(pidx_table, part_table, descs, braced_save_path)

    #Step 3: name fixer

    section_particle = "%" #WARNING: Removes ALL % signs, not just the ones at the start
    section_start = "%%%"
    target_start  = ":::"
    comment_start = "###"
    empty_char = "_" #Underscores become empty
    card_name_string = "CARDNAME"
    text_file_name  = "!TextChange"
    #regex_file_name = "!RegexChange"
    pend_stripped_marker = get_pend_stripped_marker()
    escaped_pend_marker = get_escaped_pend_marker()

    #Get the change files' paths and sort them
    change_files_list = [os.path.join(change_list_folder, filename) for filename in os.listdir(change_list_folder)
                         if (filename.startswith(text_file_name) 
                         #or filename.startswith(regex_file_name)
                         )]
    #change_files_list.sort(key=lambda x: x.replace(regex_file_name, "").replace(text_file_name, ""))
    change_files_list.sort(key=lambda x: x.replace(text_file_name, ""))    

    #List the files found
    print("Found the following files:")
    for file_path in change_files_list:
        print(os.path.basename(file_path))

    #Approve all
    choice = ""
    print("\nApproving all changes...\n")
    approve_all = 'y'
    
    #Create the output folder and put the name and desc json files in it
    os.makedirs(changed_folder, exist_ok=True)
    change_name_path = os.path.join(changed_folder, "!Changed CARD_Name.bytes.dec.json")
    change_desc_path = os.path.join(changed_folder, "!Changed !Braced CARD_Desc.bytes.dec.json") #Only apply to the braced version
    copy_and_replace(name_dec_path   , change_name_path)
    copy_and_replace(braced_save_path, change_desc_path)
    copy_and_replace(font_asset_path , changed_fontsize_path)
    copy_and_replace(os.path.join(scripts_folder, "!Changed YGO_Card_NA.ttf"), changed_font_path)

    #Start applying changes
    name_line_index_dict, name_arr = get_name_line_index_dict(name_dec_path)
    for file_path in change_files_list:
        if os.path.basename(file_path).startswith(text_file_name):
            apply_changes(change_name_path, change_desc_path, file_path, name_line_index_dict, name_arr, choice=approve_all)
        #elif os.path.basename(file_path).startswith(regex_file_name):
            #apply_regex_new(change_desc_path, file_path, name_arr, approve_choice=approve_all)

    '''
    #Add nomi subtype if the user chooses to        
    choice = ''
    while choice not in ['y','n']:
        choice = input("\n\nAdd nomi subtype? [Y/N]\n").lower()
    
    word_table = get_word_table()
    if choice == 'y':
        add_nomi(word_table)
        print("Added nomi")
    else:
        print("Did not add nomi")
    
    write_word_table(word_table)
    '''
    
    #Get Part and Pidx tables
    pidx_table = get_pidx_table(pidx_dec_path)
    part_table = get_part_table(part_dec_path, pidx_table)

    #Make desc with no braces (after applying changes)
    make_unbraced_changed(changed_braced_path, unbraced_changed_path)

    #Make the part file for the changed descs, then remove the "!Changed !Braced CARD_Desc.bytes.dec.json"
    braced_descs = ReadJSON(braced_save_path)
    changed_braced_descs = ReadJSON(changed_braced_path)
    make_changed_part_file(changed_part_path, part_table, braced_descs, changed_braced_descs)
    #silent_remove(changed_braced_path) 

    # Step 3p5: font size adjuster for readability mod

    unescaped_pend_marker = get_unescaped_pend_marker()

    #yugioh's effect font's path in utils       
    font_path = os.path.join(scripts_folder, "!Changed YGO_Card_NA.ttf") #Modded font

    #Hard-coded to go from 11 to 28 (inclusive) by steps of 0.05 ((28-11)/0.05 + 1 = 341 fonts)
    font_sizes_nums_backup = [round(s/20, 2)  for s in range(11*20, 28*20 +1)]
    font_sizes_nums_backup = [(s, ImageFont.truetype(font_path, s)) for s in font_sizes_nums_backup]
    font_sizes_nums = manager.list(font_sizes_nums_backup)
    
    #Get the current font size arrays
    json_data = ReadJSON(font_asset_path)
    norm_font_backup = json_data['m_CardidNormalFontSizePairDatas']
    norm_font = manager.list(norm_font_backup)
    pend_font_backup = json_data['m_CardidPendulumFontSizePairDatas']
    pend_font = manager.list(pend_font_backup)

    #Make a dict that makes it easy to change the pend array
    norm_to_pend_index = manager.dict(norm_index_to_pend_index_dict(norm_font, pend_font))
    
    #Don't ignore text boxes that only got effect numbers and nothing else
    ignore_if_only_numbered = 'n' == 'y'

    #Indices of the changed cards
    with open(unbraced_changed_path, encoding="utf-8") as post_change_desc_json:
        post_change_arr_backup = json.load(post_change_desc_json)
    post_change_desc_json.close()
    post_change_arr = manager.list(post_change_arr_backup)

    changed_norm_lines_indices, changed_pend_lines_indices, pre_change_arr_backup = get_changed_lines_indices(post_change_arr, ignore_if_only_numbered)
    pre_change_arr = manager.list(pre_change_arr_backup)

    #Read the prop file
    prop = get_prop_list()
    prop_len = len(prop)
    prop_range = range(prop_len)

    bool_main_changed_indices_backup = [False for _ in prop_range] #Probably shouldn't use prop for length
    for index in changed_norm_lines_indices:
        bool_main_changed_indices_backup[index] = True
    bool_main_changed_indices = manager.list(bool_main_changed_indices_backup)

    bool_pend_changed_indices_backup = [False for _ in prop_range] #Probably shouldn't use prop for length
    for index in changed_pend_lines_indices:
        bool_pend_changed_indices_backup[index] = True
    bool_pend_changed_indices = manager.list(bool_pend_changed_indices_backup)

    print("\nGot indices of changed cards:")
    print(len(changed_norm_lines_indices), "normal text boxes")
    print(len(changed_pend_lines_indices), "pendulum text boxes")

    #Calculate the new font size for each card's box(es)
    print("\nCalculating font sizes...")
    start_time_calc_font_sizes = time.time()
    print("Started at", datetime.fromtimestamp(start_time_calc_font_sizes).strftime("%Y-%m-%d %H:%M:%S"))

    norm_font,pend_font = calc_font_sizes_multiprocessing()

    end_time_calc_font_sizes = time.time()
    print("Finished at", datetime.fromtimestamp(end_time_calc_font_sizes).strftime("%Y-%m-%d %H:%M:%S"))
    print("Calculated font sizes, took", round((end_time_calc_font_sizes - start_time_calc_font_sizes) / 60, 2), "minutes")

    #Exporting the assets with UnityPy messes up the precision, so this fixes it
    fix_precision(norm_font, pend_font)

    #Copy calculated font sizes to JSON variable
    for i in range(len(json_data['m_CardidNormalFontSizePairDatas'])):
        json_data['m_CardidNormalFontSizePairDatas'][i] = norm_font[i]
    for i in range(len(json_data['m_CardidPendulumFontSizePairDatas'])):
        json_data['m_CardidPendulumFontSizePairDatas'][i] = pend_font[i]
 
    #Write JSON variable to file
    WriteJSON(json_data, changed_fontsize_path)
    print("\nWrote JSON variable to file")

    #Step 4: mod the files

    #Make the mod files folder    
    os.makedirs(modded_folder, exist_ok=True)

    #Copy the changed assets to the mod folder    
    for file_name in os.listdir(changed_folder):
        if file_name == "!Changed !Braced CARD_Desc.bytes.dec.json": continue #skip this one
        new_filename = file_name.replace("!Changed ", "").replace("!Unbraced ", "")
        copy_and_replace(os.path.join(changed_folder,file_name), os.path.join(modded_folder,new_filename))

    #Copy the encryption scripts there
    script_names = ["_CARD_merge+calc_index.py",
                    "_CARD_encrypt.py",
                    "!CryptoKey.txt"]
    copy_scripts_to_folder(scripts_folder, script_names, modded_folder)

    ### Name/Desc/Indx ###

    #Copy name/indx/desc byte files (NOT .dec files) to the modded folder
    copy_and_replace(os.path.join(decrypt_folder,"CARD_Name.bytes"), os.path.join(modded_folder,"CARD_Name.bytes"))
    copy_and_replace(os.path.join(decrypt_folder,"CARD_Desc.bytes"), os.path.join(modded_folder,"CARD_Desc.bytes"))
    copy_and_replace(os.path.join(decrypt_folder,"CARD_Indx.bytes"), os.path.join(modded_folder,"CARD_Indx.bytes"))

    #run the merge+calc script
    merge_script_path = os.path.join(modded_folder,script_names[0])
    encrypt_and_merge_names_and_descs(merge_script_path, modded_folder)

    #Delete unnecessary files
    silent_remove(os.path.join(modded_folder, "CARD_Indx.bytes.dec"))
    silent_remove(os.path.join(modded_folder, "CARD_Name.bytes.dec.json"))
    silent_remove(os.path.join(modded_folder, "CARD_Desc.bytes.dec.json"))
    silent_remove(merge_script_path)

    #Encrypt the Part and WORD files
    copy_and_replace(os.path.join(decrypt_folder,"Card_Part.bytes"), os.path.join(modded_folder,"Card_Part.bytes"))
    copy_and_replace(os.path.join(decrypt_folder,"WORD_Indx.bytes"), os.path.join(modded_folder,"WORD_Indx.bytes"))
    copy_and_replace(os.path.join(decrypt_folder,"WORD_Text.bytes"), os.path.join(modded_folder,"WORD_Text.bytes"))

    #Run the encryption script
    encrypt_script_path = os.path.join(modded_folder,script_names[1])
    os.chdir(modded_folder)    
    run_script_on_other_files(encrypt_script_path, ["Card_Part.bytes.dec","WORD_Indx.bytes.dec","WORD_Text.bytes.dec"], modded_folder)
    os.chdir(script_folder())

    #Delete unnecessary files
    silent_remove(os.path.join(modded_folder, "Card_Part.bytes.dec"))
    silent_remove(os.path.join(modded_folder, "WORD_Indx.bytes.dec"))
    silent_remove(os.path.join(modded_folder, "WORD_Text.bytes.dec"))

    silent_remove(encrypt_script_path)
    shutil.move(os.path.join(modded_folder, script_names[2]), os.path.join(scripts_folder, script_names[2]))

    copy_and_replace(os.path.join(modded_folder,"Card_Part.bytes.dec.enc"), os.path.join(modded_folder,"Card_Part.bytes"))
    copy_and_replace(os.path.join(modded_folder,"WORD_Indx.bytes.dec.enc"), os.path.join(modded_folder,"WORD_Indx.bytes"))
    copy_and_replace(os.path.join(modded_folder,"WORD_Text.bytes.dec.enc"), os.path.join(modded_folder,"WORD_Text.bytes"))

    silent_remove(os.path.join(modded_folder, "Card_Part.bytes.dec.enc"))
    silent_remove(os.path.join(modded_folder, "WORD_Indx.bytes.dec.enc"))
    silent_remove(os.path.join(modded_folder, "WORD_Text.bytes.dec.enc"))

    #Get the names and paths of the files as a list of pairs
    asset_files_list = []
    for root, dirs, files in os.walk(modded_folder):
        for file_name in files:
            file_path = os.path.join(root, file_name)
            file_name_no_ext = os.path.splitext(file_name)[0]
            asset_files_list.append((file_name_no_ext, file_path))

    #Copy the game files in the modded folder
    shutil.copytree(copied_files_folder, modded_folder, dirs_exist_ok=True)
    
    bundle_folder_name_list = ["0000","AssetBundle"]
    for i in range(len(bundle_folder_name_list)):
        for file_path in file_walker(os.path.join(modded_folder,bundle_folder_name_list[i])):
            env = UnityPy.load(file_path)
            for obj in env.objects:
                data = obj.read()
                found = False
                for (asset_name, asset_path) in asset_files_list:
                    if asset_name.lower() != data.m_Name.lower(): continue
                    found = True
                    print(data.m_Name, obj.type.name)
                    if obj.type.name == "TextAsset":
                        with open(asset_path, 'rb') as f:
                           data.m_Script = f.read().decode("utf-8", "surrogateescape")
                        f.close()
                        data.save()
                    elif obj.type.name == "MonoBehaviour":
                        #with open(os.path.join(modded_folder, "CardPictureFontSetting.json"), encoding="utf-8") as g:
                        #    json_data = json.load(g)
                        ReadJSON(asset_path)
                        data.m_CardidNormalFontSizePairDatas   = json_data['m_CardidNormalFontSizePairDatas']
                        data.m_CardidPendulumFontSizePairDatas = json_data['m_CardidPendulumFontSizePairDatas']
                        data.save()
                    elif obj.type.name == "Font":
                        with open(asset_path, 'rb') as f:
                            data.m_FontData = f.read()
                        f.close()
                        data.save()
                    else:
                        found = False
                    with open(file_path, "wb") as f:
                        f.write(env.file.save(packer="none"))
                    f.close()
                if found: break

    #Delete unnecessary files
    silent_remove(os.path.join(modded_folder, "CARD_Name.bytes"))
    silent_remove(os.path.join(modded_folder, "CARD_Indx.bytes"))
    silent_remove(os.path.join(modded_folder, "CARD_Desc.bytes"))
    silent_remove(os.path.join(modded_folder, "Card_Part.bytes"))
    silent_remove(os.path.join(modded_folder, "WORD_Indx.bytes"))
    silent_remove(os.path.join(modded_folder, "WORD_Text.bytes"))
    silent_remove(os.path.join(modded_folder, "CardPictureFontSetting.json"))
    silent_remove(os.path.join(modded_folder, "YGO_Card_NA.ttf"))    

    end_time = time.time()
    print("Script finished at", datetime.fromtimestamp(end_time).strftime("%Y-%m-%d %H:%M:%S"))
    print("Took", round((end_time-start_time)/60,2), "minutes")

    if sys.stdin.isatty():
        input("\nPress Enter to close...")
