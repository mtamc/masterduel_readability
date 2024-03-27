'''
Credit:
timelic from NexusMods: https://forums.nexusmods.com/index.php?/user/145588218-timelic

Original source:
https://bitbucket.org/timel/master-duel-chinese-translation-patch/src/master/%E5%8D%A1%E7%89%87CARD/b_CARD%E6%B8%90%E8%BF%9B%E5%A4%84%E7%90%86.py
'''

from typing import List
import json

def WriteJSON(l: list, json_file_path: str):
    with open(json_file_path, 'w', encoding='utf8') as f:
        json.dump(l, f, ensure_ascii=False, indent=4)

file_names = [
    #  'CARD_Name',
    'CARD_Desc'
]

# The start of Name and Desc is 0 and 4 respectively
def ProgressiveProcessing(file_name, start):

    # Read binary index
    with open(f'CARD_Indx.bytes.dec', "rb") as f:
        hex_str_list = ("{:02X}".format(int(c))
                        for c in f.read())  # Define variables to accept file contents
    dec_list = [int(s, 16) for s in hex_str_list]  # Convert hexadecimal to decimal

    # Get the index of Desc
    indx = []
    for i in range(start, len(dec_list), 8):
        tmp = []
        for j in range(4):
            tmp.append(dec_list[i + j])
        indx.append(tmp)

    def fourToOne(x: List[int]) -> int:
        res = 0
        for i in range(3, -1, -1):
            res *= 16 * 16
            res += x[i]
        return res

    indx = [fourToOne(i) for i in indx]
    indx = indx[1:]
    	
    # Convert Decrypted CARD files to JSON files    
    def solve(data: bytes, desc_indx: List[int]):
        res = []
        for i in range(len(desc_indx) - 1):
            s = data[desc_indx[i]:desc_indx[i + 1]]
            s = s.decode('UTF-8')
            while len(s) > 0 and s[-1] == '\u0000':
                s = s[:-1]
            res.append(s)
        return res

    # Read Desc file
    with open(f"{file_name}.bytes.dec", 'rb') as f:
        data = f.read()

    desc = solve(data, indx)
	
    WriteJSON(desc, f"{file_name}.bytes.dec.json")

if __name__ == '__main__':    
    #  ProgressiveProcessing(file_names[0], 0)    
    #  ProgressiveProcessing(file_names[1], 4)
    ProgressiveProcessing(file_names[0], 4)    
