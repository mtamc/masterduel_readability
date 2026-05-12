'''
Credit:
- timelic from NexusMods: https://forums.nexusmods.com/index.php?/user/145588218-timelic

Original source:
https://bitbucket.org/timel/master-duel-chinese-translation-patch/src/master/%E5%8D%A1%E7%89%87CARD/c_CARD%E5%8E%8B%E7%BC%A9.py
'''

from typing import List
import json
import sys
import zlib

#1. Check if CARD_* files exist:

def FileCheck(filename):
    try:
      open(filename, 'r')
      return 1
    except IOError:
      # print 'Error: File does not appear to exist.'
      return 0

filenames_to_check = ['CARD_Indx', 'CARD_Indx.bytes', 'CARD_Indx.txt', 'CARD_Desc', 'CARD_Desc.bytes', 'CARD_Desc.txt', 'CARD_Name', 'CARD_Name.bytes', 'CARD_Name.txt']
check_counter = -1
CARD_Indx_filename = ''
CARD_Desc_filename = ''
CARD_Name_filename = ''

for i in filenames_to_check:
	check_counter += 1		
	if FileCheck(i) == 1 and i.find('CARD_Indx') != -1 and CARD_Indx_filename == '':
		CARD_Indx_filename = i
	if FileCheck(i) == 1 and i.find('CARD_Desc') != -1 and CARD_Desc_filename == '':
		CARD_Desc_filename = i
	if FileCheck(i) == 1 and i.find('CARD_Name') != -1 and CARD_Name_filename == '':
		CARD_Name_filename = i
	if check_counter == len(filenames_to_check)-1 and CARD_Indx_filename == '':
		print('CARD_Indx file not found. The file name must be \"CARD_Indx\", \"CARD_Indx.bytes\" or \"CARD_Indx.txt\".\nPress <ENTER> to exit.')
		input()
		sys.exit()
	if check_counter == len(filenames_to_check)-1 and CARD_Desc_filename == '':
		print('CARD_Desc file not found. The file name must be \"CARD_Desc\", \"CARD_Desc.bytes\" or \"CARD_Desc.txt\".\nPress <ENTER> to exit.')
		input()
		sys.exit()
	if check_counter == len(filenames_to_check)-1 and CARD_Name_filename == '':
		print('CARD_Name file not found. The file name must be \"CARD_Name\", \"CARD_Name.bytes\" or \"CARD_Name.txt\".\nPress <ENTER> to exit.')
		input()
		sys.exit()

#2. Read JSON files

def ReadJSON(json_file_path: str) -> list or dict:
    with open(json_file_path, 'r', encoding='utf8') as f:
        dic: list = json.load(f)
    return dic
	
def getStringLen(s: str):
    return len(s.encode('utf-8'))
    res = 0
    for c in s:
        res += getCharLen(c)
    return res
	
def solve_P_desc(desc):
	
    res = ""
    res += monster_effect
    if p_effect != '':
        res += '\n'
        res += separator
        res += '\n'
        res += p_effect

    return res

print('Reading files...')

CARD_Name_json: list = ReadJSON(CARD_Name_filename + ".dec.json")
CARD_Desc_json: list = ReadJSON(CARD_Desc_filename + ".dec.json")

print('Finished reading files...')

#3. Merge JSON files

name_merge_string = "\u0000" * 8  # There are eight blanks at the beginning
desc_merge_string = "\u0000" * 8

merge_string = {"name": "\u0000" * 8, "desc": "\u0000" * 8}

name_indx = [0]
desc_indx = [0]

print('Merging files...')

for i in range(len(CARD_Name_json)):  # Here because of a strange bug in English desc is one less than name
    name = CARD_Name_json[i]
    desc = CARD_Desc_json[i]

    def helper(sentence: str, indx: List[int], name_or_desc: str,
               merge_string: dict):
        #    Cancel here first, but it shouldn't be a problem here.
        # Convert Chinese pendulum monster effects to Japanese format
        # if sentence.startswith('←'):
        #     sentence = solve_P_desc(sentence)
        length = getStringLen(sentence)
        if i == 0:
            length += 8
        space_len = 4 - length % 4  # It means getting the remainder
        indx.append(indx[-1] + length + space_len)  # Record indx
        if name_or_desc == "name":
            merge_string["name"] += sentence + '\u0000' * space_len
        else:
            merge_string["desc"] += sentence + '\u0000' * space_len

    helper(name, name_indx, "name", merge_string)
    helper(desc, desc_indx, "desc", merge_string)

print('Finished merging files.\nCalculating index...')

#4. Calculate CARD index

# Compression
# Can't compress. Compression is a problem.

name_indx = [4, 8] + name_indx[1:]
desc_indx = [4, 8] + desc_indx[1:]

# print(name_indx)
# print(desc_indx)

card_indx = []
for i in range(len(name_indx)):
    card_indx.append(name_indx[i])
    card_indx.append(desc_indx[i])

#print(card_indx)

def intTo4Hex(num: int) -> List[int]:
    res = []
    for _ in range(4):
        res.append(num % 256)
        num //= 256
    return res


card_indx_merge = []
for item in card_indx:
    card_indx_merge.extend(intTo4Hex(item))

print('Finished calculating index.')

# 5. Find crypto key

def Decrypt(filename):
    with open(f'{filename}', "rb") as f:
        data = bytearray(f.read())

    for i in range(len(data)):
        v = i + m_iCryptoKey + 0x23D
        v *= m_iCryptoKey
        v ^= i % 7
        data[i] ^= v & 0xFF

    with open(f'{filename}' + ".dec", "wb") as f:
        f.write(zlib.decompress(data))

def CheckCryptoKey():	
	try:
		Decrypt(CARD_Indx_filename)
		return 1
	except zlib.error:	
		return 0

if FileCheck('!CryptoKey.txt') == 1:
	print('Trying to read crypto key from file...')
	with open('!CryptoKey.txt', 'rt') as f_CryptoKey:		
			m_iCryptoKey = int(f_CryptoKey.read(),16)			
	f_CryptoKey.close()	
	print('Read crypto key "' + hex(m_iCryptoKey) + '" from file, checking if it is correct...')
else:
	m_iCryptoKey = 0x0

if CheckCryptoKey() == 1:
	print('The crypto key "' + hex(m_iCryptoKey) + '" is correct.')
else:
	print('No correct crypto key found. Searching for crypto key...')
	m_iCryptoKey = 0x0	
	while True:
		try:
			Decrypt(CARD_Indx_filename)			
			break
		except zlib.error:			
			m_iCryptoKey = m_iCryptoKey + 1			
	with open('!CryptoKey.txt', 'w') as f_CryptoKey:
		f_CryptoKey.write(hex(m_iCryptoKey))
	f_CryptoKey.close()
	print('Found correct crypto key "' + hex(m_iCryptoKey) + '" and wrote it to file "!CryptoKey.txt".')

# 6. Direct encryption

print('Encrypting files...')

file_names = [CARD_Name_filename, CARD_Desc_filename, CARD_Indx_filename]

def encrypt(output_name, b: bytes):

    data = bytearray(zlib.compress(b))

    for i in range(len(data)):
        v = i + m_iCryptoKey + 0x23D
        v *= m_iCryptoKey
        v ^= i % 7
        data[i] ^= v & 0xFF

    with open(output_name, "wb") as f:
        f.write((data))
    f.close()

encrypt(CARD_Name_filename,
        bytes(merge_string["name"], encoding='utf-8'))
encrypt(CARD_Desc_filename,
        bytes(merge_string["desc"], encoding='utf-8'))
encrypt(CARD_Indx_filename, bytes(card_indx_merge))

print('Finished encrypting files.')