'''
Credits:
akintos: https://gist.github.com/akintos/04e2494c62184d2d4384078b0511673b
crazydoomy: https://github.com/crazydoomy
timelic: https://github.com/timelic/master-duel-chinese-translation-switch
'''

from typing import List
import json
import os
import regex
import sys
import zlib

#[Decrypt and split]

def FileCheck(filename):
	try:
		open(filename, 'r')
		return 1
	except IOError:
	#print 'Error: File does not appear to exist.'
		return 0

def Decrypt(data: bytes, m_iCryptoKey):	
	data = bytearray(data)
	try:
		for i in range(len(data)):
			v = i + m_iCryptoKey + 0x23D
			v *= m_iCryptoKey
			v ^= i % 7
			data[i] ^= v & 0xFF			
		return zlib.decompress(data)		
	except zlib.error:
		#print('zlib.error because of wrong crypo key:' + hex(m_iCryptoKey))		
		return bytearray()
	#except Exception:
	#else:

def ReadByteData(filename):
	with open(f'{filename}', "rb") as f:
		data = f.read()
	f.close()
	return data

def WriteByteData(filename, data):
	with open(f'{filename}', "wb") as f:
		f.write(data)
	f.close()

def WriteDecByteData(filename, data):
	with open(f'{filename}' + ".dec", "wb") as f:
		f.write(data)
	f.close()

def WriteUTF8Data(filename, data):
	with open(f'{filename}', "wt", encoding="utf8") as f:
		f.write(data)
	f.close()

def CheckCryptoKey(filename, m_iCryptoKey):	
	data = ReadByteData(filename)	
	if Decrypt(data, m_iCryptoKey) == bytearray():
		return 0
	else:
		return 1
		
def FindCryptoKey(filename):
	print('No correct crypto key found. Searching for crypto key...')	
	m_iCryptoKey = -0x1	
	data = ReadByteData(filename)	
	dec_data = bytearray()	
	while dec_data == bytearray():			
			m_iCryptoKey = m_iCryptoKey + 1				
			dec_data = Decrypt(data, m_iCryptoKey)
			#if os.stat('CARD_Indx.dec').st_size > 0:						
	with open('!CryptoKey.txt', 'w') as f_CryptoKey:
		f_CryptoKey.write(hex(m_iCryptoKey))
	f_CryptoKey.close()
	print('Found correct crypto key "' + hex(m_iCryptoKey) + '" and wrote it to file "!CryptoKey.txt".')	
	return m_iCryptoKey

def GetCryptoKey(filename):
	if FileCheck('!CryptoKey.txt') == 1:
		print('Trying to read crypto key from file...')
		with open('!CryptoKey.txt', 'rt') as f_CryptoKey:		
				m_iCryptoKey = int(f_CryptoKey.read(),16)			
		f_CryptoKey.close()	
		print('Read crypto key "' + hex(m_iCryptoKey) + '" from file, checking if it is correct...')
	else:
		m_iCryptoKey = 0x0

	if CheckCryptoKey(filename, m_iCryptoKey) == 1:
		print('The crypto key "' + hex(m_iCryptoKey) + '" is correct.')
	else:
		m_iCryptoKey = FindCryptoKey(filename)	
	return m_iCryptoKey

def Check_files(Filename_list):
	Checked_filename_list = []
	File_found_list = []
	
	for i in range(len(Filename_list)):
		Filename = Filename_list[i]
		
		if Filename.find('.') == -1: #if no dot found in filename			
			if FileCheck(Filename) == 1:
				Checked_filename_list.append(Filename)
			elif FileCheck(Filename + '.bytes') == 1				:
				Checked_filename_list.append(Filename + '.bytes')
			elif FileCheck(Filename + '.txt') == 1:
				Checked_filename_list.append(Filename + '.txt')			
		
		if Filename.find('.dec') ==  len(Filename) - 4: #if ".dec" found at the end of filename
			if FileCheck(Filename) == 1:
				Checked_filename_list.append(Filename)
			elif FileCheck(Filename.replace('.dec', '.bytes.dec')) == 1:
				Checked_filename_list.append(Filename.replace('.dec', '.bytes.dec'))
			elif FileCheck(Filename.replace('.dec', '.txt.dec')) == 1:
				Checked_filename_list.append(Filename.replace('.dec', '.txt.dec'))

		if Filename.find('.dec.json') ==  len(Filename) - 9: #if ".dec.json" found at the end of filename									
			if FileCheck(Filename) == 1:
				Checked_filename_list.append(Filename)				
			if FileCheck(Filename.replace('.dec.json', '.bytes.dec.json')) == 1:				
				Checked_filename_list.append(Filename.replace('.dec.json', '.bytes.dec.json'))				
			if FileCheck(Filename.replace('.dec.json', '.txt.dec.json')) == 1:
				Checked_filename_list.append(Filename.replace('.dec.json', '.txt.dec.json'))
		
		if Filename.find('Replace Guide.txt') !=  -1: #if "Replace Guide.txt" found in filename
			if FileCheck(Filename) == 1:
				Checked_filename_list.append(Filename)
			elif FileCheck(Filename.replace(' ', '_')) == 1:
				Checked_filename_list.append(Filename.replace(' ', '_'))
	
		if len(Checked_filename_list) == i + 1:
			File_found_list.append(True)
		else:
			File_found_list.append(False)
		
	for i in range(len(Checked_filename_list)):
		Filename = Filename_list[i]
		Checked_filename = Checked_filename_list[i]
		File_found = File_found_list[i]
		
		if  File_found == False:
			print('"' + Filename + '" file not found.\nPress <ENTER> to exit.')
			input()
			sys.exit()
			
	return Checked_filename_list
	
def WriteJSON(l: list, json_file_path: str):
    with open(json_file_path, 'w', encoding='utf8') as f:
        json.dump(l, f, ensure_ascii=False, indent=4)

# The start of Name and Desc is 0 and 4 respectively
def ProgressiveProcessing(CARD_Indx_filename, filename, start):

    # Read binary index
    with open(CARD_Indx_filename + ".dec", "rb") as f:
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

    def FourToOne(x: List[int]) -> int:
        res = 0
        for i in range(3, -1, -1):
            res *= 16 * 16
            res += x[i]
        return res

    indx = [FourToOne(i) for i in indx]
    indx = indx[1:]
    	
    # Convert Decrypted CARD files to JSON files    
    def Solve(data: bytes, desc_indx: List[int]):
        res = []
        for i in range(len(desc_indx) - 1):
            s = data[desc_indx[i]:desc_indx[i + 1]]
            s = s.decode('UTF-8')
            while len(s) > 0 and s[-1] == '\u0000':
                s = s[:-1]
            res.append(s)
        return res

    # Read Desc file
    with open(f"{filename}" + ".dec", 'rb') as f:
        data = f.read()

    desc = Solve(data, indx)
	
    WriteJSON(desc, f"{filename}" + ".dec.json")
	
#[Merge and encrypt]

def ReadJSON(json_file_path: str) -> list or dict:
    with open(json_file_path, 'r', encoding='utf8') as f:
        dic: list = json.load(f)
    return dic
	
def GetStringLen(s: str):
    return len(s.encode('utf-8'))
    res = 0
    for c in s:
        res += getCharLen(c)
    return res
	
def Solve_P_desc(desc):	
    res = ""
    res += monster_effect
    if p_effect != '':
        res += '\n'
        res += separator
        res += '\n'
        res += p_effect
    return res

def IntTo4Hex(num: int) -> List[int]:
    res = []
    for _ in range(4):
        res.append(num % 256)
        num //= 256
    return res

def Encrypt(data: bytes, m_iCryptoKey, output_filename):
    data = bytearray(zlib.compress(data))

    for i in range(len(data)):
        v = i + m_iCryptoKey + 0x23D
        v *= m_iCryptoKey
        v ^= i % 7
        data[i] ^= v & 0xFF

    with open(output_filename, "wb") as f:
        f.write((data))
    f.close()

def CountFileLines(filename):
	with open(filename, 'rt', encoding="utf8") as f:
		for count, line in enumerate(f):
			pass
	f.close()
	return count + 1

def Dec2Hex(decimal):
	conversion_table = {0: '0', 1: '1', 2: '2', 3: '3', 4: '4',
						5: '5', 6: '6', 7: '7',
						8: '8', 9: '9', 10: 'A', 11: 'B', 12: 'C',
						13: 'D', 14: 'E', 15: 'F'}
	if decimal == 0:
		hexadecimal = '0'
	else:
		hexadecimal = ''	
		while(decimal > 0):
			remainder = decimal % 16
			hexadecimal = conversion_table[remainder] + hexadecimal
			decimal = decimal // 16
	
	if len(hexadecimal) % 2 != 0:
		hexadecimal = '0' + hexadecimal
	
	return hexadecimal

def Find_all_in_str(str, substr):
	index_list = []
	StrPos = 0 #String position
	TempStrPos = 0
	StrLen = len(str)
	while StrPos < StrLen:
		TempStrPos = str[StrPos:StrLen].find(substr)
		if TempStrPos != -1:			
			StrPos = StrPos + TempStrPos
			index_list.append(StrPos)
			index_list.append(StrPos + len(substr) - 1)
			StrPos = StrPos + len(substr)
		else:
			break
	return index_list

def Find_all_RE_in_str(str, RegEx_substr):
	iteration = regex.finditer(RegEx_substr, str)
	index_list = []
	for match in iteration:		
		index_list.append(match.start())
		index_list.append(match.end())
	#index_list = [(match.start(), match.end()) for match in iteration]		
	return index_list

def Replace_in_str(str, replacement_list):
	#Example: replacement_list = [('a', 'b'), ('c', 'd')]
	for char, replacement in replacement_list:
		if char in str:
			str = str.replace(char, replacement)
	return str

def WriteEffects(filename, CARD_Desc_list, First_Effect_ID_list, Effect_Start_Offset_list, Effect_End_Offset_list, Regular_Effects_Qty_list, Pendulum_Effects_Qty_list):
	with open(f'{filename}', "wt", encoding="utf8") as f:		
		for CARD_Desc_list_i in range(len(CARD_Desc_list)):			
			Effects_Qty = Regular_Effects_Qty_list[CARD_Desc_list_i] + Pendulum_Effects_Qty_list[CARD_Desc_list_i]
			for i in range(Effects_Qty):
				Effect_ID = First_Effect_ID_list[CARD_Desc_list_i] + i
				Card_Desc = CARD_Desc_list[CARD_Desc_list_i]
				Effect_Start_Offset = Effect_Start_Offset_list[Effect_ID]
				Effect_End_Offset = Effect_End_Offset_list[Effect_ID]
				f.write(Card_Desc[Effect_Start_Offset:Effect_End_Offset] + '\n')
	f.close()
