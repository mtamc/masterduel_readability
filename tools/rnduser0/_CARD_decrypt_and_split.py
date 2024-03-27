'''
Credits:
akintos: https://gist.github.com/akintos/04e2494c62184d2d4384078b0511673b
timelic: https://github.com/timelic/master-duel-chinese-translation-switch
'''

#from typing import List
#import json
#import os
#import sys
#import zlib
from _defs import *

# 1. Check if CARD_* files exist:

CARD_filename_list = Check_files(['CARD_Indx', 'CARD_Name', 'CARD_Desc', 'Card_Pidx', 'Card_Part'])
CARD_Indx_filename = CARD_filename_list[0]
CARD_Name_filename = CARD_filename_list[1]
CARD_Desc_filename = CARD_filename_list[2]

# 2. Get crypto key

m_iCryptoKey = GetCryptoKey(CARD_Indx_filename)

# 3. Decrypt card files from section 1

print('Decrypting files...')

for filename in CARD_filename_list:	
	data = ReadByteData(filename)
	data = Decrypt(data, m_iCryptoKey)
	WriteDecByteData(filename, data)
	print('Decrypted file "' + filename + '".')

# 4. Split CARD_Name + CARD_Desc

print('Splitting files...')

if __name__ == '__main__':    
    ProgressiveProcessing(CARD_Indx_filename, CARD_Name_filename, 0)    
    ProgressiveProcessing(CARD_Indx_filename, CARD_Desc_filename, 4)

print('Finished splitting files.')
