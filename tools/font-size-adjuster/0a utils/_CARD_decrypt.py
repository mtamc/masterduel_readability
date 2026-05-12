'''
Credit: akintos: https://gist.github.com/akintos/04e2494c62184d2d4384078b0511673b
'''

import sys
import zlib

file_name = sys.argv[1]

#0. Definitions

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
		print('Reading crypto key "' + hex(m_iCryptoKey) + '" from file, checking if it is correct...')
	else:
		m_iCryptoKey = 0x0

	if CheckCryptoKey(filename, m_iCryptoKey) == 1:
		print('The crypto key "' + hex(m_iCryptoKey) + '" is correct.')
	else:
		m_iCryptoKey = FindCryptoKey(filename)	
	return m_iCryptoKey

# 1. Get crypto key

print('Getting crypto key...')
m_iCryptoKey = GetCryptoKey(file_name)

# 2. Read data from file into byte array

print('Reading file "' + file_name + '"...')
with open(file_name, "rb") as f:
	data = bytearray(f.read())
print('Done.')

# 3. Decrypt

print('Decrypting...')
for i in range(len(data)):
	v = i + m_iCryptoKey + 0x23D
	v *= m_iCryptoKey
	v ^= i % 7
	data[i] ^= v & 0xFF
print('Done.')

# 4. Write decrypted data to file

print('Writing to file "' + file_name + '.dec"...')
with open(file_name + ".dec", "wb") as f:
	f.write(zlib.decompress(data))
print('Done.')
