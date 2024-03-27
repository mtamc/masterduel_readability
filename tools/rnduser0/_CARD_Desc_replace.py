# python3
import binascii
import fileinput
#import json
import math
#import os
#import regex
#import sys
from _defs import *

#1. Enable Card_Part modding and check for command line argument

Mod_Card_Part_file = True
Test_mode = False

if len(sys.argv) > 1:
    if sys.argv[1].find('t') != -1:
        Test_mode = True
    if sys.argv[1].find('p') != -1:
        Mod_Card_Part_file = False

#2. Check if Replace Guide, CARD_Desc JSON, decrypted Card_Part and decrypted Card_Pidx files exist:

Checked_filename_list = Check_files(['Replace Guide.txt', 'CARD_Desc.dec.json', 'Card_Pidx.dec', 'Card_Part.dec'])
RG_filename = Checked_filename_list[0]
CARD_Desc_JSON_filename = Checked_filename_list[1]
Card_Pidx_filename = Checked_filename_list[2]
Card_Part_filename = Checked_filename_list[3]

for i in range(len(Checked_filename_list)):
    print('Using file "' + Checked_filename_list[i] + '".')

#3. Create lists

RG_list = [] #Replacement Guide list
RG_list_backup = [] 
CARD_Desc_list = []

if Mod_Card_Part_file == True:
    CARD_Desc_ID_list = []
    First_Effect_ID_list = []
    Regular_Effects_Qty_list = []
    Pendulum_Effects_Qty_list = []

    Effect_ID_list = []
    Effect_Start_Offset_list = []
    Effect_End_Offset_list = []

#4. Read Replace Guide text file into the RG_list:

print('Reading file "' + RG_filename + '" into list...')

with open(RG_filename, 'rt', encoding="utf8") as f_RG:
    line_counter = 0
    for line in f_RG:
        line_counter += 1
        line = line.strip('\n') #remove line break
        #if not line == '' and not line == ' ': #skip empty lines (replaced by line below)
        if line_counter % 3 != 0: # check if line no. is not dividable by 3, because these are the blank lines
            RG_list.append(line) #append line to list
            RG_list_backup.append(line)
f_RG.close()

print('Completed.')

#Replace escaped characters with single custom characters because of card effect offsets
Replacement_list_regular = [(r'\n', 'ｎ'), (r'\"', '＂'), ('●', '●＿＿')]
Replacement_list_RegEx_search = [(r'\\n', 'ｎ'),(r'\\"', '＂'),(r'\(', '（'),(r'\)', '）')]
Replacement_list_RegEx_replace = [(r'\\n', 'ｎ'),(r'\\"', '＂'),(r'\(', '（'),(r'\)', '）')]
for i in range(len(RG_list)):    
    if i % 2  == 0:
        RG_list[i] = Replace_in_str(RG_list[i], Replacement_list_RegEx_search)
    elif i % 2  == 1:
        RG_list[i] = Replace_in_str(RG_list[i], Replacement_list_RegEx_replace)
    RG_list[i] = Replace_in_str(RG_list[i], Replacement_list_regular)

#5. Read Card_Pidx and Card_Part file into byte variables:

if Mod_Card_Part_file == True:
    print('Reading Card_Pidx and Card_Part files into string variables...')
    
    Card_Pidx_content = bytearray(ReadByteData(Card_Pidx_filename))
    Card_Part_content = bytearray(ReadByteData(Card_Part_filename))
    
    print('Completed.')

#6. Read CARD_Desc file and variables into lists

print('Reading CARD_Desc file into list...')

total_lines = CountFileLines(CARD_Desc_JSON_filename)

with open(CARD_Desc_JSON_filename, 'rt', encoding="utf8") as f_CARD_Desc_JSON:
    line_counter = 0    
    for line in f_CARD_Desc_JSON:
        line_counter += 1
        line = line.strip('\n') #remove line break
        if line_counter > 1 and line_counter < total_lines - 1:
            CARD_Desc_list.append(line[5:len(line)-2]) #Leave out the 4 leading spaces and quotation marks and comma at the end of the line
        elif line_counter == total_lines - 1:
            CARD_Desc_list.append(line[5:len(line)-1]) #Leave out the 4 leading spaces and quotation marks at the end of the line
f_CARD_Desc_JSON.close()

print('Completed.')

#Create Replacement_list to replace escaped characters with single custom characters because of card effect offsets:
Replacement_list = [(r'\n', 'ｎ'), (r'\"', '＂'), (r'\\', '＼')] # Replace line breaks, escaped quotation marks/backslashes

#Search for unicode characters and append replacement instructions to Replacement_list:
for i in range(len(CARD_Desc_list)):    
    for j in range(len(CARD_Desc_list[i])):
        if len(CARD_Desc_list[i][j].encode('utf8')) == 2:
            Replacement_list.append(tuple((CARD_Desc_list[i][j], CARD_Desc_list[i][j] + '＿')))
        if len(CARD_Desc_list[i][j].encode('utf8')) == 3:
            Replacement_list.append(tuple((CARD_Desc_list[i][j], CARD_Desc_list[i][j] + '＿＿')))
        if len(CARD_Desc_list[i][j].encode('utf8')) == 4:
            Replacement_list.append(tuple((CARD_Desc_list[i][j], CARD_Desc_list[i][j] + '＿＿＿')))

Replacement_list  = list(set(Replacement_list)) #Remove duplicates from list

#Apply replacement instructions in Replacement_list:
for i in range(len(CARD_Desc_list)):
    CARD_Desc_list[i] = Replace_in_str(CARD_Desc_list[i], Replacement_list)

CARD_Desc_list.insert(0, '') # Insert a blank item at the start of this list to match its indices with the following Card_Pidx ones

# For testing: Write card descriptions to text file (start)
if Test_mode == True and Mod_Card_Part_file == True:
    Card_desc_filename = 'Test - card descriptions before replacement.txt'
    print('Writing card description list to file "' + Card_desc_filename + '"...')    
    WriteDescs(Card_desc_filename,CARD_Desc_list)
    print('Completed.')
# For testing: Write card descriptions to text file (end)

if Mod_Card_Part_file == True:
    print('Reading Card_Pidx and Card_Part strings into lists...')
    for i in range (0,len(Card_Pidx_content)-1,4):
        CARD_Desc_ID_list.append(int(i/4))    
        First_Effect_ID_list.append(Card_Pidx_content[i+1]*256 + Card_Pidx_content[i])    
        if Card_Pidx_content[i+1] != 0:
            print(i)
            print(Card_Pidx_content[i+1])
            raise Exception('was not 0!')
        Regular_Effects_Qty_list.append(int(math.floor(Card_Pidx_content[i+3]/16)))    
        Pendulum_Effects_Qty_list.append(Card_Pidx_content[i+3] % 16)    
        
    for i in range (0,len(Card_Part_content)-1,4):    
        Effect_ID_list.append(int(i/4))
        Effect_Start_Offset_list.append(Card_Part_content[i+1]*256 + Card_Part_content[i])
        Effect_End_Offset_list.append(Card_Part_content[i+3]*256 + Card_Part_content[i+2])
    print('Completed.')
    raise('done')

# For testing: Write card effects to text file (start)
if Test_mode == True and Mod_Card_Part_file == True:
    Card_effects_filename = 'Test - card effects before replacement.txt'
    print('Writing card effect list to file "' + Card_effects_filename + '"...')    
    WriteEffects(Card_effects_filename, CARD_Desc_list, First_Effect_ID_list, Effect_Start_Offset_list, Effect_End_Offset_list, Regular_Effects_Qty_list, Pendulum_Effects_Qty_list)
    print('Completed.')
# For testing: Write card effects to text file (end)

#7. Apply string replacement instructions in replace guide text file and modifying card effect offsets:

if Mod_Card_Part_file == True:
    print('Replacing in card descriptions and modifying card effect offsets...')
elif Mod_Card_Part_file == False:
    print('Replacing in card descriptions...')
    
for CARD_Desc_list_i in range(1,len(CARD_Desc_list),1):
    for RG_list_i in range(0,len(RG_list)-1,2):        
        Card_Desc = CARD_Desc_list[CARD_Desc_list_i]
        #Test start
        if Test_mode == True:
            Test_CARD_Desc_list_i = 0
        #Test end    
        if Mod_Card_Part_file == True:
            Regular_Effects_Qty = Regular_Effects_Qty_list[CARD_Desc_list_i]
            Pendulum_Effects_Qty = Pendulum_Effects_Qty_list[CARD_Desc_list_i]
            First_Effect_ID = First_Effect_ID_list[CARD_Desc_list_i]            
            First_Pendulum_Effect_ID = 0
            First_Pendulum_Effect_Offset = 0
            First_Pendulum_Element_Offset = 0
            if Pendulum_Effects_Qty > 0:
                First_Pendulum_Effect_ID = First_Effect_ID + Regular_Effects_Qty
                First_Pendulum_Effect_Offset = Effect_Start_Offset_list[First_Pendulum_Effect_ID]
                First_Pendulum_Element_Offset = Card_Desc.find('[Pendulum Effect]')            
        #####################
        #Regular replacement:
        if not any([x in RG_list_backup[RG_list_i] for x in [r'\.',r'\\','([','.*','|']]): #check if replacement instruction contains RegEx        
            if Mod_Card_Part_file == True:
                Before_replacement_index_list = Find_all_in_str(Card_Desc, RG_list[RG_list_i])            
                #Test start
                if Test_mode == True and CARD_Desc_list_i == Test_CARD_Desc_list_i and Before_replacement_index_list != []:
                    print('Card_Desc before repl:',CARD_Desc_list[CARD_Desc_list_i])
                    print('RG search instruction:',RG_list[RG_list_i])
                    print('RG replace instruction:',RG_list[RG_list_i+1])
                    print('Before_replacement_index_list:',Before_replacement_index_list)
                #Test end
            CARD_Desc_list[CARD_Desc_list_i] = Card_Desc.replace(RG_list[RG_list_i],  #Simple string replacement
                                                                 RG_list[RG_list_i+1])            
            if Mod_Card_Part_file == True:
                Card_Desc = CARD_Desc_list[CARD_Desc_list_i]
                After_replacement_index_list = Find_all_in_str(Card_Desc, RG_list[RG_list_i+1])
                if Before_replacement_index_list != []:                    
                    while len(Before_replacement_index_list) < len(After_replacement_index_list) and After_replacement_index_list[0] < Before_replacement_index_list[0]:
                        del After_replacement_index_list[0]
                        #print("Before_replacement_index_list:",Before_replacement_index_list)
                        #print("After_replacement_index_list:",After_replacement_index_list)
                        
                #Test start
                if Test_mode == True and CARD_Desc_list_i == Test_CARD_Desc_list_i and Before_replacement_index_list != []:
                    print('After_replacement_index_list:',After_replacement_index_list)
                    print('Card_Desc after repl:',CARD_Desc_list[CARD_Desc_list_i])
                #Test end        
        #RegEx replacement:
        else:            
            if Mod_Card_Part_file == True:                
                Before_replacement_index_list = Find_all_RE_in_str(Card_Desc, RG_list[RG_list_i])
                #Test start
                if Test_mode == True and CARD_Desc_list_i == Test_CARD_Desc_list_i and Before_replacement_index_list != []:
                    print('Card_Desc before repl:',CARD_Desc_list[CARD_Desc_list_i])
                    print('RG search instruction:',RG_list[RG_list_i])
                    print('RG replace instruction:',RG_list[RG_list_i+1])                
                #Test end
            CARD_Desc_list[CARD_Desc_list_i] = regex.sub(RG_list[RG_list_i],  #RegEx replacement, search string
                                                      RG_list[RG_list_i+1].replace('\.','.'), #Remove backslash before dot in RegEx replacement string
                                                      CARD_Desc_list[CARD_Desc_list_i]) #Card desc to be replaced
            if Mod_Card_Part_file == True:
                Card_Desc = CARD_Desc_list[CARD_Desc_list_i]
                RE_Search_Instr = RG_list[RG_list_i] #Instr = Instruction
                RE_Repl_Instr = RG_list[RG_list_i+1] #Repl = Replacement
                Group_Ref_Start_list = Find_all_in_str(RE_Search_Instr, '(') #Ref = Reference
                Group_Ref_End_list = Find_all_in_str(RE_Search_Instr, ')')
                Group_1_Ref_Start = Group_Ref_Start_list[0]
                Group_1_Ref_End = Group_Ref_End_list[0] + 1
                if len(Group_Ref_Start_list) > 2:
                    Group_2_Ref_Start = Group_Ref_Start_list[2]
                else:
                    Group_2_Ref_Start =  -1
                if len(Group_Ref_End_list) > 2:
                    Group_2_Ref_End = Group_Ref_End_list[2] + 1
                else:
                    Group_2_Ref_End =  -1
                RE_Repl_Instr_to_Search_Instr = Replace_in_str(RE_Repl_Instr, [(r'\1',RE_Search_Instr[Group_1_Ref_Start:Group_1_Ref_End]),(r'\2',RE_Search_Instr[Group_2_Ref_Start:Group_2_Ref_End])])
                After_replacement_index_list = Find_all_RE_in_str(Card_Desc, RE_Repl_Instr_to_Search_Instr)
                if Before_replacement_index_list != []:  #Delete entries at start of After_replacement_index_list so that it has the same length as Before_replacement_index_list
                    while len(Before_replacement_index_list) < len(After_replacement_index_list) and After_replacement_index_list[0] < Before_replacement_index_list[0]:
                        del After_replacement_index_list[0]                    
                
                
                #Test start
                if Test_mode == True and CARD_Desc_list_i == Test_CARD_Desc_list_i and Before_replacement_index_list != []:                    
                    print('Card_Desc after repl:',CARD_Desc_list[CARD_Desc_list_i])
                #Test end                
        #####################
        if Mod_Card_Part_file == True:
            for i in range(0,len(Before_replacement_index_list)-1,2):
                Before_replacement_start_index = Before_replacement_index_list[i]
                Before_replacement_end_index = Before_replacement_index_list[i+1]
                After_replacement_start_index = After_replacement_index_list[i]
                After_replacement_end_index = After_replacement_index_list[i+1]
                Replacement_diff = (After_replacement_end_index - After_replacement_start_index) - (Before_replacement_end_index - Before_replacement_start_index)
                #Test start
                if Test_mode == True and CARD_Desc_list_i == Test_CARD_Desc_list_i and Before_replacement_index_list != []:
                    print('Before_replacement_index_list:',Before_replacement_index_list)
                    print('After_replacement_index_list:',After_replacement_index_list)                
                #Test end
                for j in range(Regular_Effects_Qty):
                    Effect_ID = First_Effect_ID + j
                    Effect_Start_Offset = Effect_Start_Offset_list[Effect_ID]
                    Effect_End_Offset = Effect_End_Offset_list[Effect_ID]

                    if (After_replacement_start_index > 0 and
                           After_replacement_end_index > 0 and
                           After_replacement_start_index  <  Effect_Start_Offset and
                           After_replacement_end_index  <  Effect_End_Offset and
                           abs(Replacement_diff) <= 2 and
                           Card_Desc[Effect_Start_Offset] != '●'):
                        Effect_Start_Offset_list[Effect_ID] = Effect_Start_Offset + Replacement_diff

                    if After_replacement_end_index > 0 and After_replacement_end_index  <  Effect_End_Offset and abs(Replacement_diff) <= 2:
                        Effect_End_Offset_list[Effect_ID] = Effect_End_Offset + Replacement_diff
                
                if Pendulum_Effects_Qty > 0:
                    for k in range(Pendulum_Effects_Qty):
                        Pendulum_Effect_ID = First_Pendulum_Effect_ID + k
                        Pendulum_Effect_Start_Offset = Effect_Start_Offset_list[Pendulum_Effect_ID]
                        Pendulum_Effect_End_Offset = Effect_End_Offset_list[Pendulum_Effect_ID]
                    
                        if  After_replacement_start_index > 0 and After_replacement_start_index < Pendulum_Effect_Start_Offset and abs(Replacement_diff) <= 2:                    
                            Effect_Start_Offset_list[Pendulum_Effect_ID] = Pendulum_Effect_Start_Offset + Replacement_diff

                        if After_replacement_end_index > 0 and After_replacement_end_index < Pendulum_Effect_End_Offset and abs(Replacement_diff) <= 2:
                            Effect_End_Offset_list[Pendulum_Effect_ID] = Pendulum_Effect_End_Offset + Replacement_diff
                #Test start
                if Test_mode == True and CARD_Desc_list_i == Test_CARD_Desc_list_i and After_replacement_index_list != []:
                    Test_Effect_ID = 0
                    print('Effect_ID:',Test_Effect_ID)
                    Test_Card_Desc = CARD_Desc_list[CARD_Desc_list_i]
                    Test_Effect_Start_Offset = Effect_Start_Offset_list[Test_Effect_ID]
                    print('Effect_Start_Offset:', Test_Effect_Start_Offset)                
                    Test_Effect_End_Offset = Effect_End_Offset_list[Test_Effect_ID]
                    print('Effect_End_Offset:', Test_Effect_End_Offset)
                    print('Effect:', Test_Card_Desc[Test_Effect_Start_Offset:Test_Effect_End_Offset])
                #Test end
print('Completed.')

#8. Convert Card_Part_list back to byte string
if Mod_Card_Part_file == True:
    print('Converting card part list to string variable...')
    Card_Part_content = ''
    for i in range(0,len(Effect_ID_list),1):
        Card_Part_content = ''.join([Card_Part_content,
                                     Dec2Hex(Effect_Start_Offset_list[i] % 256),
                                     Dec2Hex(math.floor(int(Effect_Start_Offset_list[i]/256))),
                                     Dec2Hex(Effect_End_Offset_list[i] % 256),
                                     Dec2Hex(math.floor(int(Effect_End_Offset_list[i]/256)))
                                     ])    
    Card_Part_content = binascii.unhexlify(Card_Part_content)
    print('Completed.')

#9. Write unencrypted Card_Part string to file

if Mod_Card_Part_file == True:
    print('Writing unencrypted Card_Part string to file...')    
    WriteByteData(Card_Part_filename, Card_Part_content)
    print('Completed.')

# For testing: Write card descriptions to text file (start)
if Test_mode == True and Mod_Card_Part_file == True:
    Card_desc_filename = 'Test - card descriptions before final replacement.txt'
    print('Writing card description list to file "' + Card_desc_filename + '"...')    
    WriteDescs(Card_desc_filename,CARD_Desc_list)
    print('Completed.')
# For testing: Write card descriptions to text file (end)

# For testing: Write card effects to text file (start)
if Test_mode == True and Mod_Card_Part_file == True:
    Card_effects_filename = 'Test - card effects before final replacement.txt'
    print('Writing card effect list to file "' + Card_effects_filename + '"...')    
    WriteEffects(Card_effects_filename, CARD_Desc_list, First_Effect_ID_list, Effect_Start_Offset_list, Effect_End_Offset_list, Regular_Effects_Qty_list, Pendulum_Effects_Qty_list)
    print('Completed.')
# For testing: Write card effects to text file (end)

#10. Write card description list to JSON file

#Replace temporary custom strings with original strings
Replacement_list =  [(sub[1], sub[0]) for sub in Replacement_list] # Swap tuples in Replacement_list
Replacement_list.append(tuple(('（','(')))
Replacement_list.append(tuple(('）',')')))
for i in range(len(CARD_Desc_list)):
    CARD_Desc_list[i] = Replace_in_str(CARD_Desc_list[i], Replacement_list)

print('Writing card description list to file "' + CARD_Desc_JSON_filename + '".')

with open(CARD_Desc_JSON_filename, 'wt', encoding='utf8') as f_CARD_Desc_JSON:
    f_CARD_Desc_JSON.write('[\n')
    for i in range(1,len(CARD_Desc_list)-1,1):
        f_CARD_Desc_JSON.write('    "' + CARD_Desc_list[i] + '",\n')
    f_CARD_Desc_JSON.write('    "' + CARD_Desc_list[len(CARD_Desc_list)-1] + '"\n')
    f_CARD_Desc_JSON.write(']')
f_CARD_Desc_JSON.close()

print('Completed.')

'''
print("Press <ENTER> to continue")
input()
'''
