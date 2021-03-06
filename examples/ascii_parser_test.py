import simplejson
from coboljsonifier.copybookextractor import CopybookExtractor
from coboljsonifier.parser import Parser
from coboljsonifier.config.parser_type_enum import ParseType

bookfname='ASCII_BOOK.cob'

dict_structure = CopybookExtractor(bookfname).dict_book_structure
print(simplejson.dumps(dict_structure))

# Use book structure to build a parser (FLAT_ASCII / BINARY_EBCDIC)
parser = Parser(dict_structure, ParseType.FLAT_ASCII).build()
size = parser.size
print("// Registry calculated lenght:", size)
print("// " + "-" * 70)

datafname = 'ASCII_DATA.bin'

i = 0
''' Important Note! 
    ebcdic file: Open the file with rb "read binary" and f2.read(size)
    ascii file : Open the file with  r "read text" and f2.readline()
'''
with open(datafname, 'r') as f2:
    while True:

        # ASCII
        data = f2.readline()

        if not data:
            # got eof
            break

        i += 1

        print(f"\n// Registry : {i} ","-" * 50)

        if data[0:2] == "02":         # for ASCII
            parser.parse(data)
            dict_value = parser.value

            # ALERT: Don't use json.dumps. It doesn't threat Decimal formats - float for monetary values 
            # Use simplejson instead, it has support for Decimals
            print(simplejson.dumps(dict_value))

        else:
            print(f'// Registry type {data[0:2]} not processed')
        
        

print(f"// " + "-" * 70)
print(f"// Total processed {i}")
print()

