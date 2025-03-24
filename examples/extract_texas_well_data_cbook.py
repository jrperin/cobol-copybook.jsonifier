import os
import simplejson
from coboljsonifier.copybookextractor import CopybookExtractor
from coboljsonifier.parser import Parser
from coboljsonifier.config.parser_type_enum import ParseType

current_directory = os.getcwd()
print("Diretório atual:", current_directory)

find_name = 'book_texas_well_data.cob'
bookfname = None
for root, dirs, files in os.walk(current_directory):
    if find_name in files:
        bookfname = os.path.join(root, find_name)
        break

if bookfname is None:
    raise FileNotFoundError("File {} not found!.", find_name)

print("Full path and filename:", bookfname)

dict_structure = CopybookExtractor(bookfname).dict_book_structure

print(simplejson.dumps(dict_structure))

