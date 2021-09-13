import simplejson

from coboljsonifier.copybookextractor import CopybookExtractor
from coboljsonifier.parser import Parser
from coboljsonifier.config.parser_type_enum import ParseType

# fname = '/Users/ragguag/Documents/workspace/cb-cobol-copybook-converter/tests/file_tests/book_layout_for_tests_OK.txt'
# fname = '/Users/jrpqhde/Downloads/BTK302CB.txt'
fname='/home/jrperin/projetos/cobol-copybook.jsonifier/coboljsonifier/tests/test_files/EBCDIC_DATA1.cob'

# TESTE ASCII com PIC +99999999999999.99
# fname = "./tests/file_tests/VQLBMIG_BOOK_SO_BODY.txt"

dict_structure = CopybookExtractor(fname).dict_book_structure

print(dict_structure)
exit()
# Mostrar conteudo da estrutura
# print(f"\nEstrutura do Parser ", "-" * 50)
# print(simplejson.dumps(dict_structure, indent=2))

parser = Parser(dict_structure, ParseType.FLAT_ASCII).parser
size = parser.size
print("// Tamanho do registro:", size)

print("// " + "-" * 70)
# fname2 = '/Users/ragguag/Documents/workspace/cb-cobol-copybook-converter/tests/file_tests/mock_dados_vq.txt'
# fname2 = '/Users/jrpqhde/Downloads/Arquivo Teste - TK - binario.BIN'

# TESTE ASCII com PIC +99999999999999.99
fname2 = "./tests/file_tests/VQLBMIG_CONTEUDO.txt"

i = 0
''' Nota importante! 
    Se arquivo ebcdic: open file com rb "read binary" e f2.read(size)
    Se arquivo ascii : open file com r  "read text" e f2.readline()
'''
with open(fname2, 'r') as f2:
    while True:

        # EBCDIC
        # data = f2.read(size)

        # ASCII
        data = f2.readline()

        if not data:
            # got eof
            break

        i += 1

        print(f"\n// Registro : {i} ","-" * 50)

        if data[0:2] == "02":
            parser.parse(data)
            x = parser.value

            # ALERTA: Não usar json.dumps porque tem formatos Decimal (float com precisao)
            # print(json.dumps(x, indent=2))

            # Usar simplejson que dá suporte aos formatos Decimal.
            print(simplejson.dumps(x, indent=2))

        else:
            print(f'// Registro do tipo {data[0:2]} nao processado')
        
        

print(f"// " + "-" * 70)
print(f"// Registros processados {i}")
print()

exit(0)
