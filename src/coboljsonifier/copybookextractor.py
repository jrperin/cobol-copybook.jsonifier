import os.path
import re
from typing import List

from coboljsonifier.extractors.book_item import BookItem
from coboljsonifier.extractors.field_extractor import   FieldAlphabetic, FieldAlphanumeric, FieldArray, FieldEmpty, \
                                                        FieldGroup, FieldNumericMasked1, FieldSignalNumeric1, \
                                                        FieldSignalNumeric1Decimals1, FieldSignalNumeric1Decimals2, \
                                                        FieldSimpleNumeric, FieldSimpleNumeric1, FieldSimpleNumeric1Decimals1, \
                                                        FieldSimpleNumeric1Decimals2, FieldSimpleNumericDecimals1, \
                                                        FieldSimpleNumericDecimals2, FieldUndefined
from coboljsonifier.extractors.structure_extractor import   ArrayStructureExtractor, GroupStructureExtractor, RedefinesStructureExtractor, \
                                                            SimpleFieldStructureExtractor, SubformatStructureExtractor, \
                                                            UndefinedStructureExtractor
from coboljsonifier.extractors.subformat_extractor import BookItem, SubformatBinary, SubformatComp3, SubformatEmpty, SubformatUndefined


class CopybookExtractor:

    def __init__(self, bookfilename):

        if not os.path.isfile(bookfilename):
            raise IOError(f'Arquivo {bookfilename} não exite!')

        with open(bookfilename, 'r', encoding="utf-8",errors="ignore") as f:
            
            lines = list()
            while True:
                line = f.readline()
                if not line:
                    break
                line = (line + " "*80)[:72]
                lines.append(line)

            book = self._join_lines(lines)
            self.book_structure = self._extract(book)
            self.dict_book_structure = {}
            self._dictfy_structure(self.book_structure, self.dict_book_structure, 0, 0, 0)

    ''' Retira campos desnecessários (ex. Nível 88) '''
    def _is_disposable(self, line):

        # Nivel 88
        m = re.search(r"^.{6}[^(*)]\s+([0-9]+).*$", line)
        if m:
            if int(m.group(1)) >= 88:
                return True

        return False

    ''' Percorre todo o book (dicionario) e coloca a posicao no layout '''

    def _setup_position(self, itens, pos_ini):

        for item in itens.values():

            if isinstance(item, dict):
                item['initial_pos'] = pos_ini
                pos_ini += item['lenght']

            if isinstance(item, list):
                for it in item:
                    pos_ini = self._setup_position(it, pos_ini)

        return pos_ini

    ''' Pega o tamanho do campo com base no subformato (COMP-3, BINARY) '''

    def _extract_field_lenght(self, field):

        ''' Chain of Responsability para identificar o tipo do campo'''
        field_group = FieldGroup()
        field_array = FieldArray()
        field_empty = FieldEmpty()
        field_simple_numeric = FieldSimpleNumeric()
        field_simple_numeric_decimals1 = FieldSimpleNumericDecimals1()
        field_simple_numeric_decimals2 = FieldSimpleNumericDecimals2()
        field_simple_numeric1 = FieldSimpleNumeric1()
        field_simple_numeric1_decimals1 = FieldSimpleNumeric1Decimals1()
        field_simple_numeric1_decimals2 = FieldSimpleNumeric1Decimals2()
        field_signal_numeric1 = FieldSignalNumeric1()
        field_signal_numeric1_decimals1 = FieldSignalNumeric1Decimals1()
        field_signal_numeric1_decimals2 = FieldSignalNumeric1Decimals2()
        field_numeric_masked1 = FieldNumericMasked1()
        field_alphabetic = FieldAlphabetic()
        field_alphanumeric = FieldAlphanumeric()
        field_undefined = FieldUndefined()

        field_group.set_next(field_array) \
            .set_next(field_empty) \
            .set_next(field_simple_numeric) \
            .set_next(field_simple_numeric_decimals1) \
            .set_next(field_simple_numeric_decimals2) \
            .set_next(field_simple_numeric1) \
            .set_next(field_simple_numeric1_decimals1) \
            .set_next(field_simple_numeric1_decimals2) \
            .set_next(field_signal_numeric1) \
            .set_next(field_signal_numeric1_decimals1) \
            .set_next(field_signal_numeric1_decimals2) \
            .set_next(field_numeric_masked1)\
            .set_next(field_alphabetic) \
            .set_next(field_alphanumeric) \
            .set_next(field_undefined)
        field_group.extract(field)

        ''' Chain of Responsability para identificar o subtipo do campo '''
        subformat_empty = SubformatEmpty()
        subformat_comp3 = SubformatComp3()
        subformat_binary = SubformatBinary()
        subformat_undefined = SubformatUndefined()

        subformat_empty.set_next(subformat_comp3).set_next(subformat_binary).set_next(subformat_undefined)
        
        subformat_empty.extract(field)

        return field

    def _dictfy_structure(self, bookstructure: List[BookItem], parent_object, parent_level, item, position):
        
        i = item
        response = dict()

        if len(bookstructure) > 0:
            while i < len(bookstructure):

                if bookstructure[i].type in ('ALPHANUMERIC',
                                             'ALPHABETIC',
                                             'NUMERIC',
                                             'NUMERIC_COMP3',
                                             'NUMERIC_BINARY',
                                             'NUMERIC_MASKED'):

                    name = bookstructure[i].name
                    obj_level = bookstructure[i].level
                    bookstructure[i].start = position
                    position += bookstructure[i].size

                    if isinstance(parent_object, dict):
                        parent_object[name] = bookstructure[i].__dict__
                    elif isinstance(parent_object, list):
                        parent_object.append(bookstructure[i].__dict__)

                    if i < len(bookstructure)-1:
                        i += 1
                        next_obj_level = bookstructure[i].level
                        # Precisa de uma condicao de STOP se subir um nivel
                        if next_obj_level <= parent_level:
                            return i
                    else:
                        return i
                    
                elif bookstructure[i].type == 'GROUP':
                    i += 1

                elif bookstructure[i].type == 'ARRAY':
                    cicles = int(bookstructure[i].occurs)
                    my_name = bookstructure[i].name
                    my_level = bookstructure[i].level
                    i += 1
                    r = {}
                    i = self._dictfy_structure(bookstructure, r, my_level, i, position)

                    lines = list()
                    for j in range(1, cicles + 1, 1):
                        lines.append(r)

                    response[my_name] = lines

                    if isinstance(parent_object, dict):
                        parent_object[my_name] = response[my_name]
                    elif isinstance(parent_object, list):
                        parent_object.append(response[my_name])
                    
                else:
                    raise Exception(f'bookstructure.type nao reconhecido: [{bookstructure[i].type}]')

                # if i >= len(bookstructure):
                #     break

        return

    ''' Junta as linhas, remove comentarios e niveis 88 '''

    def _join_lines(self, book):
        prev = ""
        lines = list()
        pos = 0
        isArray = False
        array_temp = list()

        for line in book:
            line = line.replace(":", "").replace("\n", "")
            if len(line) >= 6 and line[6] != "*":  # Remove linhas que sao comentarios
                ln_aux = prev + line
                if ln_aux.find('.') > -1:
                    prev = ""
                    if not self._is_disposable(ln_aux):
                        lines.append(7 * ' ' + ln_aux[7:])
                else:
                    prev = (prev + line).rstrip()

        return lines

    def _extract(self, book):
        prev = ""
        clean_book = self._join_lines(book)
        lines = list()
        pos = 0
        isArray = False
        array_temp = list()

        filler_count = 1

        for book_line in clean_book:

            book_item = BookItem()

            group_extractor = GroupStructureExtractor()
            array_extractor = ArrayStructureExtractor()
            simple_field_extractor = SimpleFieldStructureExtractor()
            subformat_field_extractor = SubformatStructureExtractor()
            redefines_extractor = RedefinesStructureExtractor()
            undefined_extractor = UndefinedStructureExtractor()

            group_extractor.set_next(array_extractor) \
                .set_next(simple_field_extractor) \
                .set_next(subformat_field_extractor) \
                .set_next(redefines_extractor) \
                .set_next(undefined_extractor)

            group_extractor.extract(book_line, book_item)

            self._extract_field_lenght(book_item)

            ''' FILLER precisam ter nomes diferentes por causa do dict() '''
            if book_item.name.upper() == 'FILLER':
                book_item.name += f"-{filler_count}"
                filler_count += 1

            lines.append(book_item)

        return lines

