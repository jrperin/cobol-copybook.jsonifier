from coboljsonifier.fields.field import Field
from coboljsonifier.fields.field_alphanumeric_ascii import FieldAlphanumericAscii 
from coboljsonifier.fields.field_alphanumeric_ebcdic import FieldAlphanumericEbcdic
from coboljsonifier.fields.field_array import FieldArray
from coboljsonifier.fields.field_group import FieldGroup
from coboljsonifier.fields.field_numeric_ascii import FieldNumericAscii
from coboljsonifier.fields.field_numeric_binary import FieldNumericBinary
from coboljsonifier.fields.field_numeric_comp3 import FieldNumericComp3
from coboljsonifier.fields.field_numeric_ebcdic import FieldNumericEbcdic
from coboljsonifier.fields.field_numeric_masked_ascii import FieldNumericMaskedAscii
from coboljsonifier.fields.field_wrapper import FieldWrapper

from coboljsonifier.config.parser_type_enum import ParseType 


class Parser:

    def __init__(self, book_structure, parser_type):

        self._book_structure = book_structure
        self._parser_type = parser_type
        main_node = FieldWrapper()
        self.parser = self._create(main_node, self._book_structure)

    def build(self):
        return self.parser

    def _create(self, parent: Field, book_structure):

        '''
            Process Logic
            receive a parent and the dictionary structure
            - for each item in the dictionary, add it as a child
                - If the item is a complex object
                    - call recursively to add the objects from the complex dict
        '''

        if len(book_structure) < 1:
            print("An error occurred while building the copybook data dictionary structure!")
            return

        ''' IT MUST ALWAYS BE A DICT (WRAPPER) '''
        for k in book_structure:
            if isinstance(book_structure[k], dict):
                f = self._choose_instance(book_structure[k])
                parent.add(f)

            elif isinstance(book_structure[k], list):
                arr = FieldArray("ARRAY", k)
                parent.add(arr)
                for item in book_structure[k]:
                    g = FieldWrapper()
                    arr.add(g)
                    self._create(g, item)
            else:
                raise Exception(f"Invalid type {type(book_structure[k])}")

        return parent

    def _create_by_type(self, k, v, parent):
        if isinstance(v, dict):
            f = self._choose_instance(v)
            parent.add(f)
        elif isinstance(v, list):
            f = FieldArray("ARRAY", k)
            parent.add(f)
            self._create(f, v)

    def _choose_instance(self, value):
        f = None
        if value.get('type') == 'ALPHANUMERIC' or value.get('type') == 'ALPHABETIC':
            if self._parser_type == ParseType.FLAT_ASCII:
                f = FieldAlphanumericAscii(value['type'], value['name'], value['size'])
            elif self._parser_type == ParseType.BINARY_EBCDIC:
                f = FieldAlphanumericEbcdic(value['type'], value['name'], value['size'])
            else:
                raise Exception(f'Invalid ParseType {self._parser_type}')

        elif value.get('type') == 'NUMERIC':
            if self._parser_type == ParseType.FLAT_ASCII:
                f = FieldNumericAscii(value['type'], value['name'], value['size'], value['decimals'])
            elif self._parser_type == ParseType.BINARY_EBCDIC:
                f = FieldNumericEbcdic(value['type'], value['name'], value['size'], value['decimals'])
            else:
                raise Exception(f'Invalid ParseType {self._parser_type}')

        elif value.get('type') == 'NUMERIC_COMP3':
            f = FieldNumericComp3(value['type'], value['name'], value['size'], value['decimals'])

        elif value.get('type') == 'NUMERIC_BINARY':
            f = FieldNumericBinary(value['type'], value['name'], value['size'], value['decimals'])

        elif value.get('type') == 'NUMERIC_MASKED':
            if self._parser_type != ParseType.FLAT_ASCII:
                raise Exception(f'Invalid ParseType {self._parser_type} can only be used with ParseType.FLAT_ASCII')
            f = FieldNumericMaskedAscii(value['type'], value['name'], value['size'], value['decimals'])

        else:
            raise Exception(f'Invalid format {value.get("type")}')
        return f