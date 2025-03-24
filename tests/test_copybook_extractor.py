import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))

import unittest
import simplejson as json
from coboljsonifier.copybookextractor import CopybookExtractor


class TestCopybookExtractor(unittest.TestCase):

    def test_open_book_file_exists(self):
        # If file exists the object will be generated
        dict_schema = CopybookExtractor('tests/test_files/book_tests_OK.cob').dict_book_structure
        # print(json.dumps(dict_schema, indent=2))
        self.assertTrue(dict_schema)

    def test_open_book_file_not_exists(self):
        # if file no exists will raise an error
        self.assertRaises(IOError, CopybookExtractor, 'tests/test_files/book_tests_NOT_EXISTS.cob')

    
    def test_values(self):
        # Checks if copybook has redefines
        self.assertRaises(Exception, CopybookExtractor, 'tests/test_files/book_tests_NOK_redefines.cob')
        # Checks if copybook has bad structure formation
        self.assertRaises(Exception, CopybookExtractor, 'tests/test_files/book_tests_NOK_bad_format.cob')
        # Checks if the maximum binary lengh of 18 was overpassed
        # dict_schema = CopybookExtractor('tests/test_files/book_tests_NOK_binary_greater_18.cob').dict_book_structure
        # print(dict_schema)
        self.assertRaises(ValueError, CopybookExtractor, 'tests/test_files/book_tests_NOK_binary_greater_18.cob')

    def test_texas_well_data_with_spaces_and_value_zeroes(self):
        dict_schema = CopybookExtractor('tests/test_files/book_texas_well_data.cob').dict_book_structure
        print(json.dumps(dict_schema, indent=2))
        self.assertTrue(dict_schema)

if __name__ == '__main__':
    unittest.main()