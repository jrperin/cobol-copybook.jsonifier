import json
import os.path
import unittest

from coboljsonifier.copybookextractor import CopybookExtractor


class TestCopybookExtractor(unittest.TestCase):

    def test_open_book_file_exists(self):
        # If file exists the object will be generated
        ce = CopybookExtractor('tests/test_files/book_tests_OK.cob').dict_book_structure
        # print(json.dumps(ce, indent=2))
        self.assertTrue(ce)

    def test_open_book_file_not_exists(self):
        # if file no exists will raise an error
        self.assertRaises(IOError, CopybookExtractor, 'tests/test_files/book_tests_NOT_EXISTS.cob')

    
    def test_values(self):
        # Checks if copybook has redefines
        self.assertRaises(Exception, CopybookExtractor, 'tests/test_files/book_tests_NOK_redefines.cob')
        # Checks if copybook has bad structure formation
        self.assertRaises(Exception, CopybookExtractor, 'tests/test_files/book_tests_NOK_bad_format.cob')
        # Checks if the maximum binary lengh of 18 was overpassed
        # ce = CopybookExtractor('tests/test_files/book_tests_NOK_binary_greater_18.cob').dict_book_structure
        # print(ce)
        self.assertRaises(ValueError, CopybookExtractor, 'tests/test_files/book_tests_NOK_binary_greater_18.cob')


if __name__ == '__main__':
    unittest.main()