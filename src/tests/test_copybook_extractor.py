import json
import os.path
import unittest

from coboljsonifier.copybookextractor import CopybookExtractor


class TestCopybookExtractor(unittest.TestCase):

    def test_open_book_file_exists(self):
        # If file exists the object will be generated
        ce = CopybookExtractor('src/tests/test_files/DATA1_BOOK.cob').dict_book_structure
        # print(json.dumps(ce, indent=2))
        self.assertTrue(ce)

    def test_open_book_file_not_exists(self):
        # if file no exists will raise an error
        self.assertRaises(IOError, CopybookExtractor, 'src/test_files/DATA1_BOOK_NOT_EXISTS.cob')

    
    def test_values(self):
        # Checks if copybook has redefines
        self.assertRaises(Exception, CopybookExtractor, 'src/test_files/book_tests_NOK_redefines.cob')
        # Checks if copybook has bad structure formation
        self.assertRaises(Exception, CopybookExtractor, 'src/test_files/book_tests_NOK_bad_format.cob')
        # Checks if the maximum binary lengh of 18 was overpassed
        # ce = CopybookExtractor('test_files/book_tests_NOK_binary_greater_18.cob').dict_book_structure
        # print(ce)
        # self.assertRaises(ValueError, CopybookExtractor, 'src/test_files/book_tests_NOK_binary_greater_18.cob')


    def test_constructor_with_file_and_list(self):
        
        with self.assertRaises(Exception) as e:
            CopybookExtractor(book_file_name="xpto", book_str_list=["xpto", "xpto"])

        self.assertEqual(str(e.exception), "Apenas um argumento deve ser preenchido book_file_name ou book_str_list")
    
    def test_constructor_without_args(self):
        
        with self.assertRaises(Exception) as e:
            CopybookExtractor()

        self.assertEqual(str(e.exception), "Apenas um argumento deve ser preenchido book_file_name ou book_str_list")

    def test_constructor_with_list_str(self):

        list_str = []
        with open('src/tests/test_files/DATA1_BOOK.cob', 'r', encoding="utf-8",errors="ignore") as f:
            while True:
                line = f.readline()
                if not line:
                    break
                list_str.append(line)
        
        ce = CopybookExtractor(book_str_list=list_str).dict_book_structure
        
        self.assertTrue(ce)

if __name__ == '__main__':
    unittest.main()