import sys
import os
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../')))

import unittest
from src.coboljsonifier.fields.field_alphanumeric_ascii import FieldAlphanumericAscii


class TestFieldAlphanumericAscii(unittest.TestCase):

    ascii_text  = 'abcdefgABCDEFG#$!Ãã'
    ebcdic_text = ascii_text.encode('cp500')

    def test_parsing_OK(self):
        alpha = FieldAlphanumericAscii('ALPHANUMERIC', 'FIELD-ALPHANUMERIC-ASCII', len(self.ascii_text))
        alpha.parse(self.ascii_text)
        self.assertEqual(alpha.value, {'FIELD-ALPHANUMERIC-ASCII' : self.ascii_text})
        

    def test_parsing_null(self):
        alpha = FieldAlphanumericAscii('ALPHANUMERIC', 'FIELD-ALPHANUMERIC-ASCII', len(self.ascii_text))
        alpha.parse("")
        self.assertEqual(alpha.value, {'FIELD-ALPHANUMERIC-ASCII' : None})

    def test_parsing_wrong_content(self):
        alpha = FieldAlphanumericAscii('ALPHANUMERIC', 'FIELD-ALPHANUMERIC-ASCII', -20)
        alpha.parse("x")
        self.assertEqual(alpha.value, {'FIELD-ALPHANUMERIC-ASCII' : None})

if __name__ == '__main__':
    unittest.main()