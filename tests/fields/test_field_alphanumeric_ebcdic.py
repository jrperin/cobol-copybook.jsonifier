import unittest

from src.coboljsonifier.fields.field_alphanumeric_ebcdic import FieldAlphanumericEbcdic

class TestFieldAlphanumericEbcdic(unittest.TestCase):

    ascii_text  = 'abcdefgABCDEFG#$!Ãã'
    ebcdic_text = ascii_text.encode('cp500')

    def test_parsing(self):
        alphaebcdic = FieldAlphanumericEbcdic('ALPHANUMERIC', 'FIELD-ALPHANUMERIC-EBCDIC', len(self.ascii_text))
        alphaebcdic.parse(self.ebcdic_text)
        self.assertEqual(alphaebcdic.value, {'FIELD-ALPHANUMERIC-EBCDIC' : self.ascii_text})
        

if __name__ == '__main__':
    unittest.main()