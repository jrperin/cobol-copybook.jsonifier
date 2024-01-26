import unittest
from decimal import Decimal

from coboljsonifier.fields.field_numeric_ebcdic import FieldNumericEbcdic

class TestFieldNumericEbcdic(unittest.TestCase):

    value_positive  = b'\xF0\xF0\xF1\xF2\xC3' # -> +00123 = F0F0F1F2C3
    value_negative  = b'\xF0\xF0\xF1\xF2\xD3' # -> -00123 = F0F0F1F2D3
    value_no_signal = b'\xF0\xF0\xF1\xF2\xF3' # ->  00123 = F0F0F1F2F3 <- It's pretty the same as EBCDIC text

    def test_parsing(self):
    
        # Testa valor positivo  00123 = F0F0F1F2C3 - 2 decimals
        numeric_obj = FieldNumericEbcdic('NUMERIC', 'FIELD-NUMERIC-EBCDIC', 5, 2)
        numeric_obj.parse(self.value_positive)
        self.assertEqual(numeric_obj.value, {'FIELD-NUMERIC-EBCDIC' : Decimal('1.23')})

        # Testa valor negativo -00123 = F0F0F1F2D3 - 2 decimals
        numeric_obj = FieldNumericEbcdic('NUMERIC', 'FIELD-NUMERIC-EBCDIC', 5, 2)
        numeric_obj.parse(self.value_negative)
        self.assertEqual(numeric_obj.value, {'FIELD-NUMERIC-EBCDIC' : Decimal('-1.23')})
    
        # Testa valor sem sinal 00123 = F0F0F1F2F3 - 2 decimals
        numeric_obj = FieldNumericEbcdic('NUMERIC', 'FIELD-NUMERIC-EBCDIC', 5, 2)
        numeric_obj.parse(self.value_no_signal)
        self.assertEqual(numeric_obj.value, {'FIELD-NUMERIC-EBCDIC' : Decimal('1.23')})

    # def test_values(self):
    #     self.assertRaises(ValueError, FieldNumericEbcdic, b'\xA0\xF0\xF1\xF2\xF3', 2)
    #     self.assertRaises(ValueError, FieldNumericEbcdic, b'\xF0\xF0\xF1\xF2\xB3', 2)

if __name__ == '__main__':
    unittest.main()