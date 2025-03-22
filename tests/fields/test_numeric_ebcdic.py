import sys
import os
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../')))

import unittest
from decimal import ROUND_HALF_UP, Decimal

from src.coboljsonifier.fields.field_numeric_ebcdic import FieldNumericEbcdic

class TestFieldNumericEbcdic(unittest.TestCase):

    value_positive  = b'\xF0\xF0\xF1\xF2\xC3' # -> +00123 = F0F0F1F2C3
    value_negative  = b'\xF0\xF0\xF1\xF2\xD3' # -> -00123 = F0F0F1F2D3
    value_no_signal = b'\xF0\xF0\xF1\xF2\xF3' # ->  00123 = F0F0F1F2F3 <- It's pretty the same as EBCDIC text

    def test_parsing(self):
    
        # Test positive value 00123 = F0F0F1F2C3 - 2 decimals
        numeric_obj = FieldNumericEbcdic('NUMERIC', 'FIELD-NUMERIC-EBCDIC', 5, 2)
        numeric_obj.parse(self.value_positive)
        self.assertEqual(numeric_obj.value, {'FIELD-NUMERIC-EBCDIC' : Decimal(1.23).quantize(Decimal('1.00'), rounding=ROUND_HALF_UP)})

        # Test negative value -00123 = F0F0F1F2D3 - 2 decimals
        numeric_obj = FieldNumericEbcdic('NUMERIC', 'FIELD-NUMERIC-EBCDIC', 5, 2)
        numeric_obj.parse(self.value_negative)
        self.assertEqual(numeric_obj.value, {'FIELD-NUMERIC-EBCDIC' : Decimal(-1.23).quantize(Decimal('1.00'), rounding=ROUND_HALF_UP)})
    
        # Test value without signal 00123 = F0F0F1F2F3 - 2 decimals
        numeric_obj = FieldNumericEbcdic('NUMERIC', 'FIELD-NUMERIC-EBCDIC', 5, 2)
        numeric_obj.parse(self.value_no_signal)
        self.assertEqual(numeric_obj.value, {'FIELD-NUMERIC-EBCDIC' : Decimal(1.23).quantize(Decimal('1.00'), rounding=ROUND_HALF_UP)})

    # def test_values(self):
    #     self.assertRaises(ValueError, FieldNumericEbcdic, b'\xA0\xF0\xF1\xF2\xF3', 2)
    #     self.assertRaises(ValueError, FieldNumericEbcdic, b'\xF0\xF0\xF1\xF2\xB3', 2)

if __name__ == '__main__':
    unittest.main()