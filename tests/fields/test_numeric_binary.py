from decimal import ROUND_HALF_UP, Decimal
import sys
import os
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../')))

import unittest
from src.coboljsonifier.fields.field_numeric_binary import FieldNumericBinary


class TestFieldNumericBinary(unittest.TestCase):


    binary_123 = b'\x00\x00\x00\x7B'            # ->  00123
    binary_negative_123 = b'\xFF\xFF\xFF\x85'   # -> -00123


    def test_parsing(self):

        # Testa valor 00123 sem casas decimais
        binary_positive=FieldNumericBinary('NUMERIC_BINARY', 'FIELD-NUMERIC-BINARY', 4, 0)
        binary_positive.parse(self.binary_123)
        self.assertEqual(binary_positive.value, {'FIELD-NUMERIC-BINARY' : 123})

        # Testa valor negativo -00123 sem casas decimais
        binary_negative=FieldNumericBinary('NUMERIC_BINARY', 'FIELD-NUMERIC-BINARY', 4, 0)
        binary_negative.parse(self.binary_negative_123)
        self.assertEqual(binary_negative.value, {'FIELD-NUMERIC-BINARY': -123})
        
        # Testa valor 00123 com 2 casas decimais
        binary_positive_decimals = FieldNumericBinary('NUMERIC_BINARY', 'FIELD-NUMERIC-BINARY', 4, 2)
        binary_positive_decimals.parse(self.binary_123)
        self.assertEqual(binary_positive_decimals.value, {'FIELD-NUMERIC-BINARY': Decimal("1.23").quantize(Decimal('1.00'), rounding=ROUND_HALF_UP)})
        
        # Testa valor negativo -00123 com 2 casas decimais
        binary_negative_decimals = FieldNumericBinary('NUMERIC_BINARY', 'FIELD-NUMERIC-BINARY', 4, 2)
        binary_negative_decimals.parse(self.binary_negative_123)
        self.assertEqual(binary_negative_decimals.value, {'FIELD-NUMERIC-BINARY': Decimal("-1.23").quantize(Decimal('1.00'), rounding=ROUND_HALF_UP)})

if __name__ == '__main__':
    unittest.main()