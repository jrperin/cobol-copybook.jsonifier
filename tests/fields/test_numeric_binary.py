import unittest
from decimal import Decimal
from coboljsonifier.fields.field_numeric_binary import FieldNumericBinary


class TestFieldNumericBinary(unittest.TestCase):


    binary_123 = b'\x00\x00\x00\x7B'            # ->  00123
    binary_negative_123 = b'\xFF\xFF\xFF\x85'   # -> -00123


    def test_parsing(self):

        # Testing value 00123 without decimal places
        binary_positive=FieldNumericBinary('NUMERIC_BINARY', 'FIELD-NUMERIC-BINARY', 4, 0)
        binary_positive.parse(self.binary_123)
        self.assertEqual(binary_positive.value, {'FIELD-NUMERIC-BINARY' : 123})

        # Testing negative value -00123 without decimal places
        binary_negative=FieldNumericBinary('NUMERIC_BINARY', 'FIELD-NUMERIC-BINARY', 4, 0)
        binary_negative.parse(self.binary_negative_123)
        self.assertEqual(binary_negative.value, {'FIELD-NUMERIC-BINARY': -123})
        
        # Testing value 00123 with 2 decimal places
        binary_positive_decimals = FieldNumericBinary('NUMERIC_BINARY', 'FIELD-NUMERIC-BINARY', 4, 2)
        binary_positive_decimals.parse(self.binary_123)
        self.assertEqual(binary_positive_decimals.value, {'FIELD-NUMERIC-BINARY': Decimal('1.23')})
        
        # Testing negative value -00123 with 2 decimal places
        binary_negative_decimals = FieldNumericBinary('NUMERIC_BINARY', 'FIELD-NUMERIC-BINARY', 4, 2)
        binary_negative_decimals.parse(self.binary_negative_123)
        self.assertEqual(binary_negative_decimals.value, {'FIELD-NUMERIC-BINARY': Decimal("-1.23")})

if __name__ == '__main__':
    unittest.main()