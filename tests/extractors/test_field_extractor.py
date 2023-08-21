import unittest
import sys

sys.path.append("./src/")
sys.path.append("./src/coboljsonifier/")

from coboljsonifier.extractors.book_item import BookItem
from coboljsonifier.extractors.field_extractor import   FieldEmpty, FieldSimpleNumeric1Decimals2, FieldSimpleNumericDecimals1, FieldSimpleNumericDecimals2, \
                                                        FieldSignalNumeric1Decimals3, FieldSimpleNumeric1Decimals2, FieldSignalNumeric1Decimals1, \
                                                        FieldSignalNumeric1Decimals2, FieldNumericMasked1, FieldAlphabetic, FieldAlphanumeric, FieldUndefined


class TestFieldExtractor(unittest.TestCase):

    # 00
    bookitem_empty = BookItem()
    simple_numeric = BookItem()
    simple_numeric.type = "NUMERIC"
    simple_numeric.level = "3"
    simple_numeric.name = "BOOK-SIMPLE-NUMERIC"
    simple_numeric.format = "999"
   
    # 02
    simple_numeric_decimals1 = BookItem( )
    simple_numeric_decimals1.type = "NUMERIC"
    simple_numeric_decimals1.level = "3"
    simple_numeric_decimals1.name = "BOOK-SIMPLE-NUMERIC-DECIMALS1"
    simple_numeric_decimals1.format = "999V99"

    # 03
    simple_numeric_decimals2 = BookItem( )
    simple_numeric_decimals2.type = "NUMERIC"
    simple_numeric_decimals2.level = "3"
    simple_numeric_decimals2.name = "BOOK-SIMPLE-NUMERIC-DECIMALS2"
    simple_numeric_decimals2.format = "999V9(2)"

    # 06
    simple_numeric1_decimals2 = BookItem( )
    simple_numeric1_decimals2.type = "NUMERIC"
    simple_numeric1_decimals2.level = "3"
    simple_numeric1_decimals2.name = "BOOK-SIMPLE-NUMERIC1-DECIMALS2"
    simple_numeric1_decimals2.format = "9(3)V9(2)"

    # 08
    signal_numeric1_decimals1 = BookItem( )
    signal_numeric1_decimals1.type = "NUMERIC"
    signal_numeric1_decimals1.level = "3"
    signal_numeric1_decimals1.name = "BOOK-SIGNAL-NUMERIC1-DECIMALS1"
    signal_numeric1_decimals1.format = "S9(3)V99"

    # 09
    signal_numeric1_decimals2 = BookItem( )
    signal_numeric1_decimals2.type = "NUMERIC"
    signal_numeric1_decimals2.level = "3"
    signal_numeric1_decimals2.name = "BOOK-SIGNAL-NUMERIC1-DECIMALS2"
    signal_numeric1_decimals2.format = "S9(3)V9(2)"

    # 10
    signal_numeric1_decimals3 = BookItem( )
    signal_numeric1_decimals3.type = "NUMERIC"
    signal_numeric1_decimals3.level = "3"
    signal_numeric1_decimals3.name = "BOOK-SIGNAL-NUMERIC1-DECIMALS3"
    signal_numeric1_decimals3.format = "S9(10)V"

    # A01
    signal_numeric_masked1 = BookItem( )
    signal_numeric_masked1.type = "NUMERIC"
    signal_numeric_masked1.level = "3"
    signal_numeric_masked1.name = "BOOK-SIGNAL-NUMERIC-MASKED1"
    signal_numeric_masked1.format = "+99999999999999.99"

    # A02
    alphabetic = BookItem( )
    alphabetic.type = "NUMERIC"
    alphabetic.level = "3"
    alphabetic.name = "BOOK-ALPHABETIC"
    alphabetic.format = "A(12)"

    # A03
    alphanumeric = BookItem( )
    alphanumeric.type = "NUMERIC"
    alphanumeric.level = "3"
    alphanumeric.name = "BOOK-ALPHANUMERIC"
    alphanumeric.format = "X(12)"


    # 00
    def test_field_empty_ok(self):
        self.assertEqual(FieldEmpty().extract(self.bookitem_empty), self.bookitem_empty)

    # 02
    def test_field_simple_numeric_decimals1(self):
        self.assertEqual(FieldSimpleNumericDecimals1().extract(self.simple_numeric_decimals1).size, 5)
        self.assertEqual(FieldSimpleNumericDecimals1().extract(self.simple_numeric_decimals1).decimals, 2)

    # 03
    def test_field_simple_numeric_decimals2(self):
        self.assertEqual(FieldSimpleNumericDecimals2().extract(self.simple_numeric_decimals2).size, 5)
        self.assertEqual(FieldSimpleNumericDecimals2().extract(self.simple_numeric_decimals2).decimals, 2)

    # 06
    def test_field_simple_numeric1_decimals2(self):
        self.assertEqual(FieldSimpleNumeric1Decimals2().extract(self.simple_numeric1_decimals2).size, 5)
        self.assertEqual(FieldSimpleNumeric1Decimals2().extract(self.simple_numeric1_decimals2).decimals, 2)

    # 08
    def test_field_signal_numeric1_decimals1(self):
        self.assertEqual(FieldSignalNumeric1Decimals1().extract(self.signal_numeric1_decimals1).size, 5)
        self.assertEqual(FieldSignalNumeric1Decimals1().extract(self.signal_numeric1_decimals1).decimals, 2)

    # 09
    def test_field_signal_numeric1_decimals2(self):
        self.assertEqual(FieldSignalNumeric1Decimals2().extract(self.signal_numeric1_decimals2).size, 5)
        self.assertEqual(FieldSignalNumeric1Decimals2().extract(self.signal_numeric1_decimals2).decimals, 2)

    # 10
    def test_field_signal_numeric1_decimals3(self):
        self.assertEqual(FieldSignalNumeric1Decimals3().extract(self.signal_numeric1_decimals3).size, 10)
        self.assertEqual(FieldSignalNumeric1Decimals3().extract(self.signal_numeric1_decimals3).decimals, 0)

    # A01
    def test_field_numeric_masked1(self):
        self.assertEqual(FieldNumericMasked1().extract(self.signal_numeric_masked1).size, 18)
        self.assertEqual(FieldNumericMasked1().extract(self.signal_numeric_masked1).decimals, 2)


    # A02
    def test_field_alphabetic(self):
        self.assertEqual(FieldAlphabetic().extract(self.alphabetic).size, 12)
        self.assertEqual(FieldAlphabetic().extract(self.alphabetic).decimals, 0)

    # A03
    def test_field_alphanumeric(self):
        self.assertEqual(FieldAlphanumeric().extract(self.alphanumeric).size, 12)
        self.assertEqual(FieldAlphanumeric().extract(self.alphanumeric).decimals, 0)

    # 999
    def test_field_undefined(self):
        self.assertRaises(Exception, FieldUndefined().extract, self.bookitem_empty)

if __name__ == '__main__':
    unittest.main()