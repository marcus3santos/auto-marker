import unittest
from Parser import parse


class TestParse(unittest.TestCase):
    def test_leaf(self):
        expr = ['2']
        expectedResult = ['2', [], []]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_regular(self):
        expr = ['3', '/', '6', '-', '9']
        expectedResult = ['-', ['/', ['3', [], []], ['6', [], []]], ['9', [], []]]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_regular2(self):
        expr = ['4', '+', '3', '*', '7']
        expectedResult = ['+', ['4', [], []], ['*', ['3', [], []], ['7', [], []]]]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_extrabracket(self):
        expr = [['4'], '+', ['3'], '+', '6']
        expectedResult = ['+', ['+', ['4', [], []], ['3', [], []]], ['6', [], []]]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_bracket(self):
        expr = [['4', '+', '3'], '*', '7']
        expectedResult = ['*', ['+', ['4', [], []], ['3', [], []]], ['7', [], []]]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_bracket2(self):
        expr = [['4', '+', '3'], '*', ['7', '*', '6']]
        expectedResult = ['*', ['+', ['4', [], []], ['3', [], []]], ['*', ['7', [], []], ['6', [], []]]]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_multibracket(self):
        expr = [[['4', '+', '3'], '*', ['7', '*', '6']]]
        expectedResult = ['*', ['+', ['4', [], []], ['3', [], []]], ['*', ['7', [], []], ['6', [], []]]]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_allops(self):
        expr = [['4', '+', '3'], '*', [['7', '*', '6'], '/', ['3', '-', '5']]]
        expectedResult = ['*', ['+', ['4', [], []], ['3', [], []]], ['/', ['*', ['7', [], []], ['6', [], []]],
                                                                     ['-', ['3', [], []], ['5', [], []]]]]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_allops2(self):
        expr = ['4', '+', '3', '*', '7', '-', '5', '/', ['3', '+', '4'], '+', '6']
        expectedResult = ['+', ['-', ['+', ['4', [], []], ['*', ['3', [], []], ['7', [], []]]],
                                ['/', ['5', [], []], ['+', ['3', [], []], ['4', [], []]]]], ['6', [], []]]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_factorial(self):
        expr = ['2', '!']
        expectedResult = ['!', ['2', [], []], []]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_factorial2(self):
        expr = ['4', '+', ['3', '!']]
        expectedResult = ['+', ['4', [], []], ['!', ['3', [], []], []]]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_factorial3(self):
        expr = ['3', '/', ['6', '!'], '-', '9']
        expectedResult = ['-', ['/', ['3', [], []], ['!', ['6', [], []], []]], ['9', [], []]]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_factorialexpr(self):
        expr = ['4', '+', [['3', '+', '1'], '!']]
        expectedResult = ['+', ['4', [], []], ['!', ['+', ['3', [], []], ['1', [], []]], []]]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_twoFactorial(self):
        expr = [['3', '!'], '!']
        expectedResult = ['!', ['!', ['3', [], []], []], []]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)

    def test_complexFactorial(self):
        expr = [[[['2', '*', '3'], '!'], '!'], '/', '5']
        expectedResult = ['/', ['!', ['!', ['*', ['2', [], []], ['3', [], []]], []], []], ['5', [], []]]
        generatedResult = parse(expr)
        self.assertEqual(expectedResult, generatedResult)


if __name__ == '__main__':
    unittest.main()
