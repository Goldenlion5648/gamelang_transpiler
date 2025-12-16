from functools import partial
from enum import Enum, auto

class TokenType(Enum):
    SECTION_LABEL = auto(),
    FOR_START = auto(),
    IF_CONDITION_START = auto(),
    AND_CONDITIONAL = auto(),
    OR_CONDITIONAL = auto(),
    FUNCTION_START = auto(),
    VARIABLE = auto(),
    STRING = auto(),
    IN = auto(),
    INDENT = auto()
    START_NEW_BLOCK_COLON = auto(),
    DECLARE_VAR_VARIABLE_WORD = auto(),
    DECLARE_LET_VARIABLE_WORD = auto(),
    DECLARE_CONST_VARIABLE_WORD = auto(),
    STATEMENT_SEP = auto(),
    ASSIGN_VALUE = auto(), # =
    FLOAT_NUMBER = auto()
    INT_NUMBER = auto()
    LESS_EQUAL_RANGE = auto()
    LESS_THAN_RANGE = auto()
    OP_ADD = auto()
    OP_SUB = auto()
    OP_MULT = auto()
    OP_DIV = auto()
    COMPARE_EQUALS = auto()
    COMPARE_NOT_EQUALS = auto()
    COMPARE_LESS_THAN = auto()
    COMPARE_GREATER_THAN = auto()
    COMPARE_LESS_THAN_OR_EQUAL_TO = auto()
    COMPARE_GREATER_THAN_OR_EQUAL_TO = auto()
    EXPRESSION_START = auto()
    EXPRESSION_END = auto()

token_type_to_string = {
    TokenType.SECTION_LABEL : "some label",
    TokenType.FOR_START : "for",
    TokenType.IF_CONDITION_START : "if",
    TokenType.AND_CONDITIONAL: "and",
    TokenType.OR_CONDITIONAL: "or",
    TokenType.IN : "in",
    TokenType.FUNCTION_START : "def",
    TokenType.VARIABLE : "some name",
    TokenType.START_NEW_BLOCK_COLON : ":",
    TokenType.DECLARE_VAR_VARIABLE_WORD : "var",
    TokenType.DECLARE_LET_VARIABLE_WORD : "let",
    TokenType.DECLARE_CONST_VARIABLE_WORD : "const",
    TokenType.STATEMENT_SEP : "\\n",
    TokenType.ASSIGN_VALUE : "=",
    TokenType.FLOAT_NUMBER : "some float",
    TokenType.INT_NUMBER : "some int",
    TokenType.LESS_EQUAL_RANGE : "..=",
    TokenType.LESS_THAN_RANGE : "..<",
    TokenType.OP_ADD : "+",
    TokenType.OP_SUB : "-",
    TokenType.OP_MULT : "*",
    TokenType.OP_DIV : "/",
    TokenType.EXPRESSION_START : "(",
    TokenType.EXPRESSION_END : ")",
    TokenType.COMPARE_EQUALS : "==",
    TokenType.COMPARE_NOT_EQUALS : "!=",
    TokenType.COMPARE_LESS_THAN : "<",
    TokenType.COMPARE_GREATER_THAN : ">",
    TokenType.COMPARE_LESS_THAN_OR_EQUAL_TO : "<=",
    TokenType.COMPARE_GREATER_THAN_OR_EQUAL_TO : ">=",
}

NEW_LINE = "\n"

class InvalidNumberException(Exception):
    pass

class MixedWhitespaceException(Exception):
    pass

class InvalidRangeException(Exception):
    pass

class Token:
    def __init__(self, type_: TokenType, line_and_col, value=None):
        self.type = type_
        self.line_and_col = line_and_col
        if value is None:
            self.value = token_type_to_string[type_]
        else:
            self.value = value

    def __repr__(self):
        return f"{self.type} {self.value} "

class Tokenizer:
    def __init__(self, text: str):
        self.pos = 0
        self.text = text
        self.keywords_to_token = {
            "let": partial(Token, TokenType.DECLARE_LET_VARIABLE_WORD),
            "var": partial(Token, TokenType.DECLARE_VAR_VARIABLE_WORD),
            "in": partial(Token, TokenType.IN),
            "for": partial(Token, TokenType.FOR_START),
            "if": partial(Token, TokenType.IF_CONDITION_START),
        }
        self.result: list[Token] = []
        self.spaces_count_stack = []
        self.is_in_block = False
        self.column_number = 0
        self.line_number = 1

        self.parse()
    
    def in_bounds(self):
        return self.pos < len(self.text)
    
    def get_cur_line_and_col(self):
        return (self.line_number, self.column_number)

    def eat_whitespace(self):
        self.spaces_count_stack.append(0)
        is_in_spaces_mode = None
        while self.in_bounds() and (cur := self.consume_char()).isspace():
            if cur == '\t':
                if is_in_spaces_mode == True:
                    raise MixedWhitespaceException("Mixed use of spaces and tabs")
                is_in_spaces_mode = False
            elif cur == ' ':
                if is_in_spaces_mode == False:
                    raise MixedWhitespaceException("Mixed use of spaces and tabs")
                is_in_spaces_mode = True

            if cur == '\n':
                self.result.append(self.make_token_of(TokenType.STATEMENT_SEP))
            else:
                self.spaces_count_stack[-1] += 1
                
        if not self.in_bounds():
            self.step_back()
            
        if self.spaces_count_stack[-1] > 0 and self.result and self.result[-1].type in [TokenType.STATEMENT_SEP, TokenType.START_NEW_BLOCK_COLON]:
            self.result.append(self.make_token_of(TokenType.INDENT, self.spaces_count_stack[-1]))
        else:
            self.spaces_count_stack.pop()

    
    def get_cur_char(self):
        return self.text[self.pos]
    
    def consume_char(self):
        ret = self.text[self.pos]
        if ret == NEW_LINE:
            self.line_number += 1
            self.column_number = 0
        else:
            self.column_number += 1
        self.pos += 1
        return ret

    def step_back(self):
        self.pos -= 1

    def make_token_of(self, type_, value=None):
        return Token(type_, self.get_cur_line_and_col(), value)

    def eat_number(self, start=None):
        current = []
        if start is not None:
            current = start
        dots_seen = 0
        #TODO: this currently allows 4234asd
        while self.in_bounds():
            char = self.consume_char()
            if char.isdigit():
                current.append(char)
            elif char == "_":
                continue
            elif char == ".":
                if len(current) == 0 or dots_seen == 0:
                    dots_seen += 1
                    current.append(char)
                elif dots_seen == 1:
                    # 4..12
                    self.step_back()
                    # removes first dot
                    current.pop()
                    return self.make_token_of(TokenType.INT_NUMBER, "".join(current))
            else:
                self.step_back()
                break
        if dots_seen == 1:
            return self.make_token_of(TokenType.FLOAT_NUMBER, "".join(current))
        return self.make_token_of(TokenType.INT_NUMBER, "".join(current))

    def eat_word(self, start=None):
        current = []
        if start is not None:
            current = start
        while self.in_bounds():
            char = self.consume_char()
            if char.isalpha() or char.isdigit() or char == "_":
                current.append(char)
            else:
                break
        if self.pos != len(self.text):
            self.step_back()
        return "".join(current)

    def read_string(self):
        char = self.consume_char()
        current = [char]
        while self.in_bounds() and (char := self.consume_char()) not in ['"', "'"]:
            current.append(char)
        if not self.in_bounds():
            raise Exception("UnexpectedStringEnd")
        if char in ['"', "'"]:
            return self.make_token_of(TokenType.STRING, "".join(current))
            
        
    def eat_symbols(self, start=None):
        current = []
        if start is not None:
            current = start
        while self.in_bounds():
            char = self.consume_char()
            if char == "=":
                second_char = self.consume_char()
                if second_char == "=":
                    return self.make_token_of(TokenType.COMPARE_EQUALS)
                return self.make_token_of(TokenType.ASSIGN_VALUE)
            elif char == "'" or char == '"':
                return self.read_string()
            elif char == "(":
                return self.make_token_of(TokenType.EXPRESSION_START)
            elif char == ")":
                return self.make_token_of(TokenType.EXPRESSION_END)
            elif char == "+":
                return self.make_token_of(TokenType.OP_ADD)
            elif char == "-":
                return self.make_token_of(TokenType.OP_SUB)
            elif char == "*":
                return self.make_token_of(TokenType.OP_MULT)
            elif char == "/":
                return self.make_token_of(TokenType.OP_DIV)
            elif char == ":":
                return self.make_token_of(TokenType.START_NEW_BLOCK_COLON)
            elif char == ".":
                second_char = self.consume_char()
                if second_char == ".":
                    third_char = self.consume_char() 
                    if third_char == "=":
                        return self.make_token_of(TokenType.LESS_EQUAL_RANGE)
                    elif third_char == "<":
                        return self.make_token_of(TokenType.LESS_THAN_RANGE)
                    raise InvalidRangeException(f"Range cant end in {third_char}")
                else:
                    return self.eat_number(["."])
            elif char == "<":
                second_char = self.consume_char()
                if second_char == "=":
                    return self.make_token_of(TokenType.COMPARE_LESS_THAN_OR_EQUAL_TO)
                else:
                    self.step_back()
                    return self.make_token_of(TokenType.COMPARE_LESS_THAN)
            elif char == ">":
                second_char = self.consume_char()
                if second_char == "=":
                    return self.make_token_of(TokenType.COMPARE_GREATER_THAN_OR_EQUAL_TO)
                else:
                    self.step_back()
                    return self.make_token_of(TokenType.COMPARE_GREATER_THAN)

        if self.pos != len(self.text):
            self.step_back()
        return "".join(current)


    def parse(self):
        while self.in_bounds():
            self.eat_whitespace()
        
            cur_char = self.get_cur_char()
            if cur_char.isdigit():
                self.result.append(self.eat_number())
            elif cur_char.isalpha():
                word_found = self.eat_word()
                if word_found in self.keywords_to_token:
                    self.result.append(self.keywords_to_token[word_found]())
                else:
                    self.result.append(self.make_token_of(TokenType.VARIABLE, word_found))
            else:
                self.result.append(self.eat_symbols())

        for part in self.result:
            print(part)
        
        return self.result

class TokenConsumer:
    
    def __init__(self, tokens):
        self.tokens = tokens.copy()
        self.pos = 0
        self.output = []
        
    def consume_token(self) -> Token:
        ret = None
        if self.pos < len(self.tokens):
            ret = self.tokens[self.pos]
            self.pos += 1
        return ret
        
    def in_bounds(self):
        return self.pos < len(self.tokens)

    def eat_variable_creation(self):
        result = []
        first_3 = self.tokens[self.pos:self.pos+3]
        match first_3:
            case [
                TokenType.DECLARE_VAR_VARIABLE_WORD, TokenType.VARIABLE, 
                TokenType.ASSIGN_VALUE
            ]:
                #TODO: come back to handle paren
                stack = []
                while stack or (cur := self.consume_token()).type != TokenType.STATEMENT_SEP:
                    result.append(cur.value)
                return " ".join(result)
            case _:
                raise Exception(f"unknown variable creation syntax: {first_3}")


    def eat_if_condition(self):
        result = []
        while self.in_bounds() and (cur := self.consume_token()).type != TokenType.START_NEW_BLOCK_COLON:
            result.append(cur)
        if self.in_bounds():
            result.append(cur)
        else:
            raise

        return " ".join(result)

    def make_output(self):
        while self.pos < len(self.tokens):
            token = self.tokens[self.pos]
            match token.type:
                case TokenType.DECLARE_VAR_VARIABLE_WORD:
                    self.output.append(self.eat_variable_creation())
                case TokenType.IF_CONDITION_START:
                    self.output.append(self.eat_if_condition())
                    


def run_tests():
    assert Tokenizer("for x in 0 ..< 12:")

if __name__ == "__main__":

    # with open("loop_test.gamelang") as f:
    #     data = f.read().strip()
    #     runner = Tokenizer(data)

    # print("\n\n")
    # with open("nesting_test.gamelang") as f:
    #     data = f.read().strip()
    #     runner = Tokenizer(data)

    with open("comparisons_test.gamelang") as f:
        data = f.read().strip()
        runner = Tokenizer(data)


    



