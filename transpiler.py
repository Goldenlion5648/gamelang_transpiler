from functools import partial
from enum import Enum, auto
from collections import defaultdict, deque
import os
import sys
import re

SHOUD_SHOW_ORIGINAL_LINE_AND_COL = True


class TokenType(Enum):
    FOR_START = auto(),
    IF_CONDITION_START = auto(),
    ELSE_CONDITION_START = auto(),
    ELIF_CONDITION_START = auto(),
    NOT_CONDITIONAL = auto(),
    AND_CONDITIONAL = auto(),
    OR_CONDITIONAL = auto(),
    FUNCTION_START = auto(),
    STRUCT_START = auto(),
    STRUCT_INSTANCE_OPEN_CURLY = auto(),
    STRUCT_INSTANCE_CLOSE_CURLY = auto(),
    SECTION_START = auto(),
    VARIABLE = auto(),
    STRING = auto(),
    IN = auto(),
    LINE_COMMENT = auto(),
    ARGUMENT_SEPARATOR_COMMA = auto(),

    RAW_INSERT_LITERAL = auto()
    INDENT = auto()
    RETURN_TYPE_ARROW = auto(),
    TYPE_DEFINITION_COLON = auto(),
    START_NEW_BLOCK_COLON = auto(),
    DECLARE_VAR_VARIABLE_WORD = auto(),
    DECLARE_LET_VARIABLE_WORD = auto(),
    DECLARE_CONST_VARIABLE_WORD = auto(),
    STATEMENT_SEP_NEW_LINE = auto(),
    ASSIGN_VALUE = auto(), # =
    FLOAT_NUMBER = auto()
    INT_NUMBER = auto()
    LESS_EQUAL_RANGE = auto()
    LESS_THAN_RANGE = auto()
    OP_ADD = auto()
    OP_SUB = auto()
    OP_MULT = auto()
    OP_DIV = auto()
    EMPTY_LINE_PASS = auto()
    COMPARE_EQUALS = auto()
    COMPARE_NOT_EQUALS = auto()
    COMPARE_LESS_THAN = auto()
    COMPARE_GREATER_THAN = auto()
    COMPARE_LESS_THAN_OR_EQUAL_TO = auto()
    COMPARE_GREATER_THAN_OR_EQUAL_TO = auto()
    EXPRESSION_START_OPEN_PAREN = auto()
    EXPRESSION_END_CLOSE_PAREN = auto()

token_type_to_string = {
    TokenType.FOR_START : "for",
    TokenType.IF_CONDITION_START : "if",
    TokenType.FUNCTION_START : "def",
    TokenType.STRUCT_START : "struct",
    TokenType.ELSE_CONDITION_START : "else",
    TokenType.ELIF_CONDITION_START : "else if",
    TokenType.AND_CONDITIONAL: "&&",
    TokenType.OR_CONDITIONAL: "||",
    TokenType.NOT_CONDITIONAL: "!",
    TokenType.IN : "in",
    TokenType.EMPTY_LINE_PASS : "pass",
    TokenType.VARIABLE : "some name",
    TokenType.START_NEW_BLOCK_COLON : ":",
    TokenType.TYPE_DEFINITION_COLON : ":",
    TokenType.RETURN_TYPE_ARROW : "->",
    TokenType.DECLARE_VAR_VARIABLE_WORD : "var",
    TokenType.DECLARE_LET_VARIABLE_WORD : "let",
    TokenType.DECLARE_CONST_VARIABLE_WORD : "const",
    TokenType.DECLARE_CONST_VARIABLE_WORD : "some raw",
    TokenType.STATEMENT_SEP_NEW_LINE : "\\n",
    TokenType.ASSIGN_VALUE : "=",
    TokenType.FLOAT_NUMBER : "some float",
    TokenType.INT_NUMBER : "some int",
    TokenType.LESS_EQUAL_RANGE : "..=",
    TokenType.LESS_THAN_RANGE : "..<",
    TokenType.OP_ADD : "+",
    TokenType.ARGUMENT_SEPARATOR_COMMA : ",",
    TokenType.SECTION_START : "section",
    TokenType.OP_SUB : "-",
    TokenType.OP_MULT : "*",
    TokenType.OP_DIV : "/",
    TokenType.STRUCT_INSTANCE_OPEN_CURLY : "{",
    TokenType.STRUCT_INSTANCE_CLOSE_CURLY : "}",
    TokenType.EXPRESSION_START_OPEN_PAREN : "(",
    TokenType.EXPRESSION_END_CLOSE_PAREN : ")",
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
        return f"{self.line_and_col} {self.type} {self.value} "
    
    def transpiled_value(self):
        ret = None
        match self.type:
            case TokenType.INDENT:
                ret = " " * self.value
            case TokenType.START_NEW_BLOCK_COLON:
                ret = " {"
            case TokenType.STATEMENT_SEP_NEW_LINE:
                ret = "\n"
            case TokenType.EMPTY_LINE_PASS:
                ret = ""
            case TokenType.FUNCTION_START:
                ret = "proc"
            case TokenType.STRING:
                ret = f'"{self.value}"'
            case TokenType.LINE_COMMENT:
                ret = f'//{self.value}'
            case TokenType.VARIABLE:
                match self.value:
                    case "print":
                        ret = f'fmt.println'
                    case "return":
                        ret = f'return '
                    case _:
                        ret = f'{self.value}'
            case TokenType.SECTION_START:
                ret = ""
            case _:
                ret = f"{self.value}"
        return ret

def join_tokens_to_string(tokens, sep=" ") -> str:
    return sep.join(x.transpiled_value() for x in tokens)

get_transpiled_value_for_tokens = join_tokens_to_string

class Tokenizer:
    def __init__(self, text: str):
        self.pos = 0
        self.text = text
        self.keywords_to_token_type = {
            "let": TokenType.DECLARE_LET_VARIABLE_WORD,
            "var": TokenType.DECLARE_VAR_VARIABLE_WORD,
            "in": TokenType.IN,
            "for": TokenType.FOR_START,
            "and": TokenType.AND_CONDITIONAL,
            "or": TokenType.OR_CONDITIONAL,
            "not": TokenType.NOT_CONDITIONAL,
            "while": TokenType.FOR_START,
            "pass": TokenType.EMPTY_LINE_PASS,
            "section": TokenType.SECTION_START,
            "def": TokenType.FUNCTION_START,
            "struct": TokenType.STRUCT_START,
            "if": TokenType.IF_CONDITION_START,
            "else": TokenType.ELSE_CONDITION_START,
            "elif": TokenType.ELIF_CONDITION_START,
        }
        self.expression_depth = 0
        self.result: list[Token] = []
        self.is_in_block = False
        # self.column_number = 1
        # self.line_number = 1
        self.pos_to_line_and_col = {}
        cur_line = 1
        col = 1
        self.text += '''
def main:
    using rl
    init()
    while should_run and not WindowShouldClose():
        update()
    CloseWindow()\n\n
'''
        self.text = "\n".join(line.rstrip() for line in self.text.splitlines())
        for i, char in enumerate(self.text):
            if char == NEW_LINE:
                self.pos_to_line_and_col[i] = (cur_line, col)
                cur_line += 1
                col = 1
            else:
                self.pos_to_line_and_col[i] = (cur_line, col)
                col += 1
        for i in range(len(self.text), len(self.text)+10):
            self.pos_to_line_and_col[i] = (cur_line, col)
        print(self.pos_to_line_and_col)

        self.parse()
    
    def in_bounds(self):
        return self.pos < len(self.text)
    
    def get_cur_line_and_col(self):
        return self.pos_to_line_and_col[self.pos]

    def get_cur_char(self):
        return self.text[self.pos]

    def peek_next(self):
        if self.in_bounds():
            return self.text[self.pos+1]
        return None
    
    def consume_char(self):
        ret = self.text[self.pos]
        
        self.pos += 1
        return ret

    def step_back(self):
        self.pos -= 1
        

    def make_token_of(self, type_, value=None, line_col_override=None):
        line_and_col = self.get_cur_line_and_col()
        if line_col_override is not None:
            line_and_col = line_col_override
        return Token(type_, line_and_col, value)

    def eat_whitespace(self):
        spaces_count = 0
        is_in_spaces_mode = None
        token_start = self.get_cur_line_and_col()
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
                self.result.append(self.make_token_of(TokenType.STATEMENT_SEP_NEW_LINE, line_col_override=token_start))
                token_start = self.get_cur_line_and_col()
                spaces_count = 0
            else:
                spaces_count += 1

        if self.in_bounds():
            self.step_back()
            
        if spaces_count > 0 and self.result and self.result[-1].type in [TokenType.STATEMENT_SEP_NEW_LINE, TokenType.START_NEW_BLOCK_COLON]:
            self.result.append(self.make_token_of(TokenType.INDENT, spaces_count, line_col_override=token_start))
        # else:
        #     spaces_count.pop()


    def eat_number(self, start=None):
        current = []
        if start is not None:
            current = start
        dots_seen = 0
        token_start = self.get_cur_line_and_col()
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
                    self.step_back()
                    # removes first dot
                    current.pop()
                    return self.make_token_of(TokenType.INT_NUMBER, "".join(current), token_start)
            else:
                self.step_back()
                break
        if dots_seen == 1:
            return self.make_token_of(TokenType.FLOAT_NUMBER, "".join(current), token_start)
        return self.make_token_of(TokenType.INT_NUMBER, "".join(current), token_start)

    def eat_word(self):
        current = []
        token_start = self.get_cur_line_and_col()

        while self.in_bounds():
            char = self.consume_char()
            if char.isalpha() or char.isdigit() or char == "_":
                current.append(char)
            else:
                break
        if self.pos != len(self.text):
            self.step_back()
        word_found = "".join(current)
        if word_found in self.keywords_to_token_type:
            new_token = self.make_token_of(self.keywords_to_token_type[word_found], line_col_override=token_start)
            return new_token
            # if new_token.type == TokenType.FUNCTION_START:
            #     return self.make_token_of(TokenType.STRING, self.consume_until(["'", '"']))
            # else:
        else:
            return self.make_token_of(TokenType.VARIABLE, word_found, line_col_override=token_start)

    def consume_until(self, terminator_strings : list, consume_ending=True) -> str:
        current = []
        matches_found = 0
        while matches_found == 0:
            for string in terminator_strings:
                window=self.text[self.pos:self.pos+len(string)]
                if len(window) != len(string):
                    continue
                if window == string:
                    if consume_ending:
                        self.pos += len(string)
                    matches_found += 1
                    break
            if not self.in_bounds() or matches_found > 0:
                break
            current.append(self.consume_char())

        if not self.in_bounds():
            raise Exception(f"UnexpectedStringEnd, {''.join(current)}")
        return "".join(current).strip("\n")
        # if char in terminator_strings:
            # return self.make_token_of(TokenType.STRING, "".join(current))

    def eat_symbols(self, start=None):
        token_start = self.get_cur_line_and_col()
        while self.in_bounds():
            char = self.consume_char()
            if char == "=":
                second_char = self.consume_char()
                if second_char == "=":
                    return self.make_token_of(TokenType.COMPARE_EQUALS, line_col_override=token_start)
                return self.make_token_of(TokenType.ASSIGN_VALUE, line_col_override=token_start)
            elif char == "'" or char == '"':
                return self.make_token_of(TokenType.STRING, self.consume_until(["'", '"']))
            elif char == "{":
                self.expression_depth += 1
                return self.make_token_of(TokenType.STRUCT_INSTANCE_OPEN_CURLY, line_col_override=token_start)
            elif char == "}":
                self.expression_depth -= 1
                return self.make_token_of(TokenType.STRUCT_INSTANCE_CLOSE_CURLY, line_col_override=token_start)
            elif char == "(":
                self.expression_depth += 1
                return self.make_token_of(TokenType.EXPRESSION_START_OPEN_PAREN, line_col_override=token_start)
            elif char == ")":
                self.expression_depth -= 1
                return self.make_token_of(TokenType.EXPRESSION_END_CLOSE_PAREN, line_col_override=token_start)
            elif char == "#":
                return self.make_token_of(TokenType.LINE_COMMENT, self.consume_until(["\n"], False))
            elif char == "+":
                return self.make_token_of(TokenType.OP_ADD, line_col_override=token_start)
            elif char == ",":
                return self.make_token_of(TokenType.ARGUMENT_SEPARATOR_COMMA, line_col_override=token_start)
            elif char == "-":
                if self.get_cur_char() == ">":
                    self.consume_char()
                    return self.make_token_of(TokenType.RETURN_TYPE_ARROW, line_col_override=token_start)
                else:
                    return self.make_token_of(TokenType.OP_SUB, line_col_override=token_start)
            elif char == "*":
                if self.get_cur_char() == "*":
                    self.consume_char()
                    return self.make_token_of(TokenType.RAW_INSERT_LITERAL, 
                    self.consume_until(["**"]),
                    line_col_override=token_start)
                else:
                    return self.make_token_of(TokenType.OP_MULT, line_col_override=token_start)
            elif char == "/":
                return self.make_token_of(TokenType.OP_DIV, line_col_override=token_start)
            elif char == ":":
                cur = self.get_cur_char()
                if cur == NEW_LINE:
                    return self.make_token_of(TokenType.START_NEW_BLOCK_COLON, line_col_override=token_start)
                else:
                    return self.make_token_of(TokenType.TYPE_DEFINITION_COLON, line_col_override=token_start)
            elif char == ".":
                second_char = self.consume_char()
                if second_char == ".":
                    third_char = self.consume_char() 
                    if third_char == "=":
                        return self.make_token_of(TokenType.LESS_EQUAL_RANGE, line_col_override=token_start)
                    elif third_char == "<":
                        return self.make_token_of(TokenType.LESS_THAN_RANGE, line_col_override=token_start)
                    raise InvalidRangeException(f"Range cant end in {third_char}")
                else:
                    return self.eat_number([".", second_char])
            elif char == "<":
                second_char = self.consume_char()
                if second_char == "=":
                    return self.make_token_of(TokenType.COMPARE_LESS_THAN_OR_EQUAL_TO, line_col_override=token_start)
                else:
                    self.step_back()
                    return self.make_token_of(TokenType.COMPARE_LESS_THAN, line_col_override=token_start)
            elif char == ">":
                second_char = self.consume_char()
                if second_char == "=":
                    return self.make_token_of(TokenType.COMPARE_GREATER_THAN_OR_EQUAL_TO, line_col_override=token_start)
                else:
                    self.step_back()
                    return self.make_token_of(TokenType.COMPARE_GREATER_THAN, line_col_override=token_start)
            else:
                return self.make_token_of(TokenType.RAW_INSERT_LITERAL, char, line_col_override=token_start)


        # if self.pos != len(self.text):
        #     self.step_back()
        # return "".join(current)


    def parse(self):
        while self.in_bounds():
            self.eat_whitespace()
            if not self.in_bounds():
                break
            cur_char = self.get_cur_char()
            if cur_char.isdigit():
                self.result.append(self.eat_number())
            elif cur_char.isalpha():
                self.result.append(self.eat_word())
            else:
                self.result.append(self.eat_symbols())

        with open("tokens.txt", 'w') as f:
            for part in self.result:
                print(part)
                print(part, file=f)

        
        return self.result

GLOBAL_SECTION = "globals"
INIT_SECTION = "init"
UPDATE_SECTION = "update"
UPDATE_SECTION_END = "update_end"

DEFAULT_SECTIONS = [
    GLOBAL_SECTION,
    INIT_SECTION,
    UPDATE_SECTION,
    UPDATE_SECTION_END,
]

def get_marker_name_for(section):
    return section + "_MARKER"

marker_to_section_name = {get_marker_name_for(x) : x for x in DEFAULT_SECTIONS}

class SectionType(Enum):
    GLOBAL_SECTION = auto()
    FUNCTION = auto()
    STRUCT = auto()
    PAREN_EXPRESSION = auto()
    CURLY_EXPRESSION = auto()

class Section:
    def __init__(self, section_type, name):
        self.section_type = section_type
        self.name = name



class Variable:
    def __init__(self, name, value):
        self.name = name
        self.value = value


    @property
    def type(self):
        if re.fullmatch(r"((\d+)?\.\d+)|(\d+?\.(\d+))", self.value):
            return "f32"
        if re.fullmatch(r"\d+", self.value):
            return "int"
        if re.fullmatch('".+"', self.value):
            return "string"
        if re.fullmatch('true|false', self.value):
            return "bool"
        

class TokenConsumer:
    
    def __init__(self, tokens: list[Token], is_main=True):
        self.tokens = self.get_clean_tokens(tokens)
        self.pos = 0
        self.indent_stack = [0]
        self.output = []
        self.variable_was_declared_on_line = False
        self.section_to_data: dict[str, deque[str]] = defaultdict(deque)
        self.current_section_stack: list[Section] = [Section(SectionType.GLOBAL_SECTION, GLOBAL_SECTION)]
        self.is_main = is_main
        # self.global_variables

        # self.section_to_data[INIT_SECTION].append()
    def get_clean_tokens(self, tokens_to_clean: list[Token]):
        seen_stack = []
        new_tokens = []
        for token in tokens_to_clean:
            match token.type:
                case TokenType.STRUCT_INSTANCE_OPEN_CURLY:
                    seen_stack.append(TokenType.STRUCT_INSTANCE_OPEN_CURLY)
                case TokenType.STRUCT_INSTANCE_CLOSE_CURLY:
                    if not seen_stack or seen_stack[-1] != TokenType.STRUCT_INSTANCE_OPEN_CURLY:
                        raise Exception(f"Closing curly with no opening {token.line_and_col}")
                    else:
                        seen_stack.pop()
                case TokenType.EXPRESSION_START_OPEN_PAREN:
                    seen_stack.append(TokenType.EXPRESSION_START_OPEN_PAREN)
                case TokenType.EXPRESSION_END_CLOSE_PAREN:
                    if not seen_stack or seen_stack[-1] != TokenType.EXPRESSION_START_OPEN_PAREN:
                        raise Exception(f"Closing parenthesis with no opening {token.line_and_col}")
                    else:
                        seen_stack.pop()
                case TokenType.STATEMENT_SEP_NEW_LINE | TokenType.INDENT:
                    if seen_stack:
                        continue
            new_tokens.append(token)
        with open("tokens.txt", 'w') as f:
            for part in new_tokens:
                print(part, file=f)
        return new_tokens


        
    def consume_token(self) -> Token:
        ret = None
        if self.pos < len(self.tokens):
            ret = self.tokens[self.pos]
            self.pos += 1
        return ret
        
    def in_bounds(self):
        return self.pos < len(self.tokens)
    
    def get_cur_token(self):
        return self.tokens[self.pos]
    
    def peek_next(self):
        if self.pos + 1 < len(self.tokens):
            return self.tokens[self.pos+1]
        return None

    def get_current_section_name(self):
        return self.current_section_stack[-1].name

    def get_current_section(self):
        return self.current_section_stack[-1]

    def eat_variable_creation(self):
        result = []
        first_3_tokens = self.tokens[self.pos:self.pos+3]
        first_3_types = [token.type for token in first_3_tokens]
        match first_3_types:
            case [
                TokenType.DECLARE_VAR_VARIABLE_WORD, TokenType.VARIABLE, 
                TokenType.ASSIGN_VALUE
            ]:
                #TODO: come back to handle paren
                value_found = []
                self.pos += 3
                while self.in_bounds() and (cur := self.consume_token()).type != TokenType.STATEMENT_SEP_NEW_LINE:
                    value_found.append(cur.transpiled_value())
                variable_name = first_3_tokens[1]
                as_variable = Variable(variable_name, value_found[-1])
                if self.get_current_section_name() == GLOBAL_SECTION:
                    self.section_to_data[GLOBAL_SECTION].append(f"{variable_name.value}: {as_variable.type}\n")
                    self.section_to_data[INIT_SECTION].append(f"{variable_name.value} = {value_found[-1]}\n")
                    return ""
                else:
                    result.append(f"{variable_name.value} :=")
                    result.append(''.join(value_found))
                    if self.in_bounds():
                        result.append(cur.transpiled_value())
                return " ".join(result)
            case [
                TokenType.DECLARE_VAR_VARIABLE_WORD, TokenType.VARIABLE, TokenType.TYPE_DEFINITION_COLON
            ]:
                #TODO: come back to handle paren
                value_found = []
                while self.in_bounds() and (cur := self.consume_token()).type != TokenType.STATEMENT_SEP_NEW_LINE:
                    value_found.append(cur.transpiled_value())
                variable_name = first_3_tokens[1]

                # as_variable = Variable(variable_name, value_found[-1])
                if self.get_current_section_name() == GLOBAL_SECTION:
                    type_part_transpiled = "".join(value_found[3:])
                    self.section_to_data[GLOBAL_SECTION].append(f"{variable_name.value}: {type_part_transpiled}\n")
                    return ""
                else:
                    raise Exception("case for only type not handled")
                return " ".join(result)
            case _:
                og_version = [x.value for x in first_3_tokens]
                raise Exception(f"unknown variable creation syntax: {og_version}")


    def eat_until_new_block_start(self, return_tokens=False) -> list[Token] | str:
        '''output includes the colon'''
        result = []
        while self.in_bounds() and (cur := self.consume_token()).type != TokenType.START_NEW_BLOCK_COLON:
            result.append(cur)
        # result.append(cur)
        if self.in_bounds():
            result.append(cur)
        else:
            raise Exception("block was never ended")
        if return_tokens:
            return result
        #avoids the space
        return " ".join(x.transpiled_value() for x in result[:-1]) + result[-1].transpiled_value()

    def handle_indent(self, should_go_to_no_indent=False):
        if self.in_bounds() and self.peek_next().type == TokenType.STATEMENT_SEP_NEW_LINE:
            self.consume_token()
            return []
        # print(self.indent_stack)
        """
        Handles this case:
        if whatever:
            stuff
        V<<<<<HERE-----------------------
        print("outside")
        """
        ret = self.remove_indentation_to_match_amount(self.get_cur_token().value)
        
        # we indented more
        if not should_go_to_no_indent and (
            not self.indent_stack or self.indent_stack[-1] != self.get_cur_token().value
        ):
            self.indent_stack.append(self.get_cur_token().value)
        ret.append(self.consume_token().transpiled_value())
        return ret

    def remove_indentation_to_match_amount(self, amount):
        ret = []
        while len(self.indent_stack) and self.indent_stack[-1] > amount:
            self.indent_stack.pop()
            if len(self.indent_stack) == 1:
                if self.get_current_section_name() == UPDATE_SECTION:
                    ret.append(get_marker_name_for(UPDATE_SECTION_END))
                    ret.append("\n")
                self.current_section_stack.pop()
            if self.get_current_section().section_type != SectionType.CURLY_EXPRESSION:
                ret.append(" " * self.indent_stack[-1] + "}\n")
        return ret

    def get_output(self) -> str:
        starting_lines = """
                package output

                import "core:fmt"
                import rl "vendor:raylib"

                // AUTO GENERATED, 
                // CHANGES WILL BE OVERRIDDEN
                """.strip()
                # main :: proc()
        starting_lines = "\n".join(line.strip() for line in starting_lines.splitlines())
        prelude_tokens = [
            Token(TokenType.RAW_INSERT_LITERAL, (-1, 0), value=starting_lines),
            Token(TokenType.STATEMENT_SEP_NEW_LINE, (-1, 2)),
        ]
        prelude = None
        # if self.is_main:
        #     prelude = TokenConsumer(prelude_tokens, False)
            # Token(TokenType.STATEMENT_SEP_NEW_LINE, (-1, 0)),
            # Token(TokenType.RAW_INSERT_LITERAL, (-1, 0), value="\n}"),
        self.output = []
        while self.pos < len(self.tokens):
            token = self.tokens[self.pos]
            # output.append(" " * self.indendt_depth)
            match token.type:
                case TokenType.DECLARE_VAR_VARIABLE_WORD:
                    self.output.append(self.eat_variable_creation())
                case TokenType.VARIABLE if self.peek_next().type == TokenType.TYPE_DEFINITION_COLON:
                    self.output.append(self.consume_token().transpiled_value())
                    self.variable_was_declared_on_line = True
                case TokenType.STRUCT_START:
                    tokens_until_new_block = self.eat_until_new_block_start(True)
                    match tokens_until_new_block:
                        case [
                                Token(type=TokenType.STRUCT_START), 
                                Token(type=TokenType.VARIABLE, value=struct_name),
                                Token(type=TokenType.START_NEW_BLOCK_COLON),
                            ]:
                            self.current_section_stack.append(Section(SectionType.STRUCT, struct_name))
                            self.output.append(f"{struct_name} :: struct {{")
                        case _:
                            raise Exception(f"invalid struct creation {tokens_until_new_block[0].line_and_col}")
                        
                case TokenType.IF_CONDITION_START:
                    self.output.append(self.eat_until_new_block_start())
                case TokenType.FOR_START:
                    tokens_until_new_block = self.eat_until_new_block_start(True)
                    match tokens_until_new_block:
                        case [
                                Token(type=TokenType.FOR_START), 
                                Token(type=TokenType.VARIABLE, value=var_name), 
                                Token(type=TokenType.IN),
                                Token(type=TokenType.INT_NUMBER) | Token(type=TokenType.VARIABLE),
                                Token(type=TokenType.LESS_THAN_RANGE) | Token(type=TokenType.LESS_EQUAL_RANGE),
                                Token(type=TokenType.INT_NUMBER) | Token(type=TokenType.VARIABLE),
                                *_
                        ]:
                            self.output.append(join_tokens_to_string(tokens_until_new_block))
                        case _:
                            self.output.append(join_tokens_to_string(tokens_until_new_block))
                case TokenType.FUNCTION_START:
                    tokens_until_new_block = self.eat_until_new_block_start(True)
                    match tokens_until_new_block:
                        # situation like this
                        # def update:
                        case [
                                Token(type=TokenType.FUNCTION_START), 
                                Token(type=TokenType.VARIABLE, value=func_name), 
                                Token(type=TokenType.START_NEW_BLOCK_COLON),
                                *_
                        ]:
                            self.current_section_stack.append(Section(SectionType.FUNCTION, func_name))
                            self.output.append(f"{func_name} :: proc() {{")
                            if func_name == INIT_SECTION:
                                self.output.append("\n// init setup\n")
                                self.output.append(get_marker_name_for(INIT_SECTION))
                                self.output.append("\n// end init setup\n")
                            if func_name == UPDATE_SECTION:
                                self.output.append(get_marker_name_for(UPDATE_SECTION))
                            
                        # situation like this
                        # def update(some_var1 : int):
                        case [
                                Token(type=TokenType.FUNCTION_START), 
                                Token(type=TokenType.VARIABLE, value=func_name), 
                                *inside,
                                Token(type=TokenType.START_NEW_BLOCK_COLON),
                        ]:
                            self.current_section_stack.append(Section(SectionType.FUNCTION, func_name))
                            inside_transpiled = " ".join([x.transpiled_value() for x in inside])
                            self.output.append(f"{func_name} :: proc{inside_transpiled} {{")
                        case _:
                            self.output.append(self.eat_until_new_block_start())
                case TokenType.SECTION_START:
                    _, section_name, colon = self.eat_until_new_block_start(True)
                    self.output.append(f"{section_name.transpiled_value()}:{colon.transpiled_value()}")
                case TokenType.INDENT:
                    self.output.extend(self.handle_indent())   
                case TokenType.STATEMENT_SEP_NEW_LINE if self.peek_next() is None or self.peek_next().type not in [
                            TokenType.INDENT, 
                            TokenType.STATEMENT_SEP_NEW_LINE
                ]:
                    self.output.extend(self.remove_indentation_to_match_amount(0))
                    self.output.append(self.consume_token().transpiled_value())
                    self.variable_was_declared_on_line = False
                case TokenType.STATEMENT_SEP_NEW_LINE:
                    if (
                        self.get_current_section().section_type == SectionType.STRUCT and 
                        self.variable_was_declared_on_line
                    ):
                        self.output.append(",")
                    self.output.append(self.consume_token().transpiled_value())
                    self.variable_was_declared_on_line = False
                case TokenType.ARGUMENT_SEPARATOR_COMMA:
                    self.output.append(self.consume_token().transpiled_value() + " ")
                # add spaces for cases like "using rl"
                case TokenType.VARIABLE if self.peek_next().type == TokenType.VARIABLE:
                    self.output.append(self.consume_token().transpiled_value() + 
                    " " + self.consume_token().transpiled_value())
                case TokenType.STRUCT_INSTANCE_OPEN_CURLY:
                    self.current_section_stack.append(Section(SectionType.CURLY_EXPRESSION, "{"))
                    self.output.append(self.consume_token().transpiled_value())
                case TokenType.STRUCT_INSTANCE_CLOSE_CURLY:
                    if self.get_current_section().section_type == SectionType.CURLY_EXPRESSION:
                        self.current_section_stack.pop()
                    else:
                        raise Exception(f"No matching open curly {self.get_cur_token().line_and_col}")
                    self.output.append(self.consume_token().transpiled_value())
                # case TokenType.EXPRESSION_START_OPEN_PAREN:
                #     self.current_section_stack.append(Section(SectionType.CURLY_EXPRESSION, "("))
                #     self.output.append(self.consume_token().transpiled_value())
                # case TokenType.EXPRESSION_END_CLOSE_PAREN:
                #     if self.get_current_section().section_type == SectionType.PAREN_EXPRESSION:
                #         self.current_section_stack.pop()
                #     else:
                #         raise Exception(f"No matching open '(' for this ')' {self.get_cur_token().line_and_col}")
                #     self.output.append(self.consume_token().transpiled_value())
                case unknown:
                    print("direct transpile for", unknown)
                    self.output.append(self.consume_token().transpiled_value())
                    # raise Exception(f"straight compile for {unknown}")
        # output.extend(self.handle_indent(True))

        if prelude is not None:
            self.output = [prelude.get_output()] + list(self.section_to_data[GLOBAL_SECTION]) + self.output
        else:
            self.output = list(self.section_to_data[INIT_SECTION]) + self.output

        self.add_raylib_inserts()
            
        self.output.extend(self.remove_indentation_to_match_amount(0))
        for i, line in enumerate(self.output):
            if line in marker_to_section_name:
                self.output[i] = "".join(self.section_to_data[marker_to_section_name[line]])
        # print(self.section_to_data)
        return "".join(self.output)

    def add_raylib_inserts(self):
        # raylib window init
        self.section_to_data[INIT_SECTION].append("""using rl
SetConfigFlags({.WINDOW_RESIZABLE, .VSYNC_HINT})
InitWindow(screen_width, screen_height, "score more")
SetTargetFPS(60)
""")
            
        self.section_to_data[UPDATE_SECTION].appendleft("""
using rl
BeginDrawing()
ClearBackground(SKYBLUE)
""")
        # self.section_to_data[UPDATE_SECTION].append("}")
        self.section_to_data[UPDATE_SECTION_END].append("""
EndDrawing()
game_clock += 1
free_all(context.temp_allocator)""")
            
                    


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

    from pathlib import Path
    options = list(Path.cwd().glob("tests/*.gamelang"))
    options.sort(key=lambda p: p.stat().st_mtime)
    # for x in options:
    #     print(x)
    newest = max(
        Path.cwd().glob("tests/*.gamelang"),
        key=lambda p: p.stat().st_mtime,
        default=None,
    )


    to_run = newest if "gamelang" not in " ".join(sys.argv) else sys.argv[1]
    with open(to_run) as f:
        data = f.read()
        runner = Tokenizer(data)
        consumer = TokenConsumer(runner.result)
    output = consumer.get_output()
    with open("output.odin", 'w') as f:
        f.write(output)
    print("Ran:\n", to_run, sep='')




    



