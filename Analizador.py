# =====================================================
# Analizador Léxico y Sintáctico FORTRAN77 (LL(1))
# INFO1148 – Tarea 3
# =====================================================

import re
from dataclasses import dataclass
from typing import List, Union

# =====================================================
# 1) DEFINICIÓN DE TOKENS (LEXER)
# =====================================================

@dataclass
class Token:
    type: str
    value: str
    line: int
    col: int

KEYWORDS = {"PROGRAM", "END", "IF", "THEN", "DO"}

TOKEN_SPEC = [
    ("REL",      r"\.(GT|LT|EQ)\."),   # .GT. .LT. .EQ.
    ("NUM",      r"\d+(\.\d+)?"),
    ("ID",       r"[A-Z][A-Z0-9]*"),
    ("PLUS",     r"\+"),
    ("MINUS",    r"-"),
    ("TIMES",    r"\*"),
    ("DIV",      r"/"),
    ("ASSIGN",   r"="),
    ("LPAREN",   r"\("),
    ("RPAREN",   r"\)"),
    ("COMMA",    r","),
    ("NEWLINE",  r"\n"),
    ("SKIP",     r"[ \t\r\f\v]+"),
    ("MISMATCH", r"."),
]
MASTER_RE = re.compile("|".join(f"(?P<{n}>{p})" for n, p in TOKEN_SPEC))

def preprocess(source: str) -> str:
    """Mayúsculas + eliminar líneas que comienzan con C (comentarios FORTRAN77)"""
    out = []
    for line in source.splitlines():
        if line and line[0] in ("C", "c"):
            out.append("")  
        else:
            out.append(line.upper())
    return "\n".join(out)

def tokenize(source: str) -> List[Token]:
    text = preprocess(source)
    tokens: List[Token] = []
    line = 1
    for mo in MASTER_RE.finditer(text):
        kind = mo.lastgroup
        value = mo.group()
        col = mo.start() - text.rfind("\n", 0, mo.start())
        if kind == "NEWLINE":
            line += 1
            continue
        if kind == "SKIP":
            continue
        if kind == "ID" and value in KEYWORDS:
            tokens.append(Token(value, value, line, col))
        elif kind == "REL":
            tokens.append(Token("REL", value, line, col))
        elif kind == "MISMATCH":
            raise SyntaxError(f"Error léxico: símbolo inesperado '{value}' en línea {line}, col {col}")
        else:
            tokens.append(Token(kind, value, line, col))
    tokens.append(Token("EOF", "", line, 0))
    return tokens

# =====================================================
# 2) DEFINICIÓN DE NODOS DEL AST
# =====================================================

@dataclass
class Program:
    name: str
    body: "Block"

@dataclass
class Block:
    statements: List["Statement"]

Statement = Union["Assignment", "Conditional"]

@dataclass
class Assignment:
    target: str
    value: "Expr"

@dataclass
class Conditional:
    left: "Expr"
    op: str
    right: "Expr"

Expr = Union["BinOp", "Number", "Identifier", "Paren"]

@dataclass
class BinOp:
    left: "Expr"
    op: str
    right: "Expr"

@dataclass
class Number:
    value: float

@dataclass
class Identifier:
    name: str

@dataclass
class Paren:
    inner: "Expr"

# =====================================================
# 3) ANALIZADOR SINTÁCTICO LL(1)
# =====================================================

class ParseError(Exception):
    pass

class Parser:
    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.i = 0

    @property
    def current(self) -> Token:
        return self.tokens[self.i]

    def match(self, *types: str) -> Token:
        if self.current.type in types:
            tok = self.current
            self.i += 1
            return tok
        t = self.current
        exp = " | ".join(types)
        raise ParseError(f"Se esperaba {exp} pero se encontró {t.type} ('{t.value}') en línea {t.line}, col {t.col}")

    def parse_program(self) -> Program:
        self.match("PROGRAM")
        name = self.match("ID").value
        body = self.parse_block()
        self.match("END")
        self.match("EOF")
        return Program(name=name, body=body)

    def parse_block(self) -> Block:
        stmts: List[Statement] = []
        while self.current.type in ("ID", "IF"):
            stmts.append(self.parse_statement())
        return Block(statements=stmts)

    def parse_statement(self) -> Statement:
        if self.current.type == "ID":
            return self.parse_assignment()
        if self.current.type == "IF":
            return self.parse_conditional()
        raise ParseError(f"Sentencia inválida en línea {self.current.line}")

    def parse_assignment(self) -> Assignment:
        ident = self.match("ID").value
        self.match("ASSIGN")
        value = self.parse_expr()
        return Assignment(target=ident, value=value)

    def parse_conditional(self) -> Conditional:
        self.match("IF")
        self.match("LPAREN")
        left = self.parse_expr()
        op = self.match("REL").value
        right = self.parse_expr()
        self.match("RPAREN")
        self.match("THEN")
        return Conditional(left=left, op=op, right=right)

    def parse_expr(self):
        node = self.parse_term()
        while self.current.type in ("PLUS", "MINUS"):
            op = self.current.value; self.i += 1
            right = self.parse_term()
            node = BinOp(left=node, op=op, right=right)
        return node

    def parse_term(self):
        node = self.parse_factor()
        while self.current.type in ("TIMES", "DIV"):
            op = self.current.value; self.i += 1
            right = self.parse_factor()
            node = BinOp(left=node, op=op, right=right)
        return node

    def parse_factor(self):
        t = self.current
        if t.type == "ID":
            self.i += 1
            return Identifier(name=t.value)
        if t.type == "NUM":
            self.i += 1
            return Number(value=float(t.value))
        if t.type == "LPAREN":
            self.i += 1
            inner = self.parse_expr()
            self.match("RPAREN")
            return Paren(inner=inner)
        raise ParseError(f"Factor inválido en línea {t.line}, col {t.col}")

# =====================================================
# 4) FUNCIONES AUXILIARES
# =====================================================

def parse_source(source: str) -> Program:
    return Parser(tokenize(source)).parse_program()

def ast_to_str(node, indent=0) -> str:
    pad = "  " * indent
    if isinstance(node, Program):
        return f"{pad}Program({node.name})\n" + ast_to_str(node.body, indent+1)
    if isinstance(node, Block):
        s = f"{pad}Block\n"
        for st in node.statements: s += ast_to_str(st, indent+1)
        return s
    if isinstance(node, Assignment):
        return f"{pad}Assignment({node.target})\n" + ast_to_str(node.value, indent+1)
    if isinstance(node, Conditional):
        return f"{pad}Conditional({node.op})\n" + ast_to_str(node.left, indent+1) + ast_to_str(node.right, indent+1)
    if isinstance(node, BinOp):
        return f"{pad}BinOp({node.op})\n" + ast_to_str(node.left, indent+1) + ast_to_str(node.right, indent+1)
    if isinstance(node, Number):
        return f"{pad}Number({node.value})\n"
    if isinstance(node, Identifier):
        return f"{pad}Identifier({node.name})\n"
    if isinstance(node, Paren):
        return f"{pad}Paren\n" + ast_to_str(node.inner, indent+1)
    return f"{pad}{node}\n"

# =====================================================
# 5) DEMO DE PRUEBA
# =====================================================

demo_ok = """\
PROGRAM SUMA
A = 10
B = 5
C = A + B * 2
IF (C .GT. 12) THEN
END
"""

demo_err = """\
PROGRAM TEST
A = + * 5
END
"""

def run_demo(source: str):
    print("=== TOKENS ===")
    for t in tokenize(source):
        print(t)
    print("\n=== PARSE ===")
    try:
        ast = parse_source(source)
        print("Programa aceptado.\n")
        print("=== AST ===")
        print(ast_to_str(ast))
    except ParseError as e:
        print("ERROR:", e)

if __name__ == "__main__":
    print(">>> DEMO OK")
    run_demo(demo_ok)
    print("\n>>> DEMO ERR")
    run_demo(demo_err)
