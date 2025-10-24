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
    ("MISMATCH", r"."),  # cualquier otro símbolo
]
MASTER_RE = re.compile("|".join(f"(?P<{n}>{p})" for n, p in TOKEN_SPEC))

def preprocess(source: str) -> str:
    """Convierte a mayúsculas y elimina comentarios (líneas que comienzan con C/c)."""
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
        return ast
    except ParseError as e:
        print("ERROR:", e)
        return None

# =====================================================
# 6) AST a Graphviz (exportar a PNG)
# =====================================================

def ast_to_graphviz(node):
    from graphviz import Digraph
    dot = Digraph(comment="AST FORTRAN77", format="png")
    counter = {"i": 0}

    def new_id():
        counter["i"] += 1
        return f"n{counter['i']}"

    def label(n):
        t = type(n).__name__
        if isinstance(n, Identifier): return f"Identifier\\n{n.name}"
        if isinstance(n, Number): return f"Number\\n{n.value}"
        return t

    def walk(n):
        nid = new_id()
        dot.node(nid, label(n))
        if isinstance(n, Program):
            cid = walk(n.body); dot.edge(nid, cid)
        elif isinstance(n, Block):
            for st in n.statements:
                cid = walk(st); dot.edge(nid, cid)
        elif isinstance(n, Assignment):
            tid = new_id(); dot.node(tid, f"Identifier\\n{n.target}")
            dot.edge(nid, tid)
            cid = walk(n.value); dot.edge(nid, cid)
        elif isinstance(n, Conditional):
            oid = new_id(); dot.node(oid, f"REL\\n{n.op}")
            dot.edge(nid, oid)
            dot.edge(nid, walk(n.left))
            dot.edge(nid, walk(n.right))
        elif isinstance(n, BinOp):
            oid = new_id(); dot.node(oid, f"OP\\n{n.op}")
            dot.edge(nid, oid)
            dot.edge(nid, walk(n.left))
            dot.edge(nid, walk(n.right))
        elif isinstance(n, Paren):
            dot.edge(nid, walk(n.inner))
        return nid

    walk(node)
    return dot

def export_demo_ast_png(outname="ast_fortran77"):
    """Genera un PNG del AST usando la demo_ok."""
    try:
        ast = parse_source(demo_ok)
        dot = ast_to_graphviz(ast)
        outpath = dot.render(filename=outname, format="png", cleanup=True)
        print(f"[OK] AST exportado: {outpath}")
    except Exception as e:
        print("[ERROR] No se pudo exportar el AST a PNG:", e)

# =====================================================
# 7) MAIN
# =====================================================

if __name__ == "__main__":
    print(">>> DEMO OK")
    ast = run_demo(demo_ok)
    print("\n>>> DEMO ERR")
    run_demo(demo_err)

    # Generar imagen del AST de la demo_ok
    export_demo_ast_png("ast_fortran77")
