# 🧮 Analizador Léxico y Sintáctico FORTRAN77 (LL(1))

**Curso:** INFO1148 – Teoría de la Computación  
**Autor:** *Demian  Binimelis , Ignacio  Cancino, Daniel Burgos*  
**Lenguaje:** Python 3  
**Fecha:** 2025  

---

## 📘 Descripción General

Este proyecto implementa un **analizador léxico y sintáctico** para un subconjunto del lenguaje **FORTRAN77**, utilizando el método **LL(1)** por descenso recursivo.

El objetivo es aplicar los conceptos de **gramáticas libres de contexto (GLC)** y **análisis sintáctico predictivo** vistos en la asignatura, demostrando cómo la teoría de autómatas y lenguajes formales se traduce en el funcionamiento real de un compilador.

El analizador está desarrollado íntegramente en **Python**, con estructura modular y mensajes de error detallados.

---

## ⚙️ Funcionalidades

- Analizador **léxico** mediante expresiones regulares (`re`).
- Analizador **sintáctico LL(1)** implementado por descenso recursivo.
- Reconocimiento de estructuras básicas de **FORTRAN77**:
  - Programa principal: `PROGRAM ... END`
  - **Asignaciones:** `A = B + C * 2`
  - **Condicionales:** `IF (A .GT. B) THEN`
- Construcción del **Árbol Sintáctico Abstracto (AST)**.
- Reporte de **errores léxicos y sintácticos** con línea y columna.

---

## 🧩 Gramática Utilizada (GLC)

<programa> → PROGRAM ID <bloque> END
<bloque> → <sentencia> | <sentencia> <bloque>
<sentencia> → <asignacion> | <condicional>
<asignacion> → ID = <expresion>
<condicional>→ IF ( <expresion> <operador_rel> <expresion> ) THEN
<expresion> → <termino> <expresion’>
<expresion’> → + <termino> <expresion’> | - <termino> <expresion’> | ε
<termino> → <factor> <termino’>
<termino’> → * <factor> <termino’> | / <factor> <termino’> | ε
<factor> → ID | NUM | ( <expresion> )
<operador_rel> → .GT. | .LT. | .EQ.

---

## 🚀 Ejecución

1. Copia el código del analizador en un archivo llamado `analizador_fortran77.py`.  
2. Abre una terminal dentro de la carpeta donde está el archivo.  
3. Ejecuta el siguiente comando:

```bash
python analizador_fortran77.py
El programa incluye una demo interna con un caso válido y otro con error.

✅ Ejemplo de Ejecución
Entrada válida:

PROGRAM SUMA
A = 10
B = 5
C = A + B * 2
IF (C .GT. 12) THEN
END
Salida esperada:

=== TOKENS ===
Token(type='PROGRAM', value='PROGRAM', line=1, col=1)
Token(type='ID', value='SUMA', line=1, col=9)
...

=== PARSE ===
Programa aceptado.

=== AST ===
Program(SUMA)
  Block
    Assignment(A)
      Number(10.0)
    Assignment(B)
      Number(5.0)
    Conditional(.GT.)
      Identifier(C)
      Number(12.0)
Entrada con error:


PROGRAM TEST
A = + * 5
END
Salida esperada:

ERROR: Factor inválido en línea 2, col 5
```

🧱 Tecnologías y Librerías Utilizadas
---
Python 3.9+

re — Expresiones regulares

dataclasses — Estructuras para el AST

typing — Anotaciones de tipo

##🔍 Funcionamiento Interno
---
Lexer: transforma el texto en una secuencia de tokens (palabras reservadas, identificadores, números, operadores).

Parser LL(1): analiza la secuencia de tokens utilizando funciones recursivas que siguen la gramática formal.

AST: se construye un árbol sintáctico que representa la estructura jerárquica del programa.

Errores: se detectan y reportan con línea y columna exactas.


##🧩 Posibles Extensiones
---
Implementar ELSE ... ENDIF dentro de los condicionales.

Añadir bucles DO ... END DO.

Incorporar instrucciones READ y WRITE.

Generar árbol en formato gráfico (graphviz o networkx).


##📚 Referencias
---
Águila, J. (2004). Apunte de Compiladores: Análisis Sintáctico Predictivo por Descenso Recursivo. Universidad de Magallanes.

Lévano, M. (2025). Clase: Tipos de Gramáticas y Lenguajes Formales. Universidad Católica de Temuco.

Departamento de Computación (2025). Procesadores de Lenguajes – Tema 3. Universidad Católica de Temuco.
