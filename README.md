# 🐍 Snake Interpreter

## Overview

This project implements a basic interpreter for a custom programming language called **Snake** (not related to Python's Snake game!). The interpreter is written in **Node.js** and supports basic programming constructs such as variables, arithmetic, conditionals, loops, lists, and list functions.

---

## 📁 File Structure

- `snake.js` - The main interpreter that includes the **Lexer**, **Parser**, and **Interpreter**.
- You run it like this:  
  ```bash
  node snake.js path/to/code.snake
  ```

---

## ✨ Features Supported

### 🧠 Language Capabilities

- **Variables** and **Assignment**
- **Arithmetic Expressions**: `+`, `-`, `*`, `/`
- **Comparison Operators**: `==`, `<`, `>`, `<=`, `>=`
- **Conditionals**: `if`, `elif`, `else`
- **Loops**:
  - `while` loops
  - `for` loops with `in`
- **Print Statement**: `print`
- **Lists**:
  - List creation: `[1, 2, 3]`
  - List methods: `.append`, `.pop`, `.item`, `.index`, `.len`
  - `range(start, end)` for generating sequences

---

## 🧩 Components

### 1. **Lexer**
Responsible for **tokenizing** raw source code.

- Converts characters into meaningful tokens like:
  - `NUMBER`, `STRING`, `IDENTIFIER`
  - `IF`, `ELSE`, `WHILE`, `FOR`, `PRINT`
  - Operators: `+`, `-`, `*`, `/`, `==`, etc.
  - Data structures: `LIST`, `LIST_FUNC`

---

### 2. **Parser**
Turns tokens into an **Abstract Syntax Tree (AST)**.

- Implements a recursive descent parser.
- Supports:
  - Expressions (`expr`, `term`, `factor`)
  - Statements (`if`, `while`, `for`, `print`, assignment)
  - Blocks (`{ ... }`)

---

### 3. **Interpreter**
Executes the AST generated by the parser.

- Maintains an **environment** (`env`) for variable bindings.
- Evaluates:
  - Arithmetic operations
  - Variable assignment and access
  - Control structures (`if`, `while`, `for`)
  - Built-in list functions
  - Print output to console

---

## 🛠️ Example Snake Code

```python
x = 5
y = [1, 2, 3]
print x
print y

if (x > 3) {
    print "x is large"
} else {
    print "x is small"
}

for i in range(1, 3) {
    print i
}

y.append(4)
print y.item(2)
```

---

## 🏃 Usage

```bash
node snake.js main.snk
```

Make sure the `.snk` file contains valid code in the custom Snake language.

---

## 📚 Token Types

| Token         | Description                          |
|---------------|--------------------------------------|
| `NUMBER`      | Integer literals                     |
| `STRING`      | Text inside `"` quotes               |
| `IDENTIFIER`  | Variable/function names              |
| `ASSIGN`      | `=` assignment operator              |
| `PLUS`        | `+` addition                         |
| `EQUALS`      | `==` equality check                  |
| `LBRACE`/`RBRACE` | `{` and `}` for code blocks     |
| `LPAREN`/`RPAREN` | `(` and `)` for expressions     |
| `LIST_FUNC`   | List operations like `.append(x)`    |

---

## 🐛 Error Handling

- Lexer and parser throw descriptive errors for:
  - Unknown characters
  - Unterminated strings/lists
  - Invalid grammar (e.g., `for` without `in`)
  - Undefined variables

---

## ✅ To-Do / Possible Extensions

- Importing other files
