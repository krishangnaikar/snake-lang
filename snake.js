#!/usr/bin/env node
const fs = require('fs');

// -----------------------------
// Lexer: Converts raw input into tokens.
class Lexer {
    constructor(text) {
        this.text = text;
        this.pos = 0;
        this.line = 1; // Track the current line number.
        this.currentChar = text[this.pos];
    }
    advance() {
        if (this.currentChar === "\n") {
            this.line++;
        }
        this.pos++;
        this.currentChar = this.pos < this.text.length ? this.text[this.pos] : null;
    }
    skipWhitespace() {
        while (this.currentChar !== null && /\s/.test(this.currentChar)) {
            this.advance();
        }
    }
    number() {
        let result = '';
        while (this.currentChar !== null && /[0-9]/.test(this.currentChar)) {
            result += this.currentChar;
            this.advance();
        }
        return { type: 'NUMBER', value: parseInt(result, 10), line: this.line };
    }
    string() {
        let result = '';
        this.advance(); // Skip the opening quote
        while (this.currentChar !== null && this.currentChar !== '"') {
            result += this.currentChar;
            this.advance();
        }
        if (this.currentChar === null) {
            throw new Error(`Line ${this.line}: Unterminated string`);
        }
        this.advance(); // Skip the closing quote
        return { type: 'STRING', value: result, line: this.line };
    }
    list() {
        let result = [];
        this.advance(); // Skip the opening bracket
        while (this.currentChar !== null && this.currentChar !== ']') {
            if (this.currentChar === ' ') {
                this.advance(); // Skip the space
                continue;
            }
            if (this.currentChar === ',') {
                this.advance(); // Skip the comma
                continue;
            }
            if (this.currentChar === '"') {
                result.push(this.string());
                continue;
            }
            if (this.currentChar === '[') {
                result.push(this.list());
                continue;
            }
            if (/[0-9]/.test(this.currentChar)) {
                result.push(this.number());
                continue;
            }
            if (/[a-zA-Z_]/.test(this.currentChar)) {
                result.push(this.identifier());
                continue;
            }
            if (this.currentChar === ',') {
                this.advance();
            }
        }
        if (this.currentChar === null) {
            throw new Error(`Line ${this.line}: Unterminated list`);
        }
        this.advance(); // Skip the closing bracket
        return { type: 'LIST', value: result, line: this.line };
    }
    funcs() {
        this.advance(); // Skip the space
        let name = '';
        while (this.currentChar !== null && /[a-zA-Z_]/.test(this.currentChar)) {
            name += this.currentChar;
            this.advance();
        }
        this.advance(); // Skip the opening parenthesis
        let params = [];
        while (this.currentChar !== null && this.currentChar !== ')') {
            if (this.currentChar === ',') {
                this.advance(); // Skip the comma
                continue;
            }
            if (/[a-zA-Z_]/.test(this.currentChar)) {
                params.push(this.identifier());
                continue;
            }
            if (/\s/.test(this.currentChar)) {
                this.advance(); // Skip whitespace
                continue;
            }
        }
        if (this.currentChar === null) {
            throw new Error(`Line ${this.line}: Unterminated function`);
        }
        this.advance(); // Skip the closing parenthesis
        return { type: "FUNCTION_DEFINE", value: name, params: params, line: this.line };
    }
    identifier() {
        let result = '';
        while (this.currentChar !== null && /[a-zA-Z_]/.test(this.currentChar)) {
            result += this.currentChar;
            this.advance();
            if (result === "def") {
                return this.funcs();
            }
        }
        // New: check for "import" keyword.
        if (result === "import") {
            return { type: "IMPORT", value: result, line: this.line };
        }
        if (this.currentChar == ".") {
            return this.listFunc(result);
        }
        if (result === "for") {
            this.advance(); // Skip the space
            let varName = '';
            while (this.currentChar !== null && /[a-zA-Z_]/.test(this.currentChar)) {
                varName += this.currentChar;
                this.advance();
            }
            this.advance(); // Skip the space
            let checkIn = '';
            while (this.currentChar !== null && /[a-zA-Z_]/.test(this.currentChar)) {
                checkIn += this.currentChar;
                this.advance();
            }
            if (checkIn === "in") {
                this.advance(); // Skip the space
                let l;
                if (this.currentChar === '[') {
                    l = this.list();
                    this.advance(); // Skip the space
                    if (this.currentChar === ' ') {
                        this.advance(); // Skip the space
                    }
                } else if (/[a-zA-Z_]/.test(this.currentChar)) {
                    l = this.identifier();
                } else {
                    throw new Error(`Line ${this.line}: Expected list or identifier after "in"`);
                }
                return { type: 'FOR', value: result, params: [{ type: "IDENTIFIER", value: varName, line: this.line }, l], line: this.line };
            } else {
                throw new Error(`Line ${this.line}: Expected "in" after variable name`);
            }
        }
        if (result === "range") {
            this.advance(); // Skip the opening parenthesis
            let start = this.number();
            if (this.currentChar === ',') {
                this.advance(); // Skip the comma
            }
            if (/\s/.test(this.currentChar)) {
                this.advance(); // Skip whitespace
            }
            let end = this.number();
            this.advance(); // Skip the closing parenthesis
            return { type: "LIST_FUNC", value: "range", params: [start, end], line: this.line };
        }
        if (result === "return") {
            return { type: 'RETURN', value: result, line: this.line };
        }
        // Reserved keywords.
        if (result === 'print') {
            return { type: 'PRINT', value: result, line: this.line };
        }
        if (result === 'if') {
            return { type: 'IF', value: result, line: this.line };
        }
        if (result === 'elif') {
            return { type: 'ELIF', value: result, line: this.line };
        }
        if (result === 'else') {
            return { type: 'ELSE', value: result, line: this.line };
        }
        if (result === 'while') {
            return { type: 'WHILE', value: result, line: this.line };
        }
        if (this.currentChar === "(") {
            this.advance(); // Skip the opening parenthesis
            let params = [];
            while (this.currentChar !== null && this.currentChar !== ')') {
                if (this.currentChar === ',') {
                    this.advance(); // Skip the comma
                    continue;
                }
                if (/[0-9]/.test(this.currentChar)) {
                    params.push(this.number());
                    continue;
                }
                if (/[a-zA-Z_]/.test(this.currentChar)) {
                    params.push(this.identifier());
                    continue;
                }
                if (this.currentChar === '"') {
                    params.push(this.string());
                    continue;
                }
                if (this.currentChar === '[') {
                    params.push(this.list());
                    continue;
                }
                if (/\s/.test(this.currentChar)) {
                    this.advance(); // Skip whitespace
                }
            }
            if (this.currentChar === null) {
                throw new Error(`Line ${this.line}: Unterminated function call`);
            }
            this.advance(); // Skip the closing parenthesis
            return { type: 'FUNCTION_CALL', value: result, params: params, line: this.line };
        }
        if (result === 'true' || result === 'false') {
            return { type: 'BOOLEAN', value: result == 'true', line: this.line };
        }
        return { type: 'IDENTIFIER', value: result, line: this.line };
    }
    listFunc(l) {
        let result = '';
        this.advance(); // Skip the '.'
        while (this.currentChar !== null && /[a-zA-Z_]/.test(this.currentChar)) {
            result += this.currentChar;
            this.advance();
        }
        if (this.currentChar == '(') {
            this.advance();
            let val;
            if (/[0-9]/.test(this.currentChar)) {
                val = this.number();
            } else if (/[a-zA-Z_]/.test(this.currentChar)) {
                val = this.identifier();
            } else if (this.currentChar === '"') {
                val = this.string();
            }
            if (this.currentChar === ')') {
                this.advance();
                return { type: 'LIST_FUNC', value: result, params: [{ type: 'IDENTIFIER', value: l, line: this.line }, val], line: this.line };
            } else {
                throw new Error(`Line ${this.line}: Unterminated list function`);
            }
        }
    }
    getNextToken() {
        while (this.currentChar !== null) {
            if (this.currentChar === '"') {
                return this.string();
            }
            if (this.currentChar === '[') {
                return this.list();
            }
            if (/\s/.test(this.currentChar)) {
                this.skipWhitespace();
                continue;
            }
            if (/[0-9]/.test(this.currentChar)) {
                return this.number();
            }
            if (/[a-zA-Z_]/.test(this.currentChar)) {
                return this.identifier();
            }
            if (this.currentChar === '+') {
                this.advance();
                return { type: 'PLUS', value: '+', line: this.line };
            }
            if (this.currentChar === '-') {
                this.advance();
                return { type: 'MINUS', value: '-', line: this.line };
            }
            if (this.currentChar === '*') {
                this.advance();
                return { type: 'MUL', value: '*', line: this.line };
            }
            if (this.currentChar === '/') {
                this.advance();
                return { type: 'DIV', value: '/', line: this.line };
            }
            if (this.currentChar === '=') {
                this.advance();
                if (this.currentChar === '=') {
                    this.advance();
                    return { type: 'EQUALS', value: '==', line: this.line };
                }
                return { type: 'ASSIGN', value: '=', line: this.line };
            }
            if (this.currentChar === '<') {
                this.advance();
                if (this.currentChar === '=') {
                    this.advance();
                    return { type: 'LESS_EQUALS', value: '<=', line: this.line };
                }
                return { type: 'LESS', value: '<', line: this.line };
            }
            if (this.currentChar === '>') {
                this.advance();
                if (this.currentChar === '=') {
                    this.advance();
                    return { type: 'GREATER_EQUALS', value: '>=', line: this.line };
                }
                return { type: 'GREATER', value: '>', line: this.line };
            }
            if (this.currentChar === '(') {
                this.advance();
                return { type: 'LPAREN', value: '(', line: this.line };
            }
            if (this.currentChar === ')') {
                this.advance();
                return { type: 'RPAREN', value: ')', line: this.line };
            }
            if (this.currentChar === '{') {
                this.advance();
                return { type: 'LBRACE', value: '{', line: this.line };
            }
            if (this.currentChar === '}') {
                this.advance();
                return { type: 'RBRACE', value: '}', line: this.line };
            }
            throw new Error(`Line ${this.line}: Unknown character: ${this.currentChar} pos: ${this.pos}`);
        }
        return { type: 'EOF', value: null, line: this.line };
    }
}

// -----------------------------
// Parser: Builds an AST from tokens.
class Parser {
    constructor(lexer) {
        this.lexer = lexer;
        this.currentToken = lexer.getNextToken();
    }
    eat(tokenType) {
        if (this.currentToken.type === tokenType) {
            this.currentToken = this.lexer.getNextToken();
        } else {
            throw new Error(`Line ${this.currentToken.line}: Expected token ${tokenType} but got ${this.currentToken.type}`);
        }
    }
    // block: { statement* }
    block() {
        this.eat('LBRACE');
        let statements = [];
        while (this.currentToken.type !== 'RBRACE') {
            let temp = this.statement();
            statements.push(temp);
        }
        this.eat('RBRACE');
        return { type: 'BLOCK', statements: statements };
    }
    parseCondition() {
        this.eat('LPAREN');
        let condition = this.expr();
        this.eat('RPAREN');
        return condition;
    }
    returnStatement() {
        this.eat('RETURN');
        let expr = this.expr();
        return { type: 'RETURN_STMT', expr, line: this.currentToken.line };
    }
    // New: import statement.
    importStatement() {
        this.eat('IMPORT');
        // Expect a string token (the filename)
        let fileToken = this.currentToken;
        if (fileToken.type !== 'STRING') {
            throw new Error(`Line ${fileToken.line}: Expected string literal after import`);
        }
        this.eat('STRING');
        return { type: 'IMPORT', filename: fileToken.value, line: fileToken.line };
    }
    // ifStatement: IF LPAREN expr RPAREN block (ELSE block)?
    ifStatement() {
        this.eat('IF');
        let condition = this.parseCondition();
        let trueBranch = this.block();
        let elifBranches = [];
        while (this.currentToken.type === 'ELIF') {
            this.eat('ELIF');
            elifBranches.push({ condition: this.parseCondition(), block: this.block() });
        }
        let falseBranch = null;
        if (this.currentToken.type === 'ELSE') {
            this.eat('ELSE');
            falseBranch = this.block();
        }
        return { type: 'IF_STMT', condition, trueBranch, elifBranches, falseBranch, line: this.currentToken.line };
    }
    whileStatement() {
        this.eat('WHILE');
        let condition = this.parseCondition();
        let body = this.block();
        return { type: 'WHILE_STMT', condition, body, line: this.currentToken.line };
    }
    forStatement() {
        let varName = this.currentToken.params[0];
        let listDetails = this.currentToken.params[1];
        this.eat('FOR');
        let body = this.block();
        return { type: 'FOR_STMT', params: [varName, listDetails], body: body, line: this.currentToken.line };
    }
    funcStatement() {
        let name = this.currentToken.value;
        let params = this.currentToken.params;
        this.eat('FUNCTION_DEFINE');
        let body = this.block();
        return { type: 'FUNCTION_DEFINE', value: name, params: params, body: body, line: this.currentToken.line };
    }
    // factor : NUMBER | IDENTIFIER | LPAREN expr RPAREN
    factor() {
        let token = this.currentToken;
        if (token.type === 'LIST_FUNC') {
            this.eat('LIST_FUNC');
            return { type: 'LIST_FUNC', value: token.value, params: token.params, line: token.line };
        } else if (token.type === 'NUMBER') {
            this.eat('NUMBER');
            return { type: 'NUMBER', value: token.value, line: token.line };
        } else if (token.type === 'BOOLEAN') {
            this.eat('BOOLEAN');
            return { type: 'BOOLEAN', value: token.value, line: token.line };
        } else if (token.type == 'STRING') {
            this.eat('STRING');
            return { type: 'STRING', value: token.value, line: token.line };
        } else if (token.type == 'LIST') {
            this.eat('LIST');
            return { type: 'LIST', value: token.value, line: token.line };
        } else if (token.type === 'IDENTIFIER') {
            this.eat('IDENTIFIER');
            return { type: 'VARIABLE', value: token.value, line: token.line };
        } else if (token.type === 'LPAREN') {
            this.eat('LPAREN');
            let node = this.expr();
            this.eat('RPAREN');
            return node;
        } else if (token.type === 'FUNCTION_CALL') {
            let funcName = token.value;
            let params = token.params;
            this.eat('FUNCTION_CALL');
            return { type: 'FUNCTION_CALL', value: funcName, params: params, line: token.line };
        }
        throw new Error(`Line ${token.line}: Unexpected token in factor: ${token.type}`);
    }
    // term : factor ((MUL|DIV) factor)*
    term() {
        let node = this.factor();
        while (['MUL', 'DIV'].includes(this.currentToken.type)) {
            let token = this.currentToken;
            if (token.type === 'MUL') {
                this.eat('MUL');
            } else if (token.type === 'DIV') {
                this.eat('DIV');
            }
            node = { type: 'BINOP', left: node, op: token.type, right: this.factor(), line: token.line };
        }
        return node;
    }
    // expr : term ((PLUS|MINUS|EQUALS|LESS|LESS_EQUALS|GREATER|GREATER_EQUALS) term)*
    expr() {
        let node = this.term();
        while (['PLUS', 'MINUS', 'EQUALS', "LESS", "LESS_EQUALS", "GREATER", "GREATER_EQUALS"].includes(this.currentToken.type)) {
            let token = this.currentToken;
            if (token.type === 'PLUS') {
                this.eat('PLUS');
            } else if (token.type === 'MINUS') {
                this.eat('MINUS');
            } else if (token.type === 'EQUALS') {
                this.eat('EQUALS');
            } else if (token.type === 'LESS') {
                this.eat('LESS');
            } else if (token.type === 'LESS_EQUALS') {
                this.eat('LESS_EQUALS');
            } else if (token.type === 'GREATER') {
                this.eat('GREATER');
            } else if (token.type === 'GREATER_EQUALS') {
                this.eat('GREATER_EQUALS');
            }
            node = { type: 'BINOP', left: node, op: token.type, right: this.term(), line: token.line };
        }
        return node;
    }
    // statement: importStatement | returnStatement | ifStatement | whileStatement | forStatement | print statement | assignment | expression
    statement() {
        if (this.currentToken.type === 'IMPORT') {
            return this.importStatement();
        }
        if (this.currentToken.type === 'RETURN') {
            return this.returnStatement();
        }
        if (this.currentToken.type === 'FUNCTION_DEFINE') {
            return this.funcStatement();
        }
        if (this.currentToken.type === 'IF') {
            return this.ifStatement();
        }
        if (this.currentToken.type === 'WHILE') {
            return this.whileStatement();
        }
        if (this.currentToken.type === 'FOR') {
            return this.forStatement();
        }
        if (this.currentToken.type === 'PRINT') {
            this.eat('PRINT');
            let value = this.expr();
            return { type: 'PRINT_STMT', expr: value, line: this.currentToken.line };
        }
        if (this.currentToken.type === 'IDENTIFIER') {
            let varToken = this.currentToken;
            this.eat('IDENTIFIER');
            if (this.currentToken.type === 'ASSIGN') {
                this.eat('ASSIGN');
                let exprNode = this.expr();
                return { type: 'ASSIGN', variable: varToken.value, expr: exprNode, line: varToken.line };
            } else {
                return { type: 'VARIABLE', value: varToken.value, line: varToken.line };
            }
        }
        if (this.currentToken.type === 'FUNCTION_CALL') {
            let funcName = this.currentToken.value;
            let params = this.currentToken.params;
            this.eat('FUNCTION_CALL');
            return { type: 'FUNCTION_CALL', value: funcName, params: params, line: this.currentToken.line };
        }
        return this.expr();
    }
    // Parses a program (a sequence of statements).
    parse() {
        let statements = [];
        while (this.currentToken.type !== 'EOF') {
            let stmt = this.statement();
            statements.push(stmt);
        }
        return { type: 'PROGRAM', statements: statements };
    }
}

// -----------------------------
// Interpreter: Walks the AST and executes the code.
class Interpreter {
    constructor(ast) {
        this.ast = ast;
        this.env = {}; // Environment for variable storage.
    }
    visit(node, env) {
        if (node.type === 'IMPORT') {
            // Handle import: read the file and run its code in the current environment.
            try {
                const importedCode = fs.readFileSync(node.filename, 'utf8');
                // Run the imported code in the current environment.
                this.env = runSnakeCode(importedCode, env);
            } catch (e) {
                throw new Error(`Line ${node.line}: Error importing file "${node.filename}": ${e.message}`);
            }

            return null;
        }
        if (node.type === 'RETURN_STMT') {
            // Evaluate and immediately return the value.
            let value = this.visit(node.expr, env);
            return value;
        }
        if (node.type === 'FUNCTION_CALL') {
            let func = env[node.value];
            let params = [];
            for (let i = 0; i < node.params.length; i++) {
                if (node.params[i].type === "IDENTIFIER") {
                    params.push(env[node.params[i].value]);
                } else {
                    params.push(node.params[i].value);
                }
            }
            let new_env = env;
            for (let i = 0; i < func.params.length; i++) {
                new_env[func.params[i].value] = params[i];
            }
            let returnval;
            for (let i = 0; i < func.body.statements.length; i++) {
                let x = this.visit(func.body.statements[i], new_env);
                if (func.body.statements[i].type === "RETURN_STMT") {
                    returnval = x;
                    break;
                }
            }
            return returnval;
        }
        if (node.type === 'FUNCTION_DEFINE') {
            env[node.value] = { type: 'FUNCTION_DEFINE', value: node.value, params: node.params, body: node.body };
            return null;
        }
        if (node.type === 'NUMBER') {
            return node.value;
        }
        if (node.type === 'STRING') {
            return node.value;
        }
        if (node.type === 'BOOLEAN') {
            return node.value;
        }
        if (node.type === 'LIST') {
            let elements = [];
            for (let i = 0; i < node.value.length; i++) {
                if (node.value[i].type === "IDENTIFIER") {
                    elements.push(env[node.value[i].value]);
                } else {
                    elements.push(node.value[i].value);
                }
            }
            return elements;
        }
        if (node.type === 'LIST_FUNC') {
            if (node.value === 'range') {
                let start = node.params[0].value;
                let end = node.params[1].value;
                let result = [];
                for (let i = start; i <= end; i++) {
                    result.push(i);
                }
                return result;
            }
            if (node.value === 'len') {
                return env[node.params[0].value].length;
            } else if (node.value === 'append') {
                if (node.params[1].type === "IDENTIFIER") {
                    return env[node.params[0].value].push(env[node.params[1].value]);
                } else {
                    return env[node.params[0].value].push(node.params[1].value);
                }
            } else if (node.value === 'pop') {
                return env[node.params[0].value].pop();
            } else if (node.value === 'item') {
                if (node.params[1].type === "IDENTIFIER") {
                    return env[node.params[0].value][env[node.params[1].value]];
                } else {
                    return env[node.params[0].value][parseInt(node.params[1].value)];
                }
            } else if (node.value === 'index') {
                return env[node.params[0].value].indexOf(parseInt(node.params[1].value));
            }
        }
        if (node.type === 'VARIABLE') {
            if (env.hasOwnProperty(node.value)) {
                return env[node.value];
            }

            throw new Error(`Line ${node.line}: Undefined variable: ${node.value}`);
        }
        if (node.type === 'BINOP') {
            const left = this.visit(node.left, env);
            const right = this.visit(node.right, env);
            switch (node.op) {
                case 'PLUS': return left + right;
                case 'MINUS': return left - right;
                case 'MUL': return left * right;
                case 'DIV': return left / right;
                case 'EQUALS': return left === right;
                case 'LESS': return left < right;
                case 'LESS_EQUALS': return left <= right;
                case 'GREATER': return left > right;
                case 'GREATER_EQUALS': return left >= right;
            }
        }
        if (node.type === 'ASSIGN') {
            const value = this.visit(node.expr, env);
            env[node.variable] = value;
            return value;
        }
        if (node.type === 'PRINT_STMT') {
            const value = this.visit(node.expr, env);
            console.log(value);
            return value;
        }
        if (node.type === 'BLOCK') {
            let result;
            for (const stmt of node.statements) {
                result = this.visit(stmt, env);
            }
            return result;
        }
        if (node.type === 'IF_STMT') {
            const condition = this.visit(node.condition, env);
            if (condition) {
                return this.visit(node.trueBranch, env);
            }
            for (let i = 0; i < node.elifBranches.length; i++) {
                if (this.visit(node.elifBranches[i].condition, env)) {
                    return this.visit(node.elifBranches[i].block, env);
                }
            }
            if (node.falseBranch) {
                return this.visit(node.falseBranch, env);
            }
            return null;
        }
        if (node.type === 'WHILE_STMT') {
            while (this.visit(node.condition, env)) {
                this.visit(node.body, env);
            }
            return null;
        }
        if (node.type === 'FOR_STMT') {
            let list;
            if (node.params[1].type == "IDENTIFIER") {
                list = env[node.params[1].value];
            } else {
                list = this.visit(node.params[1], env);
            }
            if (list == null) {
                throw new Error(`Line ${node.line}: Undefined variable: ${node.params[1].value}`);
            }
            let scoped_env = env;
            for (let i = 0; i < list.length; i++) {
                scoped_env[node.params[0].value] = list[i];
                this.visit(node.body, scoped_env);
            }
            return null;
        }
        throw new Error(`Line ${node.line}: Unknown node type: ${node.type}`);
    }
    interpret() {
        for (const stmt of this.ast.statements) {
            this.visit(stmt, this.env);
        }
    }
}

// -----------------------------
// runSnakeCode now accepts an optional environment parameter.
function runSnakeCode(code, env = {}) {
    const lexer = new Lexer(code);
    const parser = new Parser(lexer);
    const ast = parser.parse();
    const interpreter = new Interpreter(ast);
    interpreter.env = env;
    interpreter.interpret();
    return interpreter.env;
}

const fileName = process.argv[2];
if (!fileName) {
    console.error('Usage: node snake.js <path-to-snake-code-file>');
    process.exit(1);
}

try {
    const code = fs.readFileSync(fileName, 'utf8');
    runSnakeCode(code);
} catch (err) {
    console.error('Error reading file:', err.message);
}
