// A little arithmetic in TypeScript 3.6 by SUZUKI Hisao (R01.08.04/R01.11.04)
//  derived from arith.ts at github.com/nukata/little-scheme-in-typescript
'use strict';
// A Number value is treated as an inexact number.
// A BigInt value is treated as an exact number.
// Any intergers should be represented by BigInt if possible.
// If the runtime does not have BigInt, arithmetic will be done with Number.
const ZERO = (typeof BigInt === 'undefined') ? 0 : BigInt(0);
const ONE = (typeof BigInt === 'undefined') ? 1 : BigInt(1);
// Is x a Numeric?
function isNumeric(x) {
    let t = typeof x;
    return t === 'number' || t === 'bigint';
}
// x + y
function add(x, y) {
    if (typeof x === 'number') {
        if (typeof y === 'number')
            return x + y;
        else
            return x + Number(y);
    }
    else {
        if (typeof y === 'number')
            return Number(x) + y;
        else
            return x + y;
    }
}
// x - y
function subtract(x, y) {
    if (typeof x === 'number') {
        if (typeof y === 'number')
            return x - y;
        else
            return x - Number(y);
    }
    else {
        if (typeof y === 'number')
            return Number(x) - y;
        else
            return x - y;
    }
}
// x * y
function multiply(x, y) {
    if (typeof x === 'number') {
        if (typeof y === 'number')
            return x * y;
        else
            return x * Number(y);
    }
    else {
        if (typeof y === 'number')
            return Number(x) * y;
        else
            return x * y;
    }
}
// x / y (rounded quotient)
function divide(x, y) {
    return Number(x) / Number(y);
}
// Calculate the quotient of x and y.
function quotient(x, y) {
    if (typeof x === 'number' || typeof y === 'number') {
        let q = Math.trunc(Number(x) / Number(y));
        if (typeof BigInt === 'undefined')
            return q;
        else
            return BigInt(q);
    }
    else {
        return x / y;
    }
}
// Calculate the remainder of the quotient of x and y.
function remainder(x, y) {
    if (typeof x === 'number' || typeof y === 'number')
        return Number(x) % Number(y);
    else
        return x % y;
}
// Compare x and y.
// -1, 0 or 1 as x is less than, equal to, or greater than y.
function compare(x, y) {
    if (typeof x === 'number') {
        if (typeof y === 'number')
            return Math.sign(x - y);
        else
            return Math.sign(x - Number(y));
    }
    else {
        if (typeof y === 'number')
            return Math.sign(Number(x) - y);
        else
            return (x < y) ? -1 : (y < x) ? 1 : 0;
    }
}
// Try to parse the token as a Numeric or null.
function tryToParse(token) {
    try {
        return BigInt(token);
    }
    catch (ex) {
        const n = Number(token);
        if (isNaN(n))
            return null;
        return n;
    }
}
/*
  Nukata Lisp 1.91.0 in TypeScript 3.6 by SUZUKI Hisao (H28.02.08/R01.11.05)
  $ tsc -t ESNext --outFile lisp.js lisp.ts && node lisp.js
*/
/// <reference path="arith.ts" />
// Class and functions for the convenience of porting from Dart
// Base class of user-defined exceptions
class Exception extends Error {
    constructor(...params) {
        super(...params);
        // Capture the stack trace if it runs on Node.js.
        let capture = Error["captureStackTrace"];
        if (capture !== undefined)
            capture(this, this.constructor);
        // If Function#name in ES6 is available,
        // set the class name at runtime to this.name;
        // otherwise set a fixed name to it.
        let name = this.constructor["name"];
        this.name = (name !== undefined) ? name : "Exception";
    }
}
// A substitution of assert statement
// XXX You must supply message to display what has failed.
// XXX Costs to call this remains even if you empty the function body.
function assert(x, message) {
    if (!x)
        throw new Error("Assertion Failure: " + (message || ""));
}
var write; // Output string s (a new line on \n char).
var exit; // Terminate the process with exit code n.
//----------------------------------------------------------------------
// Lisp cons cell
class Cell {
    constructor(car, cdr) {
        this.car = car;
        this.cdr = cdr;
    }
    toString() { return "(" + this.car + " . " + this.cdr + ")"; }
    // Length as a list
    get length() { return foldl(0, this, (i, e) => i + 1); }
}
// foldl(x, (a b c), fn) => fn(fn(fn(x, a), b), c)
function foldl(x, j, fn) {
    while (j !== null) {
        x = fn(x, j.car);
        j = j.cdr;
    }
    return x;
}
// mapcar((a b c), fn) => (fn(a) fn(b) fn(c))
function mapcar(j, fn) {
    if (j === null)
        return null;
    let a = fn(j.car);
    let d = j.cdr;
    if (d instanceof Cell)
        d = mapcar(d, fn);
    if (Object.is(j.car, a) && Object.is(j.cdr, d))
        return j;
    return new Cell(a, d);
}
// Lisp symbol
class Sym {
    constructor(name) {
        this.name = name;
    } // Construct an uninterned symbol.
    toString() { return this.name; }
    get isInterned() {
        return symTable[this.name] === this;
    }
}
// Expression keyword
class Keyword extends Sym {
    constructor(name) { super(name); }
}
// The table of interned symbols
const symTable = {};
// Construct an interned symbol; construct a Keyword if isKeyword holds.
function newSym(name, isKeyword = false) {
    let result = symTable[name];
    assert((result === undefined || !isKeyword), name);
    if (result === undefined) {
        result = isKeyword ? new Keyword(name) : new Sym(name);
        symTable[name] = result;
    }
    return result;
}
function newKeyword(name) { return newSym(name, true); }
const backQuoteSym = newSym("`");
const commaAtSym = newSym(",@");
const commaSym = newSym(",");
const dotSym = newSym(".");
const leftParenSym = newSym("(");
const rightParenSym = newSym(")");
const singleQuoteSym = newSym("'");
const appendSym = newSym("append");
const consSym = newSym("cons");
const listSym = newSym("list");
const restSym = newSym("&rest");
const unquoteSym = newSym("unquote");
const unquoteSplicingSym = newSym("unquote-splicing");
const condSym = newKeyword("cond");
const lambdaSym = newKeyword("lambda");
const macroSym = newKeyword("macro");
const prognSym = newKeyword("progn");
const quasiquoteSym = newKeyword("quasiquote");
const quoteSym = newKeyword("quote");
const setqSym = newKeyword("setq");
//----------------------------------------------------------------------
// Get cdr of list x as a Cell or null.
function cdrCell(x) {
    let k = x.cdr;
    if (k instanceof Cell)
        return k;
    else if (k === null)
        return null;
    else
        throw new EvalException("proper list expected", x);
}
// Common base class of Lisp functions
class Func {
    // carity is a number of arguments, made negative if the func has &rest.
    constructor(carity) {
        this.carity = carity;
    }
    get arity() {
        return (this.carity < 0) ? -this.carity : this.carity;
    }
    get hasRest() {
        return (this.carity < 0);
    }
    get fixedArgs() {
        return (this.carity < 0) ? -this.carity - 1 : this.carity;
    }
    // Make a call-frame from a list of actual arguments.
    makeFrame(arg) {
        let frame = new Array(this.arity);
        let n = this.fixedArgs;
        let i = 0;
        for (; i < n && arg !== null; i++) { // Set the list of fiexed args.
            frame[i] = arg.car;
            arg = cdrCell(arg);
        }
        if (i !== n || (arg !== null && !this.hasRest))
            throw new EvalException("arity not matched", this);
        if (this.hasRest)
            frame[n] = arg;
        return frame;
    }
    // Evaluate each expression of a frame.
    evalFrame(frame, interp, env) {
        let n = this.fixedArgs;
        for (let i = 0; i < n; i++)
            frame[i] = interp.eval(frame[i], env);
        if (this.hasRest && frame[n] instanceof Cell) {
            let z = null;
            let y = null;
            for (let j = frame[n]; j != null; j = cdrCell(j)) {
                let e = interp.eval(j.car, env);
                let x = new Cell(e, null);
                if (z === null)
                    z = x;
                else
                    y.cdr = x;
                y = x;
            }
            frame[n] = z;
        }
    }
}
// Common base class of functions which are defined with Lisp expressions
class DefinedFunc extends Func {
    // body is a Lisp list as the function body.
    constructor(carity, body) {
        super(carity);
        this.body = body;
    }
}
// Compiled macro expression
class Macro extends DefinedFunc {
    constructor(carity, body) { super(carity, body); }
    toString() { return `#<macro:${this.carity}:${str(this.body)}>`; }
    // Expand the macro with a list of actual arguments.
    expandWith(interp, arg) {
        let frame = this.makeFrame(arg);
        let env = new Cell(frame, null);
        let x = null;
        for (let j = this.body; j != null; j = cdrCell(j))
            x = interp.eval(j.car, env);
        return x;
    }
    static make(carity, body, env) {
        assert(env === null);
        return new Macro(carity, body);
    }
}
// Compiled lambda expression (within another function)
class Lambda extends DefinedFunc {
    constructor(carity, body) { super(carity, body); }
    toString() { return `#<lambda:${this.carity}:${str(this.body)}>`; }
    static make(carity, body, env) {
        assert(env === null);
        return new Lambda(carity, body);
    }
}
// Compiled lambda expression (Closure with environment)
class Closure extends DefinedFunc {
    // env is the environment of the closure.
    constructor(carity, body, env) {
        super(carity, body);
        this.env = env;
    }
    static makeFrom(x, env) {
        return new Closure(x.carity, x.body, env);
    }
    toString() {
        return `#<closure:${this.carity}:${str(this.env)}:${str(this.body)}>`;
    }
    // Make a new environment from a list of actual arguments.
    makeEnv(interp, arg, interpEnv) {
        let frame = this.makeFrame(arg);
        this.evalFrame(frame, interp, interpEnv);
        return new Cell(frame, this.env); // Prepend the frame to the env.
    }
    static make(carity, body, env) {
        return new Closure(carity, body, env);
    }
}
// Built-in function
class BuiltInFunc extends Func {
    // name is the function name; body is the function body.
    constructor(name, carity, body) {
        super(carity);
        this.name = name;
        this.body = body;
    }
    toString() { return "#<" + this.name + ":" + this.carity + ">"; }
    // Invoke the built-in function with a list of actual arguments.
    evalWith(interp, arg, interpEnv) {
        let frame = this.makeFrame(arg);
        this.evalFrame(frame, interp, interpEnv);
        try {
            return this.body(frame);
        }
        catch (ex) {
            if (ex instanceof EvalException)
                throw ex;
            else
                throw new EvalException(ex + " -- " + this.name, frame);
        }
    }
}
// Bound variable in a compiled lambda/macro expression
class Arg {
    constructor(level, offset, symbol) {
        this.level = level;
        this.offset = offset;
        this.symbol = symbol;
    }
    toString() {
        return "#" + this.level + ":" + this.offset + ":" + this.symbol;
    }
    // Set a value x to the location corresponding to the variable in env.
    setValue(x, env) {
        for (let i = 0; i < this.level; i++)
            env = env.cdr;
        env.car[this.offset] = x;
    }
    // Get a value from the location corresponding to the variable in env.
    getValue(env) {
        for (let i = 0; i < this.level; i++)
            env = env.cdr;
        return env.car[this.offset];
    }
}
// Exception in evaluation
class EvalException extends Exception {
    constructor(msg, x, quoteString = true) {
        super(msg + ": " + str(x, quoteString));
        this.trace = [];
    }
    toString() {
        let s = "EvalException: " + this.message;
        for (let line of this.trace)
            s += "\n\t" + line;
        return s;
    }
}
// Exception which indicates an absence of a variable
class NotVariableException extends EvalException {
    constructor(x) {
        super("variable expected", x);
    }
}
// Exception thrown when something does not have an expected format
class FormatException extends Exception {
    constructor(msg) { super(msg); }
}
// Singleton for end-of-file
const EndOfFile = { toString: () => "EOF" };
//----------------------------------------------------------------------
// Core of the interpreter
class Interp {
    constructor() {
        // Table of the global values of symbols
        // XXX Cannot use Sym type for keys; use Sym#name: string instead.
        this.globals = {};
        this.def("car", 1, (a) => (a[0] === null) ? null : a[0].car);
        this.def("cdr", 1, (a) => (a[0] === null) ? null : a[0].cdr);
        this.def("cons", 2, (a) => new Cell(a[0], a[1]));
        this.def("atom", 1, (a) => (a[0] instanceof Cell) ? null : true);
        this.def("eq", 2, (a) => Object.is(a[0], a[1]) ? true : null);
        this.def("list", -1, (a) => a[0]);
        this.def("rplaca", 2, (a) => { a[0].car = a[1]; return a[1]; });
        this.def("rplacd", 2, (a) => { a[0].cdr = a[1]; return a[1]; });
        this.def("length", 1, (a) => (a[0] === null ? 0 : a[0].length));
        this.def("stringp", 1, (a) => (typeof a[0] === "string") ? true : null);
        this.def("numberp", 1, (a) => isNumeric(a[0]) ? true : null);
        this.def("eql", 2, (a) => {
            let x = a[0];
            let y = a[1];
            return (x === y) ? true :
                (isNumeric(x) && isNumeric(y) && compare(x, y) === 0) ? true :
                    null;
        });
        this.def("<", 2, (a) => (compare(a[0], a[1]) < 0) ? true : null);
        this.def("%", 2, (a) => remainder(a[0], a[1]));
        this.def("mod", 2, (a) => {
            let x = a[0];
            let y = a[1];
            let q = remainder(x, y);
            return (compare(multiply(x, y), ZERO) < 0) ? add(q, y) : q;
        });
        this.def("+", -1, (a) => foldl(ZERO, a[0], (i, j) => add(i, j)));
        this.def("*", -1, (a) => foldl(ONE, a[0], (i, j) => multiply(i, j)));
        this.def("-", -2, (a) => {
            let x = a[0];
            let y = a[1];
            return (y == null) ? -x : foldl(x, y, (i, j) => subtract(i, j));
        });
        this.def("/", -3, (a) => foldl(a[0] / a[1], a[2], (i, j) => divide(i, j)));
        this.def("truncate", -2, (a) => {
            let x = a[0];
            let y = a[1];
            if (y === null) {
                return quotient(x, ONE);
            }
            else if (y.cdr === null) {
                return quotient(x, y.car);
            }
            else {
                throw "one or two arguments expected";
            }
        });
        this.def("prin1", 1, (a) => {
            write(str(a[0], true));
            return a[0];
        });
        this.def("princ", 1, (a) => {
            write(str(a[0], false));
            return a[0];
        });
        this.def("terpri", 0, (a) => {
            write("\n");
            return true;
        });
        const gensymCounter = "*gensym-counter*";
        this.globals[gensymCounter] = 1;
        this.def("gensym", 0, (a) => {
            let i = this.globals[gensymCounter];
            this.globals[gensymCounter] = i + 1;
            return new Sym("G" + i); // an uninterned symbol
        });
        this.def("make-symbol", 1, (a) => new Sym(a[0]));
        this.def("intern", 1, (a) => newSym(a[0]));
        this.def("symbol-name", 1, (a) => a[0].name);
        this.def("apply", 2, (a) => this.eval(new Cell(a[0], mapcar(a[1], qqQuote)), null));
        this.def("exit", 1, (a) => exit(a[0]));
        this.def("dump", 0, (a) => {
            let s = null;
            for (let x in this.globals)
                s = new Cell(x, s);
            return s;
        });
        this.globals["*version*"] = // named after Tōkai-dō Mikawa-koku
            new Cell(1.910, // Nukata-gun (東海道 三河国 額田郡)
            new Cell("TypeScript", new Cell("Nukata Lisp", null)));
    }
    // Define a built-in function by giving a name, a carity, and a body.
    def(name, carity, body) {
        this.globals[name] = new BuiltInFunc(name, carity, body);
    }
    // Evaluate a Lisp expression in an environment.
    eval(x, env) {
        try {
            for (;;) {
                if (x instanceof Arg) {
                    return x.getValue(env);
                }
                else if (x instanceof Sym) {
                    let value = this.globals[x.name];
                    if (value === undefined)
                        throw new EvalException("void variable", x);
                    return value;
                }
                else if (x instanceof Cell) {
                    let c = x;
                    let fn = c.car;
                    let arg = cdrCell(c);
                    if (fn instanceof Keyword) {
                        switch (fn) {
                            case quoteSym:
                                if (arg !== null && arg.cdr === null)
                                    return arg.car;
                                throw new EvalException("bad quote", c);
                            case prognSym:
                                x = this.evalProgN(arg, env);
                                break;
                            case condSym:
                                x = this.evalCond(arg, env);
                                break;
                            case setqSym:
                                return this.evalSetQ(arg, env);
                            case lambdaSym:
                                return this.compile(arg, env, Closure.make);
                            case macroSym:
                                if (env !== null)
                                    throw new EvalException("nested macro", c);
                                return this.compile(arg, null, Macro.make);
                            case quasiquoteSym:
                                if (arg !== null && arg.cdr === null) {
                                    x = qqExpand(arg.car);
                                    break;
                                }
                                throw new EvalException("bad quasiquote", c);
                            default:
                                throw new EvalException("bad keyword", fn);
                        }
                    }
                    else { // Application of a function
                        // Expand fn = eval(fn, env) here on Sym for speed.
                        if (fn instanceof Sym) {
                            fn = this.globals[fn.name];
                            if (fn === undefined)
                                throw new EvalException("undefined", c.car);
                        }
                        else {
                            fn = this.eval(fn, env);
                        }
                        if (fn instanceof Closure) {
                            let cl = fn;
                            env = cl.makeEnv(this, arg, env);
                            x = this.evalProgN(cl.body, env);
                        }
                        else if (fn instanceof Macro) {
                            x = fn.expandWith(this, arg);
                        }
                        else if (fn instanceof BuiltInFunc) {
                            return fn.evalWith(this, arg, env);
                        }
                        else {
                            throw new EvalException("not applicable", fn);
                        }
                    }
                }
                else if (x instanceof Lambda) {
                    return Closure.makeFrom(x, env);
                }
                else {
                    return x; // numbers, strings, null etc.
                }
            }
        }
        catch (ex) {
            if (ex instanceof EvalException) {
                let eex = ex;
                if (eex.trace.length < 10)
                    eex.trace.push(str(x));
            }
            throw ex;
        }
    }
    // (progn E1 E2 .. En) => Evaluate E1, E2, .. except for En and return it.
    evalProgN(j, env) {
        if (j === null)
            return null;
        for (;;) {
            let x = j.car;
            j = cdrCell(j);
            if (j === null)
                return x; // The tail exp will be evaluated at the caller.
            this.eval(x, env);
        }
    }
    // Evaluate a conditional expression and return the selection unevaluated.
    evalCond(j, env) {
        for (; j !== null; j = cdrCell(j)) {
            let clause = j.car;
            if (clause instanceof Cell) {
                let result = this.eval(clause.car, env);
                if (result !== null) { // If the condition holds
                    let body = cdrCell(clause);
                    if (body === null)
                        return qqQuote(result);
                    else
                        return this.evalProgN(body, env);
                }
            }
            else if (clause !== null) {
                throw new EvalException("cond test expected", clause);
            }
        }
        return null; // No clause holds.
    }
    // (setq V1 E1 ..) => Evaluate Ei and assign it to Vi; return the last.
    evalSetQ(j, env) {
        let result = null;
        for (; j !== null; j = cdrCell(j)) {
            let lval = j.car;
            j = cdrCell(j);
            if (j === null)
                throw new EvalException("right value expected", lval);
            result = this.eval(j.car, env);
            if (lval instanceof Arg)
                lval.setValue(result, env);
            else if (lval instanceof Sym && !(lval instanceof Keyword))
                this.globals[lval.name] = result;
            else
                throw new NotVariableException(lval);
        }
        return result;
    }
    // Compile a Lisp list (macro ..) or (lambda ..).
    compile(arg, env, make) {
        if (arg === null)
            throw new EvalException("arglist and body expected", arg);
        let table = {};
        let [hasRest, arity] = makeArgTable(arg.car, table);
        let body = cdrCell(arg);
        body = scanForArgs(body, table);
        body = this.expandMacros(body, 20); // Expand macros up to 20 nestings
        body = this.compileInners(body);
        return make((hasRest) ? -arity : arity, body, env);
    }
    // Expand macros and quasi-quotations in an expression.
    expandMacros(j, count) {
        if (count > 0 && j instanceof Cell) {
            let k = j.car;
            switch (k) {
                case quoteSym:
                case lambdaSym:
                case macroSym:
                    return j;
                case quasiquoteSym:
                    let d = cdrCell(j);
                    if (d !== null && d.cdr === null) {
                        let z = qqExpand(d.car);
                        return this.expandMacros(z, count);
                    }
                    throw new EvalException("bad quasiquote", j);
                default:
                    if (k instanceof Sym)
                        k = this.globals[k.name];
                    if (k instanceof Macro) {
                        let d = cdrCell(j);
                        let z = k.expandWith(this, d);
                        return this.expandMacros(z, count - 1);
                    }
                    return mapcar(j, (x) => this.expandMacros(x, count));
            }
        }
        else {
            return j;
        }
    }
    // Replace inner lambda expressions with Lambda instances.
    compileInners(j) {
        if (j instanceof Cell) {
            let k = j.car;
            switch (k) {
                case quoteSym:
                    return j;
                case lambdaSym:
                    let d = cdrCell(j);
                    return this.compile(d, null, Lambda.make);
                case macroSym:
                    throw new EvalException("nested macro", j);
                default:
                    return mapcar(j, (x) => this.compileInners(x));
            }
        }
        else {
            return j;
        }
    }
}
//----------------------------------------------------------------------
// Make an argument table; return a pair of rest-yes/no and the arity.
function makeArgTable(arg, table) {
    if (arg === null) {
        return [false, 0];
    }
    else if (arg instanceof Cell) {
        let ag = arg;
        let offset = 0; // offset value within the call-frame
        let hasRest = false;
        for (; ag !== null; ag = cdrCell(ag)) {
            let j = ag.car;
            if (hasRest)
                throw new EvalException("2nd rest", j);
            if (j === restSym) { // &rest var
                ag = cdrCell(ag);
                if (ag === null)
                    throw new NotVariableException(ag);
                j = ag.car;
                if (j === restSym)
                    throw new NotVariableException(j);
                hasRest = true;
            }
            let sym;
            if (j instanceof Sym)
                sym = j;
            else if (j instanceof Arg)
                sym = j.symbol;
            else
                throw new NotVariableException(j);
            if (sym.name in table)
                throw new EvalException("duplicated argument name", j);
            table[sym.name] = new Arg(0, offset, sym);
            offset++;
        }
        return [hasRest, offset];
    }
    else {
        throw new EvalException("arglist expected", arg);
    }
}
// Scan 'j' for formal arguments in 'table' and replace them with Args.
// And scan 'j' for free Args not in 'table' and promote their levels.
function scanForArgs(j, table) {
    if (j instanceof Sym) {
        let k = table[j.name];
        return (k === undefined) ? j : k;
    }
    else if (j instanceof Arg) {
        let ag = j;
        let k = table[ag.symbol.name];
        return (k === undefined) ?
            new Arg(ag.level + 1, ag.offset, ag.symbol) : k;
    }
    else if (j instanceof Cell) {
        let c = j;
        if (c.car === quoteSym) {
            return c;
        }
        else if (c.car === quasiquoteSym) {
            return new Cell(quasiquoteSym, scanForQQ(c.cdr, table, 0));
        }
        else {
            return mapcar(j, (x) => scanForArgs(x, table));
        }
    }
    else {
        return j;
    }
}
// Scan for quasi-quotes and scanForArgs them depending on the nesting level.
function scanForQQ(j, table, level) {
    if (j instanceof Cell) {
        let k = j.car;
        if (k === quasiquoteSym) {
            return new Cell(k, scanForQQ(j.cdr, table, level + 1));
        }
        else if (k === unquoteSym || k === unquoteSplicingSym) {
            let d = (level === 0) ?
                scanForArgs(j.cdr, table) :
                scanForQQ(j.cdr, table, level - 1);
            if (Object.is(d, j.cdr))
                return j;
            return new Cell(k, d);
        }
        else {
            return mapcar(j, (x) => scanForQQ(x, table, level));
        }
    }
    else {
        return j;
    }
}
//----------------------------------------------------------------------
// Quasi-Quotation
// Expand x of any quasi-quotation `x into the equivalent S-expression.
function qqExpand(x) {
    return qqExpand0(x, 0); // Begin with the nesting level 0.
}
function qqExpand0(x, level) {
    if (x instanceof Cell) {
        let c = x;
        if (c.car === unquoteSym) { // ,a
            if (level === 0)
                return c.cdr.car; // ,a => a
        }
        let t = qqExpand1(c, level);
        if (t.car instanceof Cell && t.cdr === null) {
            let k = t.car;
            if (k.car == listSym || k.car === consSym)
                return k;
        }
        return new Cell(appendSym, t);
    }
    else {
        return qqQuote(x);
    }
}
// Quote x so that the result evaluates to x.
function qqQuote(x) {
    if (x instanceof Sym || x instanceof Cell)
        return new Cell(quoteSym, new Cell(x, null));
    return x;
}
// Expand x of `x so that the result can be used as an argument of append.
// Example 1: (,a b) => ((list a 'b))
// Example 2: (,a ,@(cons 2 3)) => ((cons a (cons 2 3)))
function qqExpand1(x, level) {
    if (x instanceof Cell) {
        let c = x;
        if (c.car === unquoteSym) { // ,a
            if (level === 0)
                return x.cdr; // ,a => (a)
            level--;
        }
        else if (c.car === quasiquoteSym) { // `a
            level++;
        }
        let h = qqExpand2(c.car, level);
        let t = qqExpand1(c.cdr, level); // !== null
        if (t.car === null && t.cdr === null) {
            return new Cell(h, null);
        }
        else if (h instanceof Cell) {
            let hc = h;
            if (hc.car === listSym) {
                if (t.car instanceof Cell) {
                    let tcar = t.car;
                    if (tcar.car === listSym) {
                        let hh = qqConcat(hc, tcar.cdr);
                        return new Cell(hh, t.cdr);
                    }
                }
                if (hc.cdr instanceof Cell) {
                    let hh = qqConsCons(hc.cdr, t.car);
                    return new Cell(hh, t.cdr);
                }
            }
        }
        return new Cell(h, t);
    }
    else {
        return new Cell(qqQuote(x), null);
    }
}
// (1 2), (3 4) => (1 2 3 4)
function qqConcat(x, y) {
    if (x === null)
        return y;
    return new Cell(x.car, qqConcat(x.cdr, y));
}
// (1 2 3), "a" => (cons 1 (cons 2 (cons 3 "a")))
function qqConsCons(x, y) {
    if (x === null)
        return y;
    return new Cell(consSym, new Cell(x.car, new Cell(qqConsCons(x.cdr, y), null)));
}
// Expand x.car (=y) of `x so that the result can be used as an arg of append.
// Example: ,a => (list a); ,@(foo 1 2) => (foo 1 2); b => (list 'b)
function qqExpand2(y, level) {
    if (y instanceof Cell) {
        let yc = y;
        switch (yc.car) {
            case unquoteSym: // ,a
                if (level === 0)
                    return new Cell(listSym, yc.cdr); // ,a => (list a)
                level--;
                break;
            case unquoteSplicingSym: // ,@a
                if (level === 0)
                    return yc.cdr.car; // ,@a => a
                level--;
                break;
            case quasiquoteSym: // `a
                level++;
                break;
        }
    }
    return new Cell(listSym, new Cell(qqExpand0(y, level), null));
}
//----------------------------------------------------------------------
// A list of tokens, which works as a reader of Lisp expressions
class Reader {
    constructor() {
        this.tokens = [];
        this.lineNo = 1;
    }
    // Split a text into a list of tokens and append it to this.tokens.
    // For "(a \n 1)" it appends ["(", "a", "\n", "1", ")", "\n"] to tokens.
    push(text) {
        let tokenPat = /\s+|;.*$|("(\\.?|.)*?"|,@?|[^()'`~"; \t]+|.)/g;
        for (let line of text.split("\n")) {
            for (;;) {
                let result = tokenPat.exec(line);
                if (result === null)
                    break;
                let s = result[1];
                if (s !== undefined)
                    this.tokens.push(s);
            }
            this.tokens.push("\n");
        }
    }
    // Make this be a clone of the other.
    copyFrom(other) {
        this.tokens = other.tokens.slice();
        this.lineNo = other.lineNo;
    }
    // Make this have no tokens.
    clear() {
        this.tokens.length = 0;
    }
    // Does this have no tokens?
    isEmpty() {
        for (let t of this.tokens)
            if (t !== "\n")
                return false;
        return true;
    }
    // Read a Lisp expression; throw EndOfFile if this.tokens run out.
    read() {
        try {
            this.readToken();
            return this.parseExpression();
        }
        catch (ex) {
            if (ex === EndOfFile)
                throw EndOfFile;
            else if (ex instanceof FormatException)
                throw new EvalException("syntax error", ex.message + " at " + this.lineNo, false);
            else
                throw ex;
        }
    }
    parseExpression() {
        switch (this.token) {
            case leftParenSym: // (a b c)
                this.readToken();
                return this.parseListBody();
            case singleQuoteSym: // 'a => (quote a)
                this.readToken();
                return new Cell(quoteSym, new Cell(this.parseExpression(), null));
            case backQuoteSym: // `a => (quasiquote a)
                this.readToken();
                return new Cell(quasiquoteSym, new Cell(this.parseExpression(), null));
            case commaSym: // ,a => (unquote a)
                this.readToken();
                return new Cell(unquoteSym, new Cell(this.parseExpression(), null));
            case commaAtSym: // ,@a => (unquote-splicing a)
                this.readToken();
                return new Cell(unquoteSplicingSym, new Cell(this.parseExpression(), null));
            case dotSym:
            case rightParenSym:
                throw new FormatException('unexpected "' + this.token + '"');
            default:
                return this.token;
        }
    }
    parseListBody() {
        if (this.token === rightParenSym) {
            return null;
        }
        else {
            let e1 = this.parseExpression();
            this.readToken();
            let e2;
            if (this.token == dotSym) { // (a . b)
                this.readToken();
                e2 = this.parseExpression();
                this.readToken();
                if (this.token !== rightParenSym)
                    throw new FormatException('")" expected: ' + this.token);
            }
            else {
                e2 = this.parseListBody();
            }
            return new Cell(e1, e2);
        }
    }
    // Read the next token and set it to this.token.
    readToken() {
        for (;;) {
            let t = this.tokens.shift();
            if (t === undefined) {
                throw EndOfFile;
            }
            else if (t === "\n") {
                this.lineNo += 1;
            }
            else {
                if (t[0] === '"') {
                    let s = t;
                    let n = s.length - 1;
                    if (n < 1 || s[n] !== '"')
                        throw new FormatException("bad string: " + s);
                    s = s.substring(1, n);
                    s = s.replace(/\\./g, (m) => {
                        let val = Reader.escapes[m];
                        return (val === undefined) ? m : val;
                    });
                    this.token = s;
                    return;
                }
                let n = tryToParse(t);
                if (n !== null)
                    this.token = n;
                else if (t === "nil")
                    this.token = null;
                else if (t === "t")
                    this.token = true;
                else
                    this.token = newSym(t);
                return;
            }
        }
    }
}
Reader.escapes = {
    "\\\\": "\\",
    '\\"': '"',
    "\\n": "\n", "\\r": "\r", "\\f": "\f",
    "\\b": "\b", "\\t": "\t", "\\v": "\v"
};
//----------------------------------------------------------------------
// Mapping from a quote symbol to its string representation
const quotes = {
    [quoteSym.name]: "'",
    [quasiquoteSym.name]: "`",
    [unquoteSym.name]: ",",
    [unquoteSplicingSym.name]: ",@"
};
// Make a string representation of Lisp expression
function str(x, quoteString = true, count, printed) {
    if (x === null) {
        return "nil";
    }
    else if (x === true) {
        return "t";
    }
    else if (x instanceof Cell) {
        let c = x;
        if (c.car instanceof Sym) {
            let q = quotes[c.car.name];
            if (q !== undefined && c.cdr instanceof Cell)
                if (c.cdr.cdr == null)
                    return q + str(c.cdr.car, true, count, printed);
        }
        return "(" + strListBody(x, count, printed) + ")";
    }
    else if (typeof x === "string") {
        if (!quoteString)
            return x;
        let bf = ['"'];
        for (let ch of x) {
            switch (ch) {
                case "\b":
                    bf.push("\\b");
                    break;
                case "\t":
                    bf.push("\\t");
                    break;
                case "\n":
                    bf.push("\\n");
                    break;
                case "\v":
                    bf.push("\\v");
                    break;
                case "\f":
                    bf.push("\\f");
                    break;
                case "\r":
                    bf.push("\\r");
                    break;
                case "\"":
                    bf.push("\\\"");
                    break;
                case "\\":
                    bf.push("\\\\");
                    break;
                default:
                    bf.push(ch);
                    break;
            }
        }
        bf.push('"');
        return bf.join("");
    }
    else if (x instanceof Array) {
        let s = x.map((e) => str(e, true, count, printed)).join(", ");
        return "[" + s + "]";
    }
    else if (x instanceof Sym) {
        return (x.isInterned) ? x.name : "#:" + x;
    }
    else {
        return x + "";
    }
}
// Make a string representation of list, omitting its "(" and ")".
function strListBody(x, count, printed) {
    if (printed === undefined)
        printed = [];
    if (count === undefined)
        count = 4; // threshold of ellipsis for circular lists
    let s = [];
    let y;
    for (y = x; y instanceof Cell; y = y.cdr) {
        if (printed.indexOf(y) < 0) {
            printed.push(y);
            count = 4;
        }
        else {
            count--;
            if (count < 0) {
                s.push("..."); // an ellipsis for a circular list
                return s.join(" ");
            }
        }
        s.push(str(y.car, true, count, printed));
    }
    if (y !== null) {
        s.push(".");
        s.push(str(y, true, count, printed));
    }
    for (y = x; y instanceof Cell; y = y.cdr) {
        let i = printed.indexOf(y);
        if (i >= 0)
            printed.splice(i, 1);
    }
    return s.join(" ");
}
// Read-Eval-Print Loop
class REPL {
    constructor() {
        this.stdInTokens = new Reader();
        this.oldTokens = new Reader();
        this.readState = undefined;
    }
    // Read an expression from the standard-in asynchronously.
    readExpression(prompt1, prompt2) {
        this.oldTokens.copyFrom(this.stdInTokens);
        try {
            return this.stdInTokens.read();
        }
        catch (ex) {
            if (ex === EndOfFile) {
                if (this.readState !== undefined)
                    throw Error("bad read state");
                write(this.oldTokens.isEmpty() ? prompt1 : prompt2);
                return new Promise((resolve, reject) => {
                    this.readState = [resolve, reject, prompt1, prompt2];
                    // Continue into stdInOnData/stdInOnEnd.
                });
            }
            else {
                this.stdInTokens.clear(); // Discard the erroneous tokens.
                throw ex;
            }
        }
    }
    stdInOnData(line) {
        this.oldTokens.push(line);
        this.stdInTokens.copyFrom(this.oldTokens);
        if (this.readState !== undefined) {
            let [resolve, reject, prompt1, prompt2] = this.readState;
            try {
                let exp = this.stdInTokens.read();
                resolve(exp);
                this.readState = undefined;
            }
            catch (ex) {
                if (ex === EndOfFile) {
                    write(this.oldTokens.isEmpty() ? prompt1 : prompt2);
                    // Continue into stdInOnData/stdInOnEnd.
                }
                else {
                    this.stdInTokens.clear(); // Discard the erroneous tokens.
                    reject(ex);
                    this.readState = undefined;
                }
            }
        }
    }
    stdInOnEnd() {
        if (this.readState !== undefined) {
            let [resolve, _1, _2, _3] = this.readState;
            resolve(EndOfFile);
            this.readState = undefined;
        }
    }
    // Repeat Read-Eval-Print until End-Of-File asynchronously.
    async readEvalPrintLoop(interp) {
        for (;;) {
            try {
                let exp = await this.readExpression("> ", "  ");
                if (exp === EndOfFile) {
                    write("Goodbye\n");
                    return;
                }
                let result = interp.eval(exp, null);
                write(str(result) + "\n");
            }
            catch (ex) {
                if (ex instanceof EvalException)
                    write(ex + "\n");
                else
                    throw ex;
            }
        }
    }
}
// Evaluate a string as a list of Lisp exps; return the result of the last exp.
function run(interp, text) {
    let tokens = new Reader();
    tokens.push(text);
    let result = undefined;
    while (!tokens.isEmpty()) {
        let exp = tokens.read();
        result = interp.eval(exp, null);
    }
    return result;
}
// Lisp initialization script
const prelude = `
(setq defmacro
      (macro (name args &rest body)
             \`(progn (setq ,name (macro ,args ,@body))
                     ',name)))

(defmacro defun (name args &rest body)
  \`(progn (setq ,name (lambda ,args ,@body))
          ',name))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
(defun caaar (x) (car (car (car x))))
(defun caadr (x) (car (car (cdr x))))
(defun cadar (x) (car (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cdaar (x) (cdr (car (car x))))
(defun cdadr (x) (cdr (car (cdr x))))
(defun cddar (x) (cdr (cdr (car x))))
(defun cdddr (x) (cdr (cdr (cdr x))))
(defun not (x) (eq x nil))
(defun consp (x) (not (atom x)))
(defun print (x) (prin1 x) (terpri) x)
(defun identity (x) x)

(setq
 = eql
 rem %
 null not
 setcar rplaca
 setcdr rplacd)

(defun > (x y) (< y x))
(defun >= (x y) (not (< x y)))
(defun <= (x y) (not (< y x)))
(defun /= (x y) (not (= x y)))

(defun equal (x y)
  (cond ((atom x) (eql x y))
        ((atom y) nil)
        ((equal (car x) (car y)) (equal (cdr x) (cdr y)))))

(defmacro if (test then &rest else)
  \`(cond (,test ,then)
         ,@(cond (else \`((t ,@else))))))

(defmacro when (test &rest body)
  \`(cond (,test ,@body)))

(defmacro let (args &rest body)
  ((lambda (vars vals)
     (defun vars (x)
       (cond (x (cons (if (atom (car x))
                          (car x)
                        (caar x))
                      (vars (cdr x))))))
     (defun vals (x)
       (cond (x (cons (if (atom (car x))
                          nil
                        (cadar x))
                      (vals (cdr x))))))
     \`((lambda ,(vars args) ,@body) ,@(vals args)))
   nil nil))

(defmacro letrec (args &rest body)      ; (letrec ((v e) ...) body...)
  (let (vars setqs)
    (defun vars (x)
      (cond (x (cons (caar x)
                     (vars (cdr x))))))
    (defun sets (x)
      (cond (x (cons \`(setq ,(caar x) ,(cadar x))
                     (sets (cdr x))))))
    \`(let ,(vars args) ,@(sets args) ,@body)))

(defun _append (x y)
  (if (null x)
      y
    (cons (car x) (_append (cdr x) y))))
(defmacro append (x &rest y)
  (if (null y)
      x
    \`(_append ,x (append ,@y))))

(defmacro and (x &rest y)
  (if (null y)
      x
    \`(cond (,x (and ,@y)))))

(defun mapcar (f x)
  (and x (cons (f (car x)) (mapcar f (cdr x)))))

(defmacro or (x &rest y)
  (if (null y)
      x
    \`(cond (,x)
           ((or ,@y)))))

(defun listp (x)
  (or (null x) (consp x)))    ; NB (listp (lambda (x) (+ x 1))) => nil

(defun memq (key x)
  (cond ((null x) nil)
        ((eq key (car x)) x)
        (t (memq key (cdr x)))))

(defun member (key x)
  (cond ((null x) nil)
        ((equal key (car x)) x)
        (t (member key (cdr x)))))

(defun assq (key alist)
  (cond (alist (let ((e (car alist)))
                 (if (and (consp e) (eq key (car e)))
                     e
                   (assq key (cdr alist)))))))

(defun assoc (key alist)
  (cond (alist (let ((e (car alist)))
                 (if (and (consp e) (equal key (car e)))
                     e
                   (assoc key (cdr alist)))))))

(defun _nreverse (x prev)
  (let ((next (cdr x)))
    (setcdr x prev)
    (if (null next)
        x
      (_nreverse next x))))
(defun nreverse (list)        ; (nreverse (quote (a b c d))) => (d c b a))
  (cond (list (_nreverse list nil))))

(defun last (list)
  (if (atom (cdr list))
      list
    (last (cdr list))))

(defun nconc (&rest lists)
  (if (null (cdr lists))
      (car lists)
    (if (null (car lists))
        (apply nconc (cdr lists))
      (setcdr (last (car lists))
              (apply nconc (cdr lists)))
      (car lists))))

(defmacro while (test &rest body)
  (let ((loop (gensym)))
    \`(letrec ((,loop (lambda () (cond (,test ,@body (,loop))))))
       (,loop))))

(defmacro dolist (spec &rest body) ; (dolist (name list [result]) body...)
  (let ((name (car spec))
        (list (gensym)))
    \`(let (,name
           (,list ,(cadr spec)))
       (while ,list
         (setq ,name (car ,list))
         ,@body
         (setq ,list (cdr ,list)))
       ,@(if (cddr spec)
             \`((setq ,name nil)
               ,(caddr spec))))))

(defmacro dotimes (spec &rest body) ; (dotimes (name count [result]) body...)
  (let ((name (car spec))
        (count (gensym)))
    \`(let ((,name 0)
           (,count ,(cadr spec)))
       (while (< ,name ,count)
         ,@body
         (setq ,name (+ ,name 1)))
       ,@(if (cddr spec)
             \`(,(caddr spec))))))
`;
if (typeof process !== "undefined" && typeof require !== "undefined") {
    write = (s) => process.stdout.write(s);
    exit = process.exit;
    let interp = new Interp();
    run(interp, prelude);
    async function main() {
        try {
            let stdin = undefined;
            let fs = undefined;
            // Run interactively in UTF-8 for no arguments or the "-" argument.
            // Run each as a script file in UTF-8 in order for other arguments.
            let argv = process.argv;
            if (argv.length <= 2)
                argv = [undefined, undefined, "-"];
            for (let i = 2; i < argv.length; i++) {
                let fileName = argv[i];
                if (fileName == "-") {
                    if (stdin === undefined) {
                        let repl = new REPL();
                        stdin = process.stdin;
                        stdin.setEncoding("utf8");
                        stdin.on("data", (line) => repl.stdInOnData(line));
                        stdin.on("end", () => repl.stdInOnEnd());
                        await repl.readEvalPrintLoop(interp);
                    }
                }
                else {
                    fs = fs || require("fs");
                    let text = fs.readFileSync(fileName, "utf8");
                    run(interp, text);
                }
            }
        }
        catch (ex) {
            console.log(ex);
            process.exit(1);
        }
    }
    main();
}
