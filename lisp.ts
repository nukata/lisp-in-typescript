/*
  Nukata Lisp 2.1.0 in TypeScript 4.6 by SUZUKI Hisao (H28.02.08/R04.03.28)
    $ deno bundle lisp.ts lisp.js && deno run lisp.js
  | $ deno bundle lisp.ts lisp.js && node lisp.js
  | $ deno run lisp.ts
*/

import {
    Numeric, ZERO, ONE, isNumeric,
    add, subtract, multiply, divide, quotient, remainder, compare,
    tryToParse, convertToString
} from "./arith.ts";

// An inefficient substitution of assert statement in Dart
function assert(x: boolean, message?: string): asserts x {
    if (! x)
        throw new Error("Assertion Failure: " + (message || ""));
}

// Read a line as a string, or null when EOF is read.
let readLine: () => Promise<string | null>;

let write: (s: string) => void; // Output string s (a new line on \n char).
let exit: (n: number) => void;  // Terminate the process with exit code n.

//----------------------------------------------------------------------

// Lisp cons cell
class Cell {
    constructor(public car: unknown,
                public cdr: unknown) {}

    toString(): string { return "(" + this.car + " . " + this.cdr + ")" }

    // Length as a list
    get length(): number { return foldl(0, this, (i, _) => i + 1) }
}

// Lisp's list
type List = Cell | null;

// foldl(x, (a b c), fn) => fn(fn(fn(x, a), b), c)
function foldl<T>(x: T, j: List, fn: (x: T, y: unknown) => T): T {
    while (j !== null) {
        x = fn(x, j.car);
        j = j.cdr as List;
    }
    return x;
}

// mapcar((a b c), fn) => (fn(a) fn(b) fn(c))
function mapcar(j: List, fn: (x: unknown) => unknown): List {
    if (j === null)
        return null;
    const a = fn(j.car);
    let d = j.cdr;
    if (d instanceof Cell)
        d = mapcar(d, fn);
    if (Object.is(j.car, a) && Object.is(j.cdr, d))
        return j;
    return new Cell(a, d);
}


// Lisp symbol
class Sym {
    // Construct an uninterned symbol.
    constructor(public readonly name: string) {}

    toString(): string { return this.name }

    // Is it interned?
    get isInterned(): boolean { return symTable[this.name] === this; }
}

// Expression keyword
class Keyword extends Sym {
    constructor(name: string) {
        super(name);
    }
}

// The table of interned symbols
const symTable: {[key: string]: Sym} = {};

// Construct an interned symbol; construct a Keyword if isKeyword holds.
function newSym(name: string, isKeyword = false): Sym {
    let result = symTable[name];
    assert((result === undefined || ! isKeyword), name);
    if (result === undefined) {
        result = isKeyword ? new Keyword(name) : new Sym(name);
        symTable[name] = result;
    }
    return result;
}

function newKeyword(name: string): Keyword { return newSym(name, true) }

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
const restSym = newSym ("&rest");
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
function cdrCell(x: Cell): List {
    const k = x.cdr;
    if (k instanceof Cell)
        return k;
    else if (k === null)
        return null;
    else
        throw new EvalException("proper list expected", x);
}


// Common base class of Lisp functions
abstract class Func {
    // carity is a number of arguments, made negative if the func has &rest.
    constructor(public readonly carity: number) {}

    get arity(): number {
        return (this.carity < 0) ? -this.carity : this.carity;
    }

    get hasRest(): boolean {
        return (this.carity < 0);
    }

    get fixedArgs(): number {   // Number of fixed arguments
        return (this.carity < 0) ? -this.carity - 1 : this.carity;
    }

    // Make a call-frame from a list of actual arguments.
    makeFrame(arg: List): unknown[] {
        const frame = new Array(this.arity);
        const n = this.fixedArgs;
        let i = 0;
        for (; i < n && arg !== null; i++) { // Set the list of fiexed args.
            frame[i] = arg.car;
            arg = cdrCell(arg);
        }
        if (i !== n || (arg !== null && ! this.hasRest))
            throw new EvalException("arity not matched", this);
        if (this.hasRest)
            frame[n] = arg;
        return frame;
    }

    // Evaluate each expression of a frame.
    evalFrame(frame: unknown[], interp: Interp, env: List): void {
        const n = this.fixedArgs;
        for (let i = 0; i < n; i++)
            frame[i] = interp.eval(frame[i], env);
        if (this.hasRest && frame[n] instanceof Cell) {
            let z: List = null;
            let y: List = null;
            for (let j = frame[n] as List; j !== null; j = cdrCell(j)) {
                const e = interp.eval(j.car, env);
                const x = new Cell(e, null);
                if (z === null) {
                    z = x;
                } else {
                    assert(y !== null);
                    y.cdr = x;
                }
                y = x;
            }
            frame[n] = z;
        }
    }
}


// Common base class of functions which are defined with Lisp expressions
abstract class DefinedFunc extends Func {
    // body is a Lisp list as the function body.
    constructor(carity: number,
                public readonly body: List) {
        super(carity);
    }
}

// Common function type which represents any factory methods of DefinedFunc
type FuncFactory = (carity: number, body: List, env: List) => DefinedFunc;


// Compiled macro expression
class Macro extends DefinedFunc {
    constructor(carity: number, body: List) {
        super(carity, body);
    }

    toString(): string { return `#<macro:${this.carity}:${str(this.body)}>` }

    // Expand the macro with a list of actual arguments.
    expandWith(interp: Interp, arg: List): unknown {
        const frame = this.makeFrame(arg);
        const env = new Cell(frame, null);
        let x: unknown = null;
        for (let j = this.body; j !== null; j = cdrCell(j))
            x = interp.eval(j.car, env);
        return x;
    }

    static make(carity: number, body: List, env: List): DefinedFunc {
        assert(env === null);
        return new Macro(carity, body);
    }
}


// Compiled lambda expression (within another function)
class Lambda extends DefinedFunc {
    constructor(carity: number, body: List) {
        super(carity, body);
    }

    toString(): string { return `#<lambda:${this.carity}:${str(this.body)}>` }

    static make(carity: number, body: List, env: List): DefinedFunc {
        assert(env === null);
        return new Lambda(carity, body);
    }
}


// Compiled lambda expression (Closure with environment)
class Closure extends DefinedFunc {
    // env is the environment of the closure.
    constructor(carity: number, body: List, private readonly env: List) {
        super(carity, body);
    }

    static makeFrom(x: Lambda, env: List) {
        return new Closure(x.carity, x.body, env);
    }
    
    toString(): string {
        return `#<closure:${this.carity}:${str(this.env)}:${str(this.body)}>`;
    }

    // Make a new environment from a list of actual arguments.
    makeEnv(interp: Interp, arg: List, interpEnv: List): Cell {
        const frame = this.makeFrame(arg);
        this.evalFrame(frame, interp, interpEnv);
        return new Cell(frame, this.env); // Prepend the frame to the env.
    }

    static make(carity: number, body: List, env: List): DefinedFunc {
        return new Closure(carity, body, env);
    }
}


// Function type which represents any built-in function bodies
type BuiltInFuncBody = (frame: unknown[]) => unknown;

// Built-in function
class BuiltInFunc extends Func {
    // name is the function name; body is the function body.
    constructor(private readonly name: string,
                carity: number,
                private readonly body: BuiltInFuncBody) {
        super(carity);
    }

    toString(): string { return "#<" + this.name + ":" + this.carity + ">" }

    // Invoke the built-in function with a list of actual arguments.
    evalWith(interp: Interp, arg: List, interpEnv: List): unknown {
        const frame = this.makeFrame(arg);
        this.evalFrame(frame, interp, interpEnv);
        try {
            return this.body(frame);
        } catch (ex) {
            if (ex instanceof EvalException)
                throw ex;
            else
                throw new EvalException(ex + " -- " + this.name, frame);
        }
    }
}


// Bound variable in a compiled lambda/macro expression
class Arg {
    constructor(public readonly level: number,
                public readonly offset: number,
                public readonly symbol: Sym) {}

    toString(): string {
        return "#" + this.level + ":" + this.offset + ":" + this.symbol;
    }

    // Set a value x to the location corresponding to the variable in env.
    setValue(x: unknown, env: Cell): void {
        for (let i = 0; i < this.level; i++)
            env = env.cdr as Cell;
        (env.car as unknown[])[this.offset] = x;
    }

    // Get a value from the location corresponding to the variable in env.
    getValue(env: Cell): unknown {
        for (let i = 0; i < this.level; i++)
            env = env.cdr as Cell;
        return (env.car as unknown[])[this.offset];
    }
}


// Exception in evaluation
class EvalException extends Error {
    readonly trace: string[] = [];

    constructor(msg: string, x: unknown, quoteString=true) {
        super(msg + ": " + str(x, quoteString));
    }

    toString(): string {
        let s = "EvalException: " + this.message;
        for (const line of this.trace)
            s += "\n\t" + line;
        return s;
    }
}

// Exception which indicates an absence of a variable
class NotVariableException extends EvalException {
    constructor(x: unknown) {
        super("variable expected", x);
    }
}


// Exception thrown when something does not have an expected format
class FormatException extends Error {
    constructor(msg: string) {
        super(msg);
    }
}


// Singleton for end-of-file
const EndOfFile = { toString: () => "EOF" };

//----------------------------------------------------------------------

// Core of the interpreter
class Interp {
    // Table of the global values of symbols
    private readonly globals: Map<Sym, unknown> = new Map();

    constructor() {
        this.def("car", 1, (a: unknown[]) =>
                 (a[0] === null) ? null : (<Cell>a[0]).car);
        this.def("cdr", 1, (a: unknown[]) =>
                 (a[0] === null) ? null : (<Cell>a[0]).cdr);
        this.def("cons", 2, (a: unknown[]) =>
                 new Cell(a[0], a[1]));
        this.def("atom", 1, (a: unknown[]) =>
                 (a[0] instanceof Cell) ? null : true);
        this.def("eq", 2, (a: unknown[]) =>
                 Object.is(a[0], a[1]) ? true : null);

        this.def("list", -1, (a: unknown[]) => a[0]);
        this.def("rplaca", 2, (a: unknown[]) =>
                 { (<Cell> a[0]).car = a[1]; return a[1] });
        this.def("rplacd", 2, (a: unknown[]) =>
                 { (<Cell> a[0]).cdr = a[1]; return a[1] });
        this.def("length", 1, (a: unknown[]) =>
                 (a[0] === null ? 0 :
                  quotient((a[0] as Cell | string).length, 1)));
        this.def("stringp", 1, (a: unknown[]) =>
                 (typeof a[0] === "string") ? true : null);
        this.def("numberp", 1, (a: unknown[]) =>
                 isNumeric(a[0]) ? true : null);

        this.def("eql", 2, (a: unknown[]) => {
            const x = a[0];
            const y = a[1];
            return (x === y) ? true :
                (isNumeric(x) && isNumeric(y) && compare(x, y) === 0) ? true :
                null;
        });

        this.def("<", 2, (a: unknown[]) =>
                 (compare(a[0] as Numeric, a[1] as Numeric) < 0) ? true :
                 null);

        this.def("%", 2, (a: unknown[]) =>
                 remainder(a[0] as Numeric, a[1] as Numeric));

        this.def("mod", 2, (a: unknown[]) => {
            const x = a[0] as Numeric;
            const y = a[1] as Numeric;
            const q = remainder(x, y);
            return (compare(multiply(x, y), ZERO) < 0) ? add(q, y) : q;
        });

        this.def("+", -1, (a: unknown[]) =>
                 foldl(ZERO, a[0] as List,
                       (i, j) => add(i as Numeric, j as Numeric)));

        this.def("*", -1, (a: unknown[]) =>
                 foldl(ONE, a[0] as List,
                       (i, j) => multiply(i as Numeric, j as Numeric)));

        this.def("-", -2, (a: unknown[]) => {
            const x = a[0] as Numeric;
            const y = a[1] as List;
            return (y == null) ? -x :
                foldl(x, y, (i, j) => subtract(i as Numeric, j as Numeric));
        })

        this.def("/", -3, (a: unknown[]) => 
                 foldl(divide(a[0] as Numeric, a[1] as Numeric),
                       a[2] as List,
                       (i, j) => divide(i as Numeric, j as Numeric)));

        this.def("truncate", -2, (a: unknown[]) => {
            const x = a[0] as Numeric;
            const y = a[1] as List;
            if (y === null) {
                return quotient(x, ONE);
            } else if (y.cdr === null) {
                return quotient(x, y.car as Numeric);
            } else {
                throw "one or two arguments expected";
            }
        });

        this.def("prin1", 1, (a: unknown[]) => {
            write(str(a[0], true));
            return a[0];
        });
        this.def("princ", 1, (a: unknown[]) => {
            write(str(a[0], false));
            return a[0];
        });
        this.def("terpri", 0, (_a: unknown[]) => {
            write("\n");
            return true;
        });

        const gensymCounter = newSym("*gensym-counter*");
        this.globals.set(gensymCounter, ONE);
        this.def("gensym", 0, (_a: unknown[]) => {
            const i = this.globals.get(gensymCounter) as Numeric;
            this.globals.set(gensymCounter, add(i, ONE));
            return new Sym("G" + i); // an uninterned symbol
        });

        this.def("make-symbol", 1, (a: unknown[]) => new Sym(a[0] as string));
        this.def("intern", 1, (a: unknown[]) => newSym(a[0] as string));
        this.def("symbol-name", 1, (a: unknown[]) => (<Sym> a[0]).name);

        this.def("apply", 2, (a: unknown[]) =>
                 this.eval(new Cell(a[0],
                                    mapcar(a[1] as List, qqQuote)),
                           null));

        this.def("exit", 1, (a: unknown[]) => exit(Number(a[0])));
        this.def("dump", 0, (_a: unknown[]) => {
            let s: List = null;
            for (const x of this.globals.keys())
                s = new Cell(x, s);
            return s;
        });

        // named after Tōkai-dō Mikawa-koku Nukata-gun (東海道 三河国 額田郡)
        this.globals.set(newSym("*version*"),
                         new Cell(2.10,
                                  new Cell("TypeScript",
                                           new Cell("Nukata Lisp", null))));
    }

    // Define a built-in function by giving a name, a carity, and a body.
    def(name: string, carity: number, body: BuiltInFuncBody) {
        const sym = newSym(name);
        this.globals.set(sym, new BuiltInFunc(name, carity, body));
    }

    // Evaluate a Lisp expression in an environment.
    eval(x: unknown, env: List): unknown {
        try {
            for (;;) {
                if (x instanceof Arg) {
                    assert(env !== null);
                    return x.getValue(env);
                } else if (x instanceof Sym) {
                    const value = this.globals.get(x);
                    if (value === undefined)
                        throw new EvalException("void variable", x);
                    return value;
                } else if (x instanceof Cell) {
                    let fn = x.car;
                    const arg = cdrCell(x);
                    if (fn instanceof Keyword) {
                        switch (<Keyword> fn) {
                        case quoteSym:
                            if (arg !== null && arg.cdr === null)
                                return arg.car;
                            throw new EvalException("bad quote", x);
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
                                throw new EvalException("nested macro", x);
                            return this.compile(arg, null, Macro.make);
                        case quasiquoteSym:
                            if (arg !== null && arg.cdr === null) {
                                x = qqExpand(arg.car);
                                break;
                            }
                            throw new EvalException("bad quasiquote", x);
                        default:
                            throw new EvalException("bad keyword", fn);
                        }
                    } else {    // Application of a function
                        // Expand fn = eval(fn, env) here on Sym for speed.
                        if (fn instanceof Sym) {
                            fn = this.globals.get(fn);
                            if (fn === undefined)
                                throw new EvalException("undefined", x.car);
                        } else {
                            fn = this.eval(fn, env);
                        }

                        if (fn instanceof Closure) {
                            env = fn.makeEnv(this, arg, env);
                            x = this.evalProgN(fn.body, env);
                        } else if (fn instanceof Macro) {
                            x = fn.expandWith(this, arg);
                        } else if (fn instanceof BuiltInFunc) {
                            return fn.evalWith(this, arg, env);
                        } else {
                            throw new EvalException("not applicable", fn);
                        }
                    }
                } else if (x instanceof Lambda) {
                    return Closure.makeFrom(x, env);
                } else {
                    return x;   // numbers, strings, null etc.
                }
            }
        } catch (ex) {
            if (ex instanceof EvalException) {
                if (ex.trace.length < 10)
                    ex.trace.push(str(x));
            }
            throw ex;
        }
    }

    // (progn E1 E2 .. En) => Evaluate E1, E2, .. except for En and return it.
    private evalProgN(j: List, env: List): unknown {
        if (j === null)
            return null;
        for (;;) {
            const x = j.car;
            j = cdrCell(j);
            if (j === null)
                return x; // The tail exp will be evaluated at the caller.
            this.eval(x, env);
        }
    }

    // Evaluate a conditional expression and return the selection unevaluated.
    private evalCond(j: List, env: List): unknown {
        for (; j !== null; j = cdrCell(j)) {
            const clause = j.car;
            if (clause instanceof Cell) {
                const result = this.eval(clause.car, env);
                if (result !== null) { // If the condition holds
                    const body = cdrCell(clause);
                    if (body === null)
                        return qqQuote(result);
                    else
                        return this.evalProgN(body, env);
                }
            } else if (clause !== null) {
                throw new EvalException("cond test expected", clause);
            }
        }
        return null;            // No clause holds.
    }

    // (setq V1 E1 ..) => Evaluate Ei and assign it to Vi; return the last.
    private evalSetQ(j: List, env: List): unknown {
        let result: unknown = null;
        for (; j !== null; j = cdrCell(j)) {
            const lval = j.car;
            j = cdrCell(j);
            if (j === null)
                throw new EvalException("right value expected", lval);
            result = this.eval(j.car, env);
            if (lval instanceof Arg) {
                assert(env !== null);
                lval.setValue(result, env);
            } else if (lval instanceof Sym && !(lval instanceof Keyword)) {
                this.globals.set(lval, result);
            } else {
                throw new NotVariableException(lval);
            }
        }
        return result;
    }

    // Compile a Lisp list (macro ..) or (lambda ..).
    private compile(arg: List, env: List, make: FuncFactory): DefinedFunc {
        if (arg === null)
            throw new EvalException("arglist and body expected", arg);
        const table = new Map<Sym, Arg>();
        const [hasRest, arity] = makeArgTable(arg.car, table);
        let body = cdrCell(arg);
        body = scanForArgs(body, table) as List;
        // Expand macros up to 20 nestings
        body = this.expandMacros(body, 20) as List;
        body = this.compileInners(body) as List;
        return make((hasRest) ? -arity : arity, body, env);
    }

    // Expand macros and quasi-quotations in an expression.
    private expandMacros(j: unknown, count: number): unknown {
        if (count > 0 && j instanceof Cell) {
            let k = j.car;
            switch (k) {
            case quoteSym:
            case lambdaSym:
            case macroSym:
                return j;
            case quasiquoteSym: {
                const d = cdrCell(j);
                if (d !== null && d.cdr === null) {
                    const z = qqExpand(d.car);
                    return this.expandMacros(z, count);
                }
                throw new EvalException("bad quasiquote", j);
            }
            default:
                if (k instanceof Sym)
                    k = this.globals.get(k);
                if (k instanceof Macro) {
                    const d = cdrCell(j);
                    const z = k.expandWith(this, d);
                    return this.expandMacros(z, count - 1);
                }
                return mapcar(j, (x) => this.expandMacros(x, count));
            }
        } else {
            return j;
        }
    }

    // Replace inner lambda expressions with Lambda instances.
    private compileInners(j: unknown): unknown {
        if (j instanceof Cell) {
            const k = j.car;
            switch (k) {
            case quoteSym:
                return j;
            case lambdaSym: {
                const d = cdrCell(j);
                return this.compile(d, null, Lambda.make);
            }
            case macroSym:
                throw new EvalException("nested macro", j);
            default:
                return mapcar(j, (x) => this.compileInners(x));
            }
        } else {
            return j;
        }
    }
}

//----------------------------------------------------------------------

// Make an argument table; return a pair of rest-yes/no and the arity.
function makeArgTable(arg: unknown, table: Map<Sym, Arg>): [boolean, number]
{
    if (arg === null) {
        return [false, 0];
    } else if (arg instanceof Cell) {
        let ag = arg as List;
        let offset = 0;         // offset value within the call-frame
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
            let sym: Sym;
            if (j instanceof Sym)
                sym = j;
            else if (j instanceof Arg)
                sym = j.symbol;
            else
                throw new NotVariableException(j);
            if (table.has(sym))
                throw new EvalException("duplicated argument name", j);
            table.set(sym, new Arg(0, offset, sym));
            offset++;
        }
        return [hasRest, offset];
    } else {
        throw new EvalException("arglist expected", arg);
    }
}

// Scan 'j' for formal arguments in 'table' and replace them with Args.
// And scan 'j' for free Args not in 'table' and promote their levels.
function scanForArgs(j: unknown, table: Map<Sym, Arg>): unknown {
    if (j instanceof Sym) {
        const k = table.get(j);
        return (k === undefined) ? j : k;
    } else if (j instanceof Arg) {
        const k = table.get(j.symbol);
        return (k === undefined) ?
            new Arg(j.level + 1, j.offset, j.symbol) : k;
    } else if (j instanceof Cell) {
        if (j.car === quoteSym) {
            return j;
        } else if (j.car === quasiquoteSym) {
            return new Cell(quasiquoteSym, scanForQQ(j.cdr, table, 0));
        } else {
            return mapcar(j, (x) => scanForArgs(x, table));
        }
    } else {
        return j;
    }
}

// Scan for quasi-quotes and scanForArgs them depending on the nesting level.
function scanForQQ(j: unknown, table: Map<Sym, Arg>, level: number): unknown {
    if (j instanceof Cell) {
        const k = j.car;
        if (k === quasiquoteSym) {
            return new Cell(k, scanForQQ(j.cdr, table, level + 1));
        } else if (k === unquoteSym || k === unquoteSplicingSym) {
            const d = (level === 0) ?
                scanForArgs(j.cdr, table) :
                scanForQQ(j.cdr, table, level - 1);
            if (Object.is(d, j.cdr))
                return j;
            return new Cell(k, d);
        } else {
            return mapcar(j, (x) => scanForQQ(x, table, level));
        }
    } else {
        return j;
    }
}

//----------------------------------------------------------------------
// Quasi-Quotation

// Expand x of any quasi-quotation `x into the equivalent S-expression.
function qqExpand(x: unknown): unknown {
    return qqExpand0(x, 0);     // Begin with the nesting level 0.
}

function qqExpand0(x: unknown, level: number): unknown {
    if (x instanceof Cell) {
        if (x.car === unquoteSym) { // ,a
            if (level === 0)
                return (x.cdr as Cell).car; // ,a => a
        }
        const t = qqExpand1(x, level);
        if (t.car instanceof Cell && t.cdr === null) {
            const k = t.car;
            if (k.car == listSym || k.car === consSym)
                return k;
        }
        return new Cell(appendSym, t);
    } else {
        return qqQuote(x);
    }
}

// Quote x so that the result evaluates to x.
function qqQuote(x: unknown): unknown {
    if (x instanceof Sym || x instanceof Cell)
        return new Cell(quoteSym, new Cell(x, null));
    return x;
}

// Expand x of `x so that the result can be used as an argument of append.
// Example 1: (,a b) => ((list a 'b))
// Example 2: (,a ,@(cons 2 3)) => ((cons a (cons 2 3)))
function qqExpand1(x: unknown, level: number): Cell {
    if (x instanceof Cell) {
        if (x.car === unquoteSym) { // ,a
            if (level === 0)
                return x.cdr as Cell // ,a => (a)
            level--;
        } else if (x.car === quasiquoteSym) { // `a
            level++;
        }
        const h = qqExpand2(x.car, level);
        const t = qqExpand1(x.cdr, level); // !== null
        if (t.car === null && t.cdr === null) {
            return new Cell(h, null);
        } else if (h instanceof Cell) {
            if (h.car === listSym) {
                const tcar = t.car;
                if (tcar instanceof Cell) {
                    if (tcar.car === listSym) {
                        const hh = qqConcat(h, tcar.cdr);
                        return new Cell(hh, t.cdr);
                    }
                }
                if (h.cdr instanceof Cell) {
                    const hh = qqConsCons(h.cdr, tcar);
                    return new Cell(hh, t.cdr);
                }
            }
        }
        return new Cell(h, t);
    } else {
        return new Cell(qqQuote(x), null);
    }
}

// (1 2), (3 4) => (1 2 3 4)
function qqConcat(x: Cell, y: unknown): unknown {
    if (x === null)
        return y;
    return new Cell(x.car, qqConcat(x.cdr as Cell, y));
}

// (1 2 3), "a" => (cons 1 (cons 2 (cons 3 "a")))
function qqConsCons(x: Cell, y: unknown): unknown {
    if (x === null)
        return y;
    return new Cell(consSym,
                    new Cell(x.car,
                             new Cell(qqConsCons(x.cdr as Cell, y),
                                      null)))
}

// Expand x.car (=y) of `x so that the result can be used as an arg of append.
// Example: ,a => (list a); ,@(foo 1 2) => (foo 1 2); b => (list 'b)
function qqExpand2(y: unknown, level: number): unknown {
    if (y instanceof Cell) {
        switch (y.car) {
        case unquoteSym:        // ,a
            if (level === 0)
                return new Cell(listSym, y.cdr); // ,a => (list a)
            level--;
            break;
        case unquoteSplicingSym: // ,@a
            if (level === 0)
                return (y.cdr as Cell).car; // ,@a => a
            level--;
            break;
        case quasiquoteSym:     // `a
            level++;
            break;
        }
    }
    return new Cell(listSym, new Cell(qqExpand0(y, level), null));
}

//----------------------------------------------------------------------

// A list of tokens, which works as a reader of Lisp expressions
class Reader {
    private token: unknown;
    private tokens: string[] = [];
    private lineNo = 1;

    // Split a text into a list of tokens and append it to this.tokens.
    // For "(a \n 1)" it appends ["(", "a", "\n", "1", ")", "\n"] to tokens.
    push(text: string): void {
        const tokenPat = /\s+|;.*$|("(\\.?|.)*?"|,@?|[^()'`~"; \t]+|.)/g;
        for (const line of text.split("\n")) {
            for (;;) {
                const result = tokenPat.exec(line);
                if (result === null)
                    break;
                const s = result[1];
                if (s !== undefined)
                    this.tokens.push(s)
            }
            this.tokens.push("\n");
        }
    }

    // Make this be a clone of the other.
    copyFrom(other: Reader): void {
        this.tokens = other.tokens.slice();
        this.lineNo = other.lineNo;
    }

    // Make this have no tokens.
    clear(): void {
        this.tokens.length = 0;
    }

    // Does this have no tokens?
    isEmpty(): boolean {
        return this.tokens.every((t: string) => t === "\n");
    }

    // Read a Lisp expression; throw EndOfFile if this.tokens run out.
    read(): unknown {
        try {
            this.readToken();
            return this.parseExpression();
        } catch (ex) {
            if (ex === EndOfFile)
                throw EndOfFile;
            else if (ex instanceof FormatException)
                throw new EvalException(
                    "syntax error", ex.message + " at " + this.lineNo, false);
            else
                throw ex;
        }
    }

    private parseExpression(): unknown {
        switch (this.token) {
        case leftParenSym:      // (a b c)
            this.readToken();
            return this.parseListBody();
        case singleQuoteSym:    // 'a => (quote a)
            this.readToken();
            return new Cell(quoteSym,
                            new Cell(this.parseExpression(), null));
        case backQuoteSym:      // `a => (quasiquote a)
            this.readToken();
            return new Cell(quasiquoteSym,
                            new Cell(this.parseExpression(), null));
        case commaSym:          // ,a => (unquote a)
            this.readToken();
            return new Cell(unquoteSym,
                            new Cell(this.parseExpression(), null));
        case commaAtSym:        // ,@a => (unquote-splicing a)
            this.readToken();
            return new Cell(unquoteSplicingSym,
                            new Cell(this.parseExpression(), null));
        case dotSym:
        case rightParenSym:
            throw new FormatException('unexpected "' + this.token + '"');
        default:
            return this.token;
        }
    }

    private parseListBody(): unknown {
        if (this.token === rightParenSym) {
            return null;
        } else {
            const e1 = this.parseExpression();
            this.readToken();
            let e2: unknown;
            if (this.token == dotSym) { // (a . b)
                this.readToken();
                e2 = this.parseExpression();
                this.readToken();
                if (this.token !== rightParenSym)
                    throw new FormatException('")" expected: ' + this.token);
            } else {
                e2 = this.parseListBody();
            }
            return new Cell(e1, e2);
        }
    }

    // Read the next token and set it to this.token.
    private readToken(): void {
        for (;;) {
            const t = this.tokens.shift();
            if (t === undefined) {
                throw EndOfFile;
            } else if (t === "\n") {
                this.lineNo += 1;
            } else if (t === "+" || t === "-") {
                // N.B. BigInt("+") and BigInt("-") return 0n in Safari.
                this.token = newSym(t);
                return;
            } else {
                if (t[0] === '"') {
                    let s = t;
                    const n = s.length - 1;
                    if (n < 1 || s[n] !== '"')
                        throw new FormatException("bad string: " + s);
                    s = s.substring(1, n);
                    s = s.replace(/\\./g, (m: string) => {
                        const val = Reader.escapes[m];
                        return (val === undefined) ? m : val;
                    });
                    this.token = s;
                    return;
                }
                const n = tryToParse(t);
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

    private static escapes: {[key: string]: string} = {
        "\\\\": "\\",
        '\\"': '"',
        "\\n": "\n", "\\r": "\r", "\\f": "\f",
        "\\b": "\b", "\\t": "\t", "\\v": "\v"
    };
}

//----------------------------------------------------------------------

// Mapping from a quote symbol to its string representation
const quotes: {[key: string]: string} = {
    [quoteSym.name]: "'",
    [quasiquoteSym.name]: "`",
    [unquoteSym.name]: ",",
    [unquoteSplicingSym.name]: ",@"
};

// Make a string representation of Lisp expression
function str(x: unknown, quoteString = true,
             count?: number, printed?: Cell[]) : string
{
    if (x === null) {
        return "nil";
    } else if (x === true) {
        return "t";
    } else if (x instanceof Cell) {
        if (x.car instanceof Sym) {
            const q = quotes[x.car.name];
            if (q !== undefined && x.cdr instanceof Cell)
                if (x.cdr.cdr == null)
                    return q + str(x.cdr.car, true, count, printed);
        }
        return "(" + strListBody(x, count, printed) + ")";
    } else if (typeof x === "string") {
        if (! quoteString)
            return x;
        const bf: string[] = ['"'];
        for (const ch of x) {
            switch (ch) {
            case "\b": bf.push("\\b"); break;
            case "\t": bf.push("\\t"); break;
            case "\n": bf.push("\\n"); break;
            case "\v": bf.push("\\v"); break;
            case "\f": bf.push("\\f"); break;
            case "\r": bf.push("\\r"); break;
            case "\"": bf.push("\\\""); break;
            case "\\": bf.push("\\\\"); break;
            default:
                bf.push(ch); break;
            }
        }
        bf.push('"');
        return bf.join("");
    } else if (x instanceof Array) {
        const s = x.map((e) => str(e, true, count, printed)).join(", ");
        return "[" + s + "]";
    } else if (x instanceof Sym) {
        return (x.isInterned) ? x.name : "#:" + x;
    } else if (isNumeric(x)) {
        return convertToString(x);
    } else {
        return x + "";
    }
}

// Make a string representation of list, omitting its "(" and ")".
function strListBody(x: Cell, count?: number, printed?: Cell[]): string {
    if (printed === undefined)
        printed = [];
    if (count === undefined)
        count = 4;         // threshold of ellipsis for circular lists
    const s: string[] = [];
    let y: unknown;
    for (y = x; y instanceof Cell; y = y.cdr) {
        if (printed.indexOf(y) < 0) {
            printed.push(y);
            count = 4;
        } else {
            count--;
            if (count < 0) {
                s.push("...");  // an ellipsis for a circular list
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
        const i = printed.indexOf(y);
        if (i >= 0)
            printed.splice(i, 1)
    }
    return s.join(" ");
}

//----------------------------------------------------------------------

// Read-Eval-Print Loop
class REPL {
    private readonly stdInTokens: Reader = new Reader();

    // Read an expression from the standard-in asynchronously.
    private async readExpression(prompt1: string, prompt2: string):
    Promise<unknown> {
        const oldTokens = new Reader();
        for (;;) {
            oldTokens.copyFrom(this.stdInTokens);
            try {
                return this.stdInTokens.read();
            } catch (ex) {
                if (ex === EndOfFile) {
                    write(oldTokens.isEmpty() ? prompt1 : prompt2);
                    const line = await readLine();
                    if (line === null)
                        return EndOfFile;
                    oldTokens.push(line);
                    this.stdInTokens.copyFrom(oldTokens);
                } else {
                    this.stdInTokens.clear(); // Discard the erroneous tokens.
                    throw ex;
                }
            }
        }
    }

    // Repeat Read-Eval-Print until End-Of-File asynchronously.
    async readEvalPrintLoop(interp: Interp): Promise<void> {
        for (;;) {
            try {
                const exp = await this.readExpression("> ", "  ");
                if (exp === EndOfFile) {
                    write("Goodbye\n");
                    return;
                }
                const result = interp.eval(exp, null);
                write(str(result) + "\n");
            } catch (ex) {
                if (ex instanceof EvalException)
                    write(ex + "\n");
                else
                    throw ex;
            }
        }
    }
}

// Evaluate a string as a list of Lisp exps; return the result of the last exp.
function run(interp: Interp, text: string): unknown {
    const tokens = new Reader();
    tokens.push(text);
    let result = undefined;
    while (! tokens.isEmpty()) {
        const exp = tokens.read();
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

//----------------------------------------------------------------------
// Main procedures for Deno and Node.js (not used on browsers)

declare function require(name: string): any; // A hack for Node.js
declare let process: any;                    // A hack for Node.js

if (typeof Deno !== 'undefined') {
    // For Deno
    const decoder = new TextDecoder(); // from utf-8 bytes
    const buf = new Uint8Array(8000);
    readLine = async function(): Promise<string | null> {
        const n = await Deno.stdin.read(buf);
        if (n == null)
            return null;        // EOF
        return decoder.decode(buf.subarray(0, n));
    };

    const encoder = new TextEncoder(); // to utf-8 bytes
    write = (s: string) => {
        const bb = encoder.encode(s);
        for (let n = 0; n < bb.length; )
            n += Deno.stdout.writeSync(bb.subarray(n))
    };

    exit = Deno.exit;

    const main = async function() {
        const interp = new Interp();
        run(interp, prelude);

        let repl = undefined;
        const args = (Deno.args.length == 0) ? ['-'] : Deno.args;
        try {
            // Run the REPL on the first '-' argument.
            // Run each script file for other arguments.
            for (const fileName of args) {
                if (fileName === '-') {
                    if (repl === undefined) {
                        repl = new REPL();
                        await repl.readEvalPrintLoop(interp);
                    }
                } else {
                    const text = Deno.readTextFileSync(fileName);
                    run(interp, text);
                }
            }
        } catch (ex) {
            console.log(ex);
            exit(1);
        }
    };
    main();
} else if (typeof process === 'object') {
    // For Node.js
    let readLine_atFirst = true;
    let readLine_resolve: ((line: string | null) => void) | null = null;
    readLine = () => new Promise<string | null>((resolve, _reject) => {
        if (readLine_atFirst) {
            process.stdin.setEncoding('utf8');
            process.stdin.on('data', (line: string) => {
                if (readLine_resolve !== null) {
                    readLine_resolve(line);
                    readLine_resolve = null;
                }
            });
            process.stdin.on('end', () => {
                if (readLine_resolve !== null) {
                    readLine_resolve(null);
                    readLine_resolve = null;
                }
            });
            readLine_atFirst = false;
        }
        readLine_resolve = resolve;
    });

    write = (s: string) => process.stdout.write(s);
    exit = process.exit;

    const main = async function() {
        const interp = new Interp();
        run(interp, prelude);

        let repl = undefined;
        let fs = undefined;
        let argv = process.argv as string[];
        if (argv.length <= 2)
            argv = ['', '', '-'];
        try {
            for (let i = 2; i < argv.length; i++) {
                const fileName = argv[i];
                if (fileName == '-') {
                    if (repl === undefined) {
                        repl = new REPL();
                        await repl.readEvalPrintLoop(interp);
                    }
                } else {
                    fs = fs || require('fs');
                    const text = fs.readFileSync(fileName, 'utf8');
                    run(interp, text);
                }
            }
        } catch (ex) {
            console.log(ex);
            exit(1);
        }
    };
    main();
}
