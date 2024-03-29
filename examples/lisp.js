// deno-fmt-ignore-file
// deno-lint-ignore-file
// This code was bundled using `deno bundle` and it's not recommended to edit it manually

const ZERO = typeof BigInt === 'undefined' ? 0 : BigInt(0);
const ONE = typeof BigInt === 'undefined' ? 1 : BigInt(1);
function isNumeric(x) {
    const t = typeof x;
    return t === 'number' || t === 'bigint';
}
function add(x, y) {
    if (typeof x === 'number') {
        if (typeof y === 'number') return x + y;
        else return x + Number(y);
    } else {
        if (typeof y === 'number') return Number(x) + y;
        else return x + y;
    }
}
function subtract(x, y) {
    if (typeof x === 'number') {
        if (typeof y === 'number') return x - y;
        else return x - Number(y);
    } else {
        if (typeof y === 'number') return Number(x) - y;
        else return x - y;
    }
}
function multiply(x, y) {
    if (typeof x === 'number') {
        if (typeof y === 'number') return x * y;
        else return x * Number(y);
    } else {
        if (typeof y === 'number') return Number(x) * y;
        else return x * y;
    }
}
function divide(x, y) {
    return Number(x) / Number(y);
}
function quotient(x, y) {
    if (typeof x === 'number' || typeof y === 'number') {
        const q = Math.trunc(Number(x) / Number(y));
        if (typeof BigInt === 'undefined') return q;
        else return BigInt(q);
    } else {
        return x / y;
    }
}
function remainder(x, y) {
    if (typeof x === 'number' || typeof y === 'number') return Number(x) % Number(y);
    else return x % y;
}
function compare(x, y) {
    if (typeof x === 'number') {
        if (typeof y === 'number') return Math.sign(x - y);
        else return Math.sign(x - Number(y));
    } else {
        if (typeof y === 'number') return Math.sign(Number(x) - y);
        else return x < y ? -1 : y < x ? 1 : 0;
    }
}
function tryToParse(token) {
    try {
        return BigInt(token);
    } catch (_ex) {
        const n = Number(token);
        if (isNaN(n)) return null;
        return n;
    }
}
function convertToString(x) {
    const s = x + '';
    if (typeof BigInt !== 'undefined') {
        if (typeof x === 'number') {
            if (Number.isInteger(x) && !s.includes('e')) return s + '.0';
        }
    }
    return s;
}
function assert(x, message) {
    if (!x) throw new Error("Assertion Failure: " + (message || ""));
}
let readLine;
let write;
let exit;
class Cell {
    constructor(car, cdr){
        this.car = car;
        this.cdr = cdr;
    }
    toString() {
        return "(" + this.car + " . " + this.cdr + ")";
    }
    get length() {
        return foldl(0, this, (i, _)=>i + 1
        );
    }
    car;
    cdr;
}
function foldl(x, j, fn) {
    while(j !== null){
        x = fn(x, j.car);
        j = j.cdr;
    }
    return x;
}
function mapcar(j, fn) {
    if (j === null) return null;
    const a = fn(j.car);
    let d = j.cdr;
    if (d instanceof Cell) d = mapcar(d, fn);
    if (Object.is(j.car, a) && Object.is(j.cdr, d)) return j;
    return new Cell(a, d);
}
class Sym {
    constructor(name){
        this.name = name;
    }
    toString() {
        return this.name;
    }
    get isInterned() {
        return symTable[this.name] === this;
    }
    name;
}
class Keyword extends Sym {
    constructor(name){
        super(name);
    }
}
const symTable = {};
function newSym(name, isKeyword = false) {
    let result = symTable[name];
    assert(result === undefined || !isKeyword, name);
    if (result === undefined) {
        result = isKeyword ? new Keyword(name) : new Sym(name);
        symTable[name] = result;
    }
    return result;
}
function newKeyword(name) {
    return newSym(name, true);
}
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
function cdrCell(x) {
    const k = x.cdr;
    if (k instanceof Cell) return k;
    else if (k === null) return null;
    else throw new EvalException("proper list expected", x);
}
class Func {
    constructor(carity){
        this.carity = carity;
    }
    get arity() {
        return this.carity < 0 ? -this.carity : this.carity;
    }
    get hasRest() {
        return this.carity < 0;
    }
    get fixedArgs() {
        return this.carity < 0 ? -this.carity - 1 : this.carity;
    }
    makeFrame(arg) {
        const frame = new Array(this.arity);
        const n = this.fixedArgs;
        let i = 0;
        for(; i < n && arg !== null; i++){
            frame[i] = arg.car;
            arg = cdrCell(arg);
        }
        if (i !== n || arg !== null && !this.hasRest) throw new EvalException("arity not matched", this);
        if (this.hasRest) frame[n] = arg;
        return frame;
    }
    evalFrame(frame, interp, env) {
        const n = this.fixedArgs;
        for(let i = 0; i < n; i++)frame[i] = interp.eval(frame[i], env);
        if (this.hasRest && frame[n] instanceof Cell) {
            let z = null;
            let y = null;
            for(let j = frame[n]; j !== null; j = cdrCell(j)){
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
    carity;
}
class DefinedFunc extends Func {
    constructor(carity, body){
        super(carity);
        this.body = body;
    }
    body;
}
class Macro extends DefinedFunc {
    constructor(carity, body){
        super(carity, body);
    }
    toString() {
        return `#<macro:${this.carity}:${str(this.body)}>`;
    }
    expandWith(interp, arg) {
        const frame = this.makeFrame(arg);
        const env = new Cell(frame, null);
        let x = null;
        for(let j = this.body; j !== null; j = cdrCell(j))x = interp.eval(j.car, env);
        return x;
    }
    static make(carity, body, env) {
        assert(env === null);
        return new Macro(carity, body);
    }
}
class Lambda extends DefinedFunc {
    constructor(carity, body){
        super(carity, body);
    }
    toString() {
        return `#<lambda:${this.carity}:${str(this.body)}>`;
    }
    static make(carity, body, env) {
        assert(env === null);
        return new Lambda(carity, body);
    }
}
class Closure extends DefinedFunc {
    constructor(carity, body, env){
        super(carity, body);
        this.env = env;
    }
    static makeFrom(x, env) {
        return new Closure(x.carity, x.body, env);
    }
    toString() {
        return `#<closure:${this.carity}:${str(this.env)}:${str(this.body)}>`;
    }
    makeEnv(interp, arg, interpEnv) {
        const frame = this.makeFrame(arg);
        this.evalFrame(frame, interp, interpEnv);
        return new Cell(frame, this.env);
    }
    static make(carity, body, env) {
        return new Closure(carity, body, env);
    }
    env;
}
class BuiltInFunc extends Func {
    constructor(name, carity, body){
        super(carity);
        this.name = name;
        this.body = body;
    }
    toString() {
        return "#<" + this.name + ":" + this.carity + ">";
    }
    evalWith(interp, arg, interpEnv) {
        const frame = this.makeFrame(arg);
        this.evalFrame(frame, interp, interpEnv);
        try {
            return this.body(frame);
        } catch (ex) {
            if (ex instanceof EvalException) throw ex;
            else throw new EvalException(ex + " -- " + this.name, frame);
        }
    }
    name;
    body;
}
class Arg {
    constructor(level, offset, symbol){
        this.level = level;
        this.offset = offset;
        this.symbol = symbol;
    }
    toString() {
        return "#" + this.level + ":" + this.offset + ":" + this.symbol;
    }
    setValue(x, env) {
        for(let i = 0; i < this.level; i++)env = env.cdr;
        env.car[this.offset] = x;
    }
    getValue(env) {
        for(let i = 0; i < this.level; i++)env = env.cdr;
        return env.car[this.offset];
    }
    level;
    offset;
    symbol;
}
class EvalException extends Error {
    trace = [];
    constructor(msg, x, quoteString = true){
        super(msg + ": " + str(x, quoteString));
    }
    toString() {
        let s = "EvalException: " + this.message;
        for (const line of this.trace)s += "\n\t" + line;
        return s;
    }
}
class NotVariableException extends EvalException {
    constructor(x){
        super("variable expected", x);
    }
}
class FormatException extends Error {
    constructor(msg){
        super(msg);
    }
}
const EndOfFile = {
    toString: ()=>"EOF"
};
class Interp {
    globals = new Map();
    constructor(){
        this.def("car", 1, (a)=>a[0] === null ? null : a[0].car
        );
        this.def("cdr", 1, (a)=>a[0] === null ? null : a[0].cdr
        );
        this.def("cons", 2, (a)=>new Cell(a[0], a[1])
        );
        this.def("atom", 1, (a)=>a[0] instanceof Cell ? null : true
        );
        this.def("eq", 2, (a)=>Object.is(a[0], a[1]) ? true : null
        );
        this.def("list", -1, (a)=>a[0]
        );
        this.def("rplaca", 2, (a)=>{
            a[0].car = a[1];
            return a[1];
        });
        this.def("rplacd", 2, (a)=>{
            a[0].cdr = a[1];
            return a[1];
        });
        this.def("length", 1, (a)=>a[0] === null ? 0 : quotient(a[0].length, 1)
        );
        this.def("stringp", 1, (a)=>typeof a[0] === "string" ? true : null
        );
        this.def("numberp", 1, (a)=>isNumeric(a[0]) ? true : null
        );
        this.def("eql", 2, (a)=>{
            const x = a[0];
            const y = a[1];
            return x === y ? true : isNumeric(x) && isNumeric(y) && compare(x, y) === 0 ? true : null;
        });
        this.def("<", 2, (a)=>compare(a[0], a[1]) < 0 ? true : null
        );
        this.def("%", 2, (a)=>remainder(a[0], a[1])
        );
        this.def("mod", 2, (a)=>{
            const x = a[0];
            const y = a[1];
            const q = remainder(x, y);
            return compare(multiply(x, y), ZERO) < 0 ? add(q, y) : q;
        });
        this.def("+", -1, (a)=>foldl(ZERO, a[0], (i, j)=>add(i, j)
            )
        );
        this.def("*", -1, (a)=>foldl(ONE, a[0], (i, j)=>multiply(i, j)
            )
        );
        this.def("-", -2, (a)=>{
            const x = a[0];
            const y = a[1];
            return y == null ? -x : foldl(x, y, (i, j)=>subtract(i, j)
            );
        });
        this.def("/", -3, (a)=>foldl(divide(a[0], a[1]), a[2], (i, j)=>divide(i, j)
            )
        );
        this.def("truncate", -2, (a)=>{
            const x = a[0];
            const y = a[1];
            if (y === null) {
                return quotient(x, ONE);
            } else if (y.cdr === null) {
                return quotient(x, y.car);
            } else {
                throw "one or two arguments expected";
            }
        });
        this.def("prin1", 1, (a)=>{
            write(str(a[0], true));
            return a[0];
        });
        this.def("princ", 1, (a)=>{
            write(str(a[0], false));
            return a[0];
        });
        this.def("terpri", 0, (_a)=>{
            write("\n");
            return true;
        });
        const gensymCounter = newSym("*gensym-counter*");
        this.globals.set(gensymCounter, ONE);
        this.def("gensym", 0, (_a)=>{
            const i = this.globals.get(gensymCounter);
            this.globals.set(gensymCounter, add(i, ONE));
            return new Sym("G" + i);
        });
        this.def("make-symbol", 1, (a)=>new Sym(a[0])
        );
        this.def("intern", 1, (a)=>newSym(a[0])
        );
        this.def("symbol-name", 1, (a)=>a[0].name
        );
        this.def("apply", 2, (a)=>this.eval(new Cell(a[0], mapcar(a[1], qqQuote)), null)
        );
        this.def("exit", 1, (a)=>exit(Number(a[0]))
        );
        this.def("dump", 0, (_a)=>{
            let s = null;
            for (const x of this.globals.keys())s = new Cell(x, s);
            return s;
        });
        this.globals.set(newSym("*version*"), new Cell(2.1, new Cell("TypeScript", new Cell("Nukata Lisp", null))));
    }
    def(name, carity, body) {
        const sym = newSym(name);
        this.globals.set(sym, new BuiltInFunc(name, carity, body));
    }
    eval(x, env) {
        try {
            for(;;){
                if (x instanceof Arg) {
                    assert(env !== null);
                    return x.getValue(env);
                } else if (x instanceof Sym) {
                    const value = this.globals.get(x);
                    if (value === undefined) throw new EvalException("void variable", x);
                    return value;
                } else if (x instanceof Cell) {
                    let fn = x.car;
                    const arg = cdrCell(x);
                    if (fn instanceof Keyword) {
                        switch(fn){
                            case quoteSym:
                                if (arg !== null && arg.cdr === null) return arg.car;
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
                                if (env !== null) throw new EvalException("nested macro", x);
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
                    } else {
                        if (fn instanceof Sym) {
                            fn = this.globals.get(fn);
                            if (fn === undefined) throw new EvalException("undefined", x.car);
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
                    return x;
                }
            }
        } catch (ex) {
            if (ex instanceof EvalException) {
                if (ex.trace.length < 10) ex.trace.push(str(x));
            }
            throw ex;
        }
    }
    evalProgN(j, env) {
        if (j === null) return null;
        for(;;){
            const x = j.car;
            j = cdrCell(j);
            if (j === null) return x;
            this.eval(x, env);
        }
    }
    evalCond(j, env) {
        for(; j !== null; j = cdrCell(j)){
            const clause = j.car;
            if (clause instanceof Cell) {
                const result = this.eval(clause.car, env);
                if (result !== null) {
                    const body = cdrCell(clause);
                    if (body === null) return qqQuote(result);
                    else return this.evalProgN(body, env);
                }
            } else if (clause !== null) {
                throw new EvalException("cond test expected", clause);
            }
        }
        return null;
    }
    evalSetQ(j, env) {
        let result = null;
        for(; j !== null; j = cdrCell(j)){
            const lval = j.car;
            j = cdrCell(j);
            if (j === null) throw new EvalException("right value expected", lval);
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
    compile(arg, env, make) {
        if (arg === null) throw new EvalException("arglist and body expected", arg);
        const table = new Map();
        const [hasRest, arity] = makeArgTable(arg.car, table);
        let body = cdrCell(arg);
        body = scanForArgs(body, table);
        body = this.expandMacros(body, 20);
        body = this.compileInners(body);
        return make(hasRest ? -arity : arity, body, env);
    }
    expandMacros(j, count) {
        if (count > 0 && j instanceof Cell) {
            let k = j.car;
            switch(k){
                case quoteSym:
                case lambdaSym:
                case macroSym:
                    return j;
                case quasiquoteSym:
                    {
                        const d = cdrCell(j);
                        if (d !== null && d.cdr === null) {
                            const z = qqExpand(d.car);
                            return this.expandMacros(z, count);
                        }
                        throw new EvalException("bad quasiquote", j);
                    }
                default:
                    if (k instanceof Sym) k = this.globals.get(k);
                    if (k instanceof Macro) {
                        const d = cdrCell(j);
                        const z = k.expandWith(this, d);
                        return this.expandMacros(z, count - 1);
                    }
                    return mapcar(j, (x)=>this.expandMacros(x, count)
                    );
            }
        } else {
            return j;
        }
    }
    compileInners(j) {
        if (j instanceof Cell) {
            const k = j.car;
            switch(k){
                case quoteSym:
                    return j;
                case lambdaSym:
                    {
                        const d = cdrCell(j);
                        return this.compile(d, null, Lambda.make);
                    }
                case macroSym:
                    throw new EvalException("nested macro", j);
                default:
                    return mapcar(j, (x)=>this.compileInners(x)
                    );
            }
        } else {
            return j;
        }
    }
}
function makeArgTable(arg, table) {
    if (arg === null) {
        return [
            false,
            0
        ];
    } else if (arg instanceof Cell) {
        let ag = arg;
        let offset = 0;
        let hasRest = false;
        for(; ag !== null; ag = cdrCell(ag)){
            let j = ag.car;
            if (hasRest) throw new EvalException("2nd rest", j);
            if (j === restSym) {
                ag = cdrCell(ag);
                if (ag === null) throw new NotVariableException(ag);
                j = ag.car;
                if (j === restSym) throw new NotVariableException(j);
                hasRest = true;
            }
            let sym;
            if (j instanceof Sym) sym = j;
            else if (j instanceof Arg) sym = j.symbol;
            else throw new NotVariableException(j);
            if (table.has(sym)) throw new EvalException("duplicated argument name", j);
            table.set(sym, new Arg(0, offset, sym));
            offset++;
        }
        return [
            hasRest,
            offset
        ];
    } else {
        throw new EvalException("arglist expected", arg);
    }
}
function scanForArgs(j, table) {
    if (j instanceof Sym) {
        const k = table.get(j);
        return k === undefined ? j : k;
    } else if (j instanceof Arg) {
        const k = table.get(j.symbol);
        return k === undefined ? new Arg(j.level + 1, j.offset, j.symbol) : k;
    } else if (j instanceof Cell) {
        if (j.car === quoteSym) {
            return j;
        } else if (j.car === quasiquoteSym) {
            return new Cell(quasiquoteSym, scanForQQ(j.cdr, table, 0));
        } else {
            return mapcar(j, (x)=>scanForArgs(x, table)
            );
        }
    } else {
        return j;
    }
}
function scanForQQ(j, table, level) {
    if (j instanceof Cell) {
        const k = j.car;
        if (k === quasiquoteSym) {
            return new Cell(k, scanForQQ(j.cdr, table, level + 1));
        } else if (k === unquoteSym || k === unquoteSplicingSym) {
            const d = level === 0 ? scanForArgs(j.cdr, table) : scanForQQ(j.cdr, table, level - 1);
            if (Object.is(d, j.cdr)) return j;
            return new Cell(k, d);
        } else {
            return mapcar(j, (x)=>scanForQQ(x, table, level)
            );
        }
    } else {
        return j;
    }
}
function qqExpand(x) {
    return qqExpand0(x, 0);
}
function qqExpand0(x, level) {
    if (x instanceof Cell) {
        if (x.car === unquoteSym) {
            if (level === 0) return x.cdr.car;
        }
        const t = qqExpand1(x, level);
        if (t.car instanceof Cell && t.cdr === null) {
            const k = t.car;
            if (k.car == listSym || k.car === consSym) return k;
        }
        return new Cell(appendSym, t);
    } else {
        return qqQuote(x);
    }
}
function qqQuote(x) {
    if (x instanceof Sym || x instanceof Cell) return new Cell(quoteSym, new Cell(x, null));
    return x;
}
function qqExpand1(x, level) {
    if (x instanceof Cell) {
        if (x.car === unquoteSym) {
            if (level === 0) return x.cdr;
            level--;
        } else if (x.car === quasiquoteSym) {
            level++;
        }
        const h = qqExpand2(x.car, level);
        const t = qqExpand1(x.cdr, level);
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
function qqConcat(x, y) {
    if (x === null) return y;
    return new Cell(x.car, qqConcat(x.cdr, y));
}
function qqConsCons(x, y) {
    if (x === null) return y;
    return new Cell(consSym, new Cell(x.car, new Cell(qqConsCons(x.cdr, y), null)));
}
function qqExpand2(y, level) {
    if (y instanceof Cell) {
        switch(y.car){
            case unquoteSym:
                if (level === 0) return new Cell(listSym, y.cdr);
                level--;
                break;
            case unquoteSplicingSym:
                if (level === 0) return y.cdr.car;
                level--;
                break;
            case quasiquoteSym:
                level++;
                break;
        }
    }
    return new Cell(listSym, new Cell(qqExpand0(y, level), null));
}
class Reader {
    token;
    tokens = [];
    lineNo = 1;
    push(text) {
        const tokenPat = /\s+|;.*$|("(\\.?|.)*?"|,@?|[^()'`~"; \t]+|.)/g;
        for (const line of text.split("\n")){
            for(;;){
                const result = tokenPat.exec(line);
                if (result === null) break;
                const s = result[1];
                if (s !== undefined) this.tokens.push(s);
            }
            this.tokens.push("\n");
        }
    }
    copyFrom(other) {
        this.tokens = other.tokens.slice();
        this.lineNo = other.lineNo;
    }
    clear() {
        this.tokens.length = 0;
    }
    isEmpty() {
        return this.tokens.every((t)=>t === "\n"
        );
    }
    read() {
        try {
            this.readToken();
            return this.parseExpression();
        } catch (ex) {
            if (ex === EndOfFile) throw EndOfFile;
            else if (ex instanceof FormatException) throw new EvalException("syntax error", ex.message + " at " + this.lineNo, false);
            else throw ex;
        }
    }
    parseExpression() {
        switch(this.token){
            case leftParenSym:
                this.readToken();
                return this.parseListBody();
            case singleQuoteSym:
                this.readToken();
                return new Cell(quoteSym, new Cell(this.parseExpression(), null));
            case backQuoteSym:
                this.readToken();
                return new Cell(quasiquoteSym, new Cell(this.parseExpression(), null));
            case commaSym:
                this.readToken();
                return new Cell(unquoteSym, new Cell(this.parseExpression(), null));
            case commaAtSym:
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
        } else {
            const e1 = this.parseExpression();
            this.readToken();
            let e2;
            if (this.token == dotSym) {
                this.readToken();
                e2 = this.parseExpression();
                this.readToken();
                if (this.token !== rightParenSym) throw new FormatException('")" expected: ' + this.token);
            } else {
                e2 = this.parseListBody();
            }
            return new Cell(e1, e2);
        }
    }
    readToken() {
        for(;;){
            const t = this.tokens.shift();
            if (t === undefined) {
                throw EndOfFile;
            } else if (t === "\n") {
                this.lineNo += 1;
            } else if (t === "+" || t === "-") {
                this.token = newSym(t);
                return;
            } else {
                if (t[0] === '"') {
                    let s = t;
                    const n = s.length - 1;
                    if (n < 1 || s[n] !== '"') throw new FormatException("bad string: " + s);
                    s = s.substring(1, n);
                    s = s.replace(/\\./g, (m)=>{
                        const val = Reader.escapes[m];
                        return val === undefined ? m : val;
                    });
                    this.token = s;
                    return;
                }
                const n = tryToParse(t);
                if (n !== null) this.token = n;
                else if (t === "nil") this.token = null;
                else if (t === "t") this.token = true;
                else this.token = newSym(t);
                return;
            }
        }
    }
    static escapes = {
        "\\\\": "\\",
        '\\"': '"',
        "\\n": "\n",
        "\\r": "\r",
        "\\f": "\f",
        "\\b": "\b",
        "\\t": "\t",
        "\\v": "\v"
    };
}
const quotes = {
    [quoteSym.name]: "'",
    [quasiquoteSym.name]: "`",
    [unquoteSym.name]: ",",
    [unquoteSplicingSym.name]: ",@"
};
function str(x, quoteString = true, count, printed) {
    if (x === null) {
        return "nil";
    } else if (x === true) {
        return "t";
    } else if (x instanceof Cell) {
        if (x.car instanceof Sym) {
            const q = quotes[x.car.name];
            if (q !== undefined && x.cdr instanceof Cell) {
                if (x.cdr.cdr == null) return q + str(x.cdr.car, true, count, printed);
            }
        }
        return "(" + strListBody(x, count, printed) + ")";
    } else if (typeof x === "string") {
        if (!quoteString) return x;
        const bf = [
            '"'
        ];
        for (const ch of x){
            switch(ch){
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
    } else if (x instanceof Array) {
        const s = x.map((e)=>str(e, true, count, printed)
        ).join(", ");
        return "[" + s + "]";
    } else if (x instanceof Sym) {
        return x.isInterned ? x.name : "#:" + x;
    } else if (isNumeric(x)) {
        return convertToString(x);
    } else {
        return x + "";
    }
}
function strListBody(x, count, printed) {
    if (printed === undefined) printed = [];
    if (count === undefined) count = 4;
    const s = [];
    let y;
    for(y = x; y instanceof Cell; y = y.cdr){
        if (printed.indexOf(y) < 0) {
            printed.push(y);
            count = 4;
        } else {
            count--;
            if (count < 0) {
                s.push("...");
                return s.join(" ");
            }
        }
        s.push(str(y.car, true, count, printed));
    }
    if (y !== null) {
        s.push(".");
        s.push(str(y, true, count, printed));
    }
    for(y = x; y instanceof Cell; y = y.cdr){
        const i = printed.indexOf(y);
        if (i >= 0) printed.splice(i, 1);
    }
    return s.join(" ");
}
class REPL {
    stdInTokens = new Reader();
    async readExpression(prompt1, prompt2) {
        const oldTokens = new Reader();
        for(;;){
            oldTokens.copyFrom(this.stdInTokens);
            try {
                return this.stdInTokens.read();
            } catch (ex) {
                if (ex === EndOfFile) {
                    write(oldTokens.isEmpty() ? prompt1 : prompt2);
                    const line = await readLine();
                    if (line === null) return EndOfFile;
                    oldTokens.push(line);
                    this.stdInTokens.copyFrom(oldTokens);
                } else {
                    this.stdInTokens.clear();
                    throw ex;
                }
            }
        }
    }
    async readEvalPrintLoop(interp) {
        for(;;){
            try {
                const exp = await this.readExpression("> ", "  ");
                if (exp === EndOfFile) {
                    write("Goodbye\n");
                    return;
                }
                const result = interp.eval(exp, null);
                write(str(result) + "\n");
            } catch (ex) {
                if (ex instanceof EvalException) write(ex + "\n");
                else throw ex;
            }
        }
    }
}
function run(interp, text) {
    const tokens = new Reader();
    tokens.push(text);
    let result = undefined;
    while(!tokens.isEmpty()){
        const exp = tokens.read();
        result = interp.eval(exp, null);
    }
    return result;
}
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
if (typeof Deno !== 'undefined') {
    const decoder = new TextDecoder();
    const buf = new Uint8Array(8000);
    readLine = async function() {
        const n = await Deno.stdin.read(buf);
        if (n == null) return null;
        return decoder.decode(buf.subarray(0, n));
    };
    const encoder = new TextEncoder();
    write = (s)=>{
        const bb = encoder.encode(s);
        for(let n = 0; n < bb.length;)n += Deno.stdout.writeSync(bb.subarray(n));
    };
    exit = Deno.exit;
    const main = async function() {
        const interp = new Interp();
        run(interp, prelude);
        let repl = undefined;
        const args = Deno.args.length == 0 ? [
            '-'
        ] : Deno.args;
        try {
            for (const fileName of args){
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
    let readLine_atFirst = true;
    let readLine_resolve = null;
    readLine = ()=>new Promise((resolve, _reject)=>{
            if (readLine_atFirst) {
                process.stdin.setEncoding('utf8');
                process.stdin.on('data', (line)=>{
                    if (readLine_resolve !== null) {
                        readLine_resolve(line);
                        readLine_resolve = null;
                    }
                });
                process.stdin.on('end', ()=>{
                    if (readLine_resolve !== null) {
                        readLine_resolve(null);
                        readLine_resolve = null;
                    }
                });
                readLine_atFirst = false;
            }
            readLine_resolve = resolve;
        })
    ;
    write = (s)=>process.stdout.write(s)
    ;
    exit = process.exit;
    const main = async function() {
        const interp = new Interp();
        run(interp, prelude);
        let repl = undefined;
        let fs = undefined;
        let argv = process.argv;
        if (argv.length <= 2) argv = [
            '',
            '',
            '-'
        ];
        try {
            for(let i = 2; i < argv.length; i++){
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
