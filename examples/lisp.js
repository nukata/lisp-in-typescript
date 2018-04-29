/*
  Nukata Lisp Light 1.27 in TypeScript 2.2 by SUZUKI Hisao (H29.3/13)
  $ tsc -t ES2015 lisp.ts && node lisp.js
  See "A Lisp interpreter in TypeScript"
      http://www.oki-osk.jp/esc/typescript/lisp-en.html
*/
"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = Object.setPrototypeOf ||
        ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
        function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
// Class and functions for the convenience of porting from Dart
// Base class of user-defined exceptions
var Exception = /** @class */ (function (_super) {
    __extends(Exception, _super);
    function Exception(message) {
        var _this = _super.call(this) || this;
        // XXX super(message) has no effects for Error; set it up manually.
        // Capture the stack trace if it runs on Node.js.
        var capture = Error["captureStackTrace"];
        if (capture !== undefined)
            capture(_this, _this.constructor);
        _this.message = message;
        // If Function#name in ES6 is available,
        // set the class name at runtime to this.name;
        // otherwise set a fixed name to it.
        var name = _this.constructor["name"];
        _this.name = (name !== undefined) ? name : "Exception";
        return _this;
    }
    return Exception;
}(Error));
// Return true if two arguments are identical. (cf. Object#if in ES6)
function identical(x, y) {
    if (x === y)
        return x !== 0 || 1 / x === 1 / y;
    else
        return x !== x && y !== y;
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
var Cell = /** @class */ (function () {
    function Cell(car, cdr) {
        this.car = car;
        this.cdr = cdr;
    }
    Cell.prototype.toString = function () { return "(" + this.car + " . " + this.cdr + ")"; };
    Object.defineProperty(Cell.prototype, "length", {
        // Length as a list
        get: function () { return foldl(0, this, function (i, e) { return i + 1; }); },
        enumerable: true,
        configurable: true
    });
    return Cell;
}());
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
    var a = fn(j.car);
    var d = j.cdr;
    if (d instanceof Cell)
        d = mapcar(d, fn);
    if (identical(j.car, a) && identical(j.cdr, d))
        return j;
    return new Cell(a, d);
}
// Lisp symbol
var Sym = /** @class */ (function () {
    function Sym(name) {
        this.name = name;
    } // Construct an uninterned symbol.
    Sym.prototype.toString = function () { return this.name; };
    Object.defineProperty(Sym.prototype, "isInterned", {
        get: function () {
            return symTable[this.name] === this;
        },
        enumerable: true,
        configurable: true
    });
    return Sym;
}());
// Expression keyword
var Keyword = /** @class */ (function (_super) {
    __extends(Keyword, _super);
    function Keyword(name) {
        return _super.call(this, name) || this;
    }
    return Keyword;
}(Sym));
// The table of interned symbols
var symTable = {};
// Construct an interned symbol; construct a Keyword if isKeyword holds.
function newSym(name, isKeyword) {
    if (isKeyword === void 0) { isKeyword = false; }
    var result = symTable[name];
    assert((result === undefined || !isKeyword), name);
    if (result === undefined) {
        result = isKeyword ? new Keyword(name) : new Sym(name);
        symTable[name] = result;
    }
    return result;
}
function newKeyword(name) { return newSym(name, true); }
var backQuoteSym = newSym("`");
var commaAtSym = newSym(",@");
var commaSym = newSym(",");
var dotSym = newSym(".");
var leftParenSym = newSym("(");
var rightParenSym = newSym(")");
var singleQuoteSym = newSym("'");
var appendSym = newSym("append");
var consSym = newSym("cons");
var listSym = newSym("list");
var restSym = newSym("&rest");
var unquoteSym = newSym("unquote");
var unquoteSplicingSym = newSym("unquote-splicing");
var condSym = newKeyword("cond");
var lambdaSym = newKeyword("lambda");
var macroSym = newKeyword("macro");
var prognSym = newKeyword("progn");
var quasiquoteSym = newKeyword("quasiquote");
var quoteSym = newKeyword("quote");
var setqSym = newKeyword("setq");
//----------------------------------------------------------------------
// Get cdr of list x as a Cell or null.
function cdrCell(x) {
    var k = x.cdr;
    if (k instanceof Cell)
        return k;
    else if (k === null)
        return null;
    else
        throw new EvalException("proper list expected", x);
}
// Common base class of Lisp functions
var Func = /** @class */ (function () {
    // carity is a number of arguments, made negative if the func has &rest.
    function Func(carity) {
        this.carity = carity;
    }
    Object.defineProperty(Func.prototype, "arity", {
        get: function () {
            return (this.carity < 0) ? -this.carity : this.carity;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Func.prototype, "hasRest", {
        get: function () {
            return (this.carity < 0);
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Func.prototype, "fixedArgs", {
        get: function () {
            return (this.carity < 0) ? -this.carity - 1 : this.carity;
        },
        enumerable: true,
        configurable: true
    });
    // Make a call-frame from a list of actual arguments.
    Func.prototype.makeFrame = function (arg) {
        var frame = new Array(this.arity);
        var n = this.fixedArgs;
        var i = 0;
        for (; i < n && arg !== null; i++) { // Set the list of fiexed args.
            frame[i] = arg.car;
            arg = cdrCell(arg);
        }
        if (i !== n || (arg !== null && !this.hasRest))
            throw new EvalException("arity not matched", this);
        if (this.hasRest)
            frame[n] = arg;
        return frame;
    };
    // Evaluate each expression of a frame.
    Func.prototype.evalFrame = function (frame, interp, env) {
        var n = this.fixedArgs;
        for (var i = 0; i < n; i++)
            frame[i] = interp.eval(frame[i], env);
        if (this.hasRest && frame[n] instanceof Cell) {
            var z = null;
            var y = null;
            for (var j = frame[n]; j != null; j = cdrCell(j)) {
                var e = interp.eval(j.car, env);
                var x = new Cell(e, null);
                if (z === null)
                    z = x;
                else
                    y.cdr = x;
                y = x;
            }
            frame[n] = z;
        }
    };
    return Func;
}());
// Common base class of functions which are defined with Lisp expressions
var DefinedFunc = /** @class */ (function (_super) {
    __extends(DefinedFunc, _super);
    // body is a Lisp list as the function body.
    function DefinedFunc(carity, body) {
        var _this = _super.call(this, carity) || this;
        _this.body = body;
        return _this;
    }
    return DefinedFunc;
}(Func));
// Compiled macro expression
var Macro = /** @class */ (function (_super) {
    __extends(Macro, _super);
    function Macro(carity, body) {
        return _super.call(this, carity, body) || this;
    }
    Macro.prototype.toString = function () { return "#<macro:" + this.carity + ":" + str(this.body) + ">"; };
    // Expand the macro with a list of actual arguments.
    Macro.prototype.expandWith = function (interp, arg) {
        var frame = this.makeFrame(arg);
        var env = new Cell(frame, null);
        var x = null;
        for (var j = this.body; j != null; j = cdrCell(j))
            x = interp.eval(j.car, env);
        return x;
    };
    Macro.make = function (carity, body, env) {
        assert(env === null);
        return new Macro(carity, body);
    };
    return Macro;
}(DefinedFunc));
// Compiled lambda expression (within another function)
var Lambda = /** @class */ (function (_super) {
    __extends(Lambda, _super);
    function Lambda(carity, body) {
        return _super.call(this, carity, body) || this;
    }
    Lambda.prototype.toString = function () { return "#<lambda:" + this.carity + ":" + str(this.body) + ">"; };
    Lambda.make = function (carity, body, env) {
        assert(env === null);
        return new Lambda(carity, body);
    };
    return Lambda;
}(DefinedFunc));
// Compiled lambda expression (Closure with environment)
var Closure = /** @class */ (function (_super) {
    __extends(Closure, _super);
    // env is the environment of the closure.
    function Closure(carity, body, env) {
        var _this = _super.call(this, carity, body) || this;
        _this.env = env;
        return _this;
    }
    Closure.makeFrom = function (x, env) {
        return new Closure(x.carity, x.body, env);
    };
    Closure.prototype.toString = function () {
        return "#<closure:" + this.carity + ":" + str(this.env) + ":" + str(this.body) + ">";
    };
    // Make a new environment from a list of actual arguments.
    Closure.prototype.makeEnv = function (interp, arg, interpEnv) {
        var frame = this.makeFrame(arg);
        this.evalFrame(frame, interp, interpEnv);
        return new Cell(frame, this.env); // Prepend the frame to the env.
    };
    Closure.make = function (carity, body, env) {
        return new Closure(carity, body, env);
    };
    return Closure;
}(DefinedFunc));
// Built-in function
var BuiltInFunc = /** @class */ (function (_super) {
    __extends(BuiltInFunc, _super);
    // name is the function name; body is the function body.
    function BuiltInFunc(name, carity, body) {
        var _this = _super.call(this, carity) || this;
        _this.name = name;
        _this.body = body;
        return _this;
    }
    BuiltInFunc.prototype.toString = function () { return "#<" + this.name + ":" + this.carity + ">"; };
    // Invoke the built-in function with a list of actual arguments.
    BuiltInFunc.prototype.evalWith = function (interp, arg, interpEnv) {
        var frame = this.makeFrame(arg);
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
    };
    return BuiltInFunc;
}(Func));
// Bound variable in a compiled lambda/macro expression
var Arg = /** @class */ (function () {
    function Arg(level, offset, symbol) {
        this.level = level;
        this.offset = offset;
        this.symbol = symbol;
    }
    Arg.prototype.toString = function () {
        return "#" + this.level + ":" + this.offset + ":" + this.symbol;
    };
    // Set a value x to the location corresponding to the variable in env.
    Arg.prototype.setValue = function (x, env) {
        for (var i = 0; i < this.level; i++)
            env = env.cdr;
        env.car[this.offset] = x;
    };
    // Get a value from the location corresponding to the variable in env.
    Arg.prototype.getValue = function (env) {
        for (var i = 0; i < this.level; i++)
            env = env.cdr;
        return env.car[this.offset];
    };
    return Arg;
}());
// Exception in evaluation
var EvalException = /** @class */ (function (_super) {
    __extends(EvalException, _super);
    function EvalException(msg, x, quoteString) {
        if (quoteString === void 0) { quoteString = true; }
        var _this = _super.call(this, msg + ": " + str(x, quoteString)) || this;
        _this.trace = [];
        return _this;
    }
    EvalException.prototype.toString = function () {
        var s = "EvalException: " + this.message;
        for (var _i = 0, _a = this.trace; _i < _a.length; _i++) {
            var line = _a[_i];
            s += "\n\t" + line;
        }
        return s;
    };
    return EvalException;
}(Exception));
// Exception which indicates an absence of a variable
var NotVariableException = /** @class */ (function (_super) {
    __extends(NotVariableException, _super);
    function NotVariableException(x) {
        return _super.call(this, "variable expected", x) || this;
    }
    return NotVariableException;
}(EvalException));
// Exception thrown when something does not have an expected format
var FormatException = /** @class */ (function (_super) {
    __extends(FormatException, _super);
    function FormatException(msg) {
        return _super.call(this, msg) || this;
    }
    return FormatException;
}(Exception));
// Singleton for end-of-file
var EndOfFile = { toString: function () { return "EOF"; } };
//----------------------------------------------------------------------
// Core of the interpreter
var Interp = /** @class */ (function () {
    function Interp() {
        var _this = this;
        // Table of the global values of symbols
        // XXX Cannot use Sym type for keys; use Sym#name: string instead.
        this.globals = {};
        this.def("car", 1, function (a) {
            return (a[0] === null) ? null : a[0].car;
        });
        this.def("cdr", 1, function (a) {
            return (a[0] === null) ? null : a[0].cdr;
        });
        this.def("cons", 2, function (a) {
            return new Cell(a[0], a[1]);
        });
        this.def("atom", 1, function (a) {
            return (a[0] instanceof Cell) ? null : true;
        });
        this.def("eq", 2, function (a) {
            return identical(a[0], a[1]) ? true : null;
        });
        this.def("list", -1, function (a) { return a[0]; });
        this.def("rplaca", 2, function (a) { a[0].car = a[1]; return a[1]; });
        this.def("rplacd", 2, function (a) { a[0].cdr = a[1]; return a[1]; });
        this.def("length", 1, function (a) {
            return (a[0] === null ? 0 : a[0].length);
        });
        this.def("stringp", 1, function (a) {
            return (typeof a[0] === "string") ? true : null;
        });
        this.def("numberp", 1, function (a) {
            return (typeof a[0] === "number") ? true : null;
        });
        this.def("eql", 2, function (a) { return (a[0] === a[1]) ? true : null; });
        this.def("<", 2, function (a) { return (a[0] < a[1]) ? true : null; });
        this.def("%", 2, function (a) { return a[0] % a[1]; });
        this.def("mod", 2, function (a) {
            var x = a[0];
            var y = a[1];
            if ((x < 0 && y > 0) || (x > 0 && y < 0))
                return x % y + y;
            return x % y;
        });
        this.def("+", -1, function (a) { return foldl(0, a[0], function (i, j) { return i + j; }); });
        this.def("*", -1, function (a) { return foldl(1, a[0], function (i, j) { return i * j; }); });
        this.def("-", -2, function (a) {
            var x = a[0];
            var y = a[1];
            return (y == null) ? -x : foldl(x, y, function (i, j) { return i - j; });
        });
        this.def("/", -3, function (a) {
            return foldl(a[0] / a[1], a[2], function (i, j) { return i / j; });
        });
        this.def("truncate", -2, function (a) {
            var x = a[0];
            var y = a[1];
            if (y === null) {
            }
            else if (y.cdr === null) {
                x = x / y.car;
            }
            else {
                throw "one or two arguments expected";
            }
            return (x < 0) ? Math.ceil(x) : Math.floor(x);
        });
        this.def("prin1", 1, function (a) {
            write(str(a[0], true));
            return a[0];
        });
        this.def("princ", 1, function (a) {
            write(str(a[0], false));
            return a[0];
        });
        this.def("terpri", 0, function (a) {
            write("\n");
            return true;
        });
        var gensymCounter = "*gensym-counter*";
        this.globals[gensymCounter] = 1;
        this.def("gensym", 0, function (a) {
            var i = _this.globals[gensymCounter];
            _this.globals[gensymCounter] = i + 1;
            return new Sym("G" + i); // an uninterned symbol
        });
        this.def("make-symbol", 1, function (a) { return new Sym(a[0]); });
        this.def("intern", 1, function (a) { return newSym(a[0]); });
        this.def("symbol-name", 1, function (a) { return a[0].name; });
        this.def("apply", 2, function (a) {
            return _this.eval(new Cell(a[0], mapcar(a[1], qqQuote)), null);
        });
        this.def("exit", 1, function (a) { return exit(a[0]); });
        this.def("dump", 0, function (a) {
            var s = null;
            for (var x in _this.globals)
                s = new Cell(x, s);
            return s;
        });
        this.globals["*version*"] = // named after Tōkai-dō Mikawa-koku
            new Cell(1.27, // Nukata-gun (東海道 三河国 額田郡)
            new Cell("TypeScript", new Cell("Nukata Lisp Light", null)));
    }
    // Define a built-in function by giving a name, a carity, and a body.
    Interp.prototype.def = function (name, carity, body) {
        this.globals[name] = new BuiltInFunc(name, carity, body);
    };
    // Evaluate a Lisp expression in an environment.
    Interp.prototype.eval = function (x, env) {
        try {
            for (;;) {
                if (x instanceof Arg) {
                    return x.getValue(env);
                }
                else if (x instanceof Sym) {
                    var value = this.globals[x.name];
                    if (value === undefined)
                        throw new EvalException("void variable", x);
                    return value;
                }
                else if (x instanceof Cell) {
                    var c = x;
                    var fn = c.car;
                    var arg = cdrCell(c);
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
                            var cl = fn;
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
                var eex = ex;
                if (eex.trace.length < 10)
                    eex.trace.push(str(x));
            }
            throw ex;
        }
    };
    // (progn E1 E2 .. En) => Evaluate E1, E2, .. except for En and return it.
    Interp.prototype.evalProgN = function (j, env) {
        if (j === null)
            return null;
        for (;;) {
            var x = j.car;
            j = cdrCell(j);
            if (j === null)
                return x; // The tail exp will be evaluated at the caller.
            this.eval(x, env);
        }
    };
    // Evaluate a conditional expression and return the selection unevaluated.
    Interp.prototype.evalCond = function (j, env) {
        for (; j !== null; j = cdrCell(j)) {
            var clause = j.car;
            if (clause instanceof Cell) {
                var result = this.eval(clause.car, env);
                if (result !== null) { // If the condition holds
                    var body = cdrCell(clause);
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
    };
    // (setq V1 E1 ..) => Evaluate Ei and assign it to Vi; return the last.
    Interp.prototype.evalSetQ = function (j, env) {
        var result = null;
        for (; j !== null; j = cdrCell(j)) {
            var lval = j.car;
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
    };
    // Compile a Lisp list (macro ..) or (lambda ..).
    Interp.prototype.compile = function (arg, env, make) {
        if (arg === null)
            throw new EvalException("arglist and body expected", arg);
        var table = {};
        var _a = makeArgTable(arg.car, table), hasRest = _a[0], arity = _a[1];
        var body = cdrCell(arg);
        body = scanForArgs(body, table);
        body = this.expandMacros(body, 20); // Expand macros up to 20 nestings
        body = this.compileInners(body);
        return make((hasRest) ? -arity : arity, body, env);
    };
    // Expand macros and quasi-quotations in an expression.
    Interp.prototype.expandMacros = function (j, count) {
        var _this = this;
        if (count > 0 && j instanceof Cell) {
            var k = j.car;
            switch (k) {
                case quoteSym:
                case lambdaSym:
                case macroSym:
                    return j;
                case quasiquoteSym:
                    var d = cdrCell(j);
                    if (d !== null && d.cdr === null) {
                        var z = qqExpand(d.car);
                        return this.expandMacros(z, count);
                    }
                    throw new EvalException("bad quasiquote", j);
                default:
                    if (k instanceof Sym)
                        k = this.globals[k.name];
                    if (k instanceof Macro) {
                        var d_1 = cdrCell(j);
                        var z = k.expandWith(this, d_1);
                        return this.expandMacros(z, count - 1);
                    }
                    return mapcar(j, function (x) { return _this.expandMacros(x, count); });
            }
        }
        else {
            return j;
        }
    };
    // Replace inner lambda expressions with Lambda instances.
    Interp.prototype.compileInners = function (j) {
        var _this = this;
        if (j instanceof Cell) {
            var k = j.car;
            switch (k) {
                case quoteSym:
                    return j;
                case lambdaSym:
                    var d = cdrCell(j);
                    return this.compile(d, null, Lambda.make);
                case macroSym:
                    throw new EvalException("nested macro", j);
                default:
                    return mapcar(j, function (x) { return _this.compileInners(x); });
            }
        }
        else {
            return j;
        }
    };
    return Interp;
}());
//----------------------------------------------------------------------
// Make an argument table; return a pair of rest-yes/no and the arity.
function makeArgTable(arg, table) {
    if (arg === null) {
        return [false, 0];
    }
    else if (arg instanceof Cell) {
        var ag = arg;
        var offset = 0; // offset value within the call-frame
        var hasRest = false;
        for (; ag !== null; ag = cdrCell(ag)) {
            var j = ag.car;
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
            var sym = void 0;
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
        var k = table[j.name];
        return (k === undefined) ? j : k;
    }
    else if (j instanceof Arg) {
        var ag = j;
        var k = table[ag.symbol.name];
        return (k === undefined) ?
            new Arg(ag.level + 1, ag.offset, ag.symbol) : k;
    }
    else if (j instanceof Cell) {
        var c = j;
        if (c.car === quoteSym) {
            return c;
        }
        else if (c.car === quasiquoteSym) {
            return new Cell(quasiquoteSym, scanForQQ(c.cdr, table, 0));
        }
        else {
            return mapcar(j, function (x) { return scanForArgs(x, table); });
        }
    }
    else {
        return j;
    }
}
// Scan for quasi-quotes and scanForArgs them depending on the nesting level.
function scanForQQ(j, table, level) {
    if (j instanceof Cell) {
        var k = j.car;
        if (k === quasiquoteSym) {
            return new Cell(k, scanForQQ(j.cdr, table, level + 1));
        }
        else if (k === unquoteSym || k === unquoteSplicingSym) {
            var d = (level === 0) ?
                scanForArgs(j.cdr, table) :
                scanForQQ(j.cdr, table, level - 1);
            if (identical(d, j.cdr))
                return j;
            return new Cell(k, d);
        }
        else {
            return mapcar(j, function (x) { return scanForQQ(x, table, level); });
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
        var c = x;
        if (c.car === unquoteSym) { // ,a
            if (level === 0)
                return c.cdr.car; // ,a => a
        }
        var t = qqExpand1(c, level);
        if (t.car instanceof Cell && t.cdr === null) {
            var k = t.car;
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
        var c = x;
        if (c.car === unquoteSym) { // ,a
            if (level === 0)
                return x.cdr; // ,a => (a)
            level--;
        }
        else if (c.car === quasiquoteSym) { // `a
            level++;
        }
        var h = qqExpand2(c.car, level);
        var t = qqExpand1(c.cdr, level); // !== null
        if (t.car === null && t.cdr === null) {
            return new Cell(h, null);
        }
        else if (h instanceof Cell) {
            var hc = h;
            if (hc.car === listSym) {
                if (t.car instanceof Cell) {
                    var tcar = t.car;
                    if (tcar.car === listSym) {
                        var hh = qqConcat(hc, tcar.cdr);
                        return new Cell(hh, t.cdr);
                    }
                }
                if (hc.cdr instanceof Cell) {
                    var hh = qqConsCons(hc.cdr, t.car);
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
        var yc = y;
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
// Reader of Lisp expressions
var Reader = /** @class */ (function () {
    // Construct a Reader which will read Lisp exps from a given string.
    function Reader(text) {
        this.lineNo = 1;
        var tokenPat = /\s+|;.*$|("(\\.?|.)*?"|,@?|[^()'`~"; \t]+|.)/g;
        var textlines = text.split("\n");
        this.lines = new Array();
        for (;;) {
            var line = textlines.shift();
            if (line === undefined)
                break;
            var tokens = new Array();
            for (;;) {
                var result = tokenPat.exec(line);
                if (result === null)
                    break;
                var s = result[1];
                if (s !== undefined)
                    tokens.push(s);
            }
            this.lines.push(tokens);
        }
    }
    // Read a Lisp expression; return EndOfFile if the input runs out.
    Reader.prototype.read = function () {
        try {
            this.readToken();
            return this.parseExpression();
        }
        catch (ex) {
            if (ex instanceof FormatException)
                throw new EvalException("syntax error", ex.message + " at " + this.lineNo, false);
            else
                throw ex;
        }
    };
    Reader.prototype.parseExpression = function () {
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
    };
    Reader.prototype.parseListBody = function () {
        if (this.token == EndOfFile) {
            throw new FormatException('")" expected: ' + this.token);
        }
        if (this.token === rightParenSym) {
            return null;
        }
        else {
            var e1 = this.parseExpression();
            this.readToken();
            var e2 = void 0;
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
    };
    // Read the next token and set it to this.token.
    Reader.prototype.readToken = function () {
        if (this.lines.length === 0) {
            this.token = EndOfFile;
            return;
        }
        for (;;) {
            var t = this.lines[0].shift();
            if (t !== undefined) {
                if (t[0] === '"') {
                    var s = t;
                    var n_1 = s.length - 1;
                    if (n_1 < 1 || s[n_1] !== '"')
                        throw new FormatException("bad string: " + s);
                    s = s.substring(1, n_1);
                    s = s.replace(/\\./g, function (m) {
                        var val = Reader.escapes[m];
                        return (val === undefined) ? m : val;
                    });
                    this.token = s;
                    return;
                }
                var n = Number(t);
                if (!isNaN(n))
                    this.token = n;
                else if (t === "nil")
                    this.token = null;
                else if (t === "t")
                    this.token = true;
                else
                    this.token = newSym(t);
                return;
            }
            // If the line runs out
            this.lines.shift();
            if (this.lines.length === 0) {
                this.token = EndOfFile;
                return;
            }
            this.lineNo += 1;
        }
    };
    Reader.escapes = {
        "\\\\": "\\",
        '\\"': '"',
        "\\n": "\n", "\\r": "\r", "\\f": "\f",
        "\\b": "\b", "\\t": "\t", "\\v": "\v"
    };
    return Reader;
}());
//----------------------------------------------------------------------
// Mapping from a quote symbol to its string representation
var quotes = (_a = {},
    _a[quoteSym.name] = "'",
    _a[quasiquoteSym.name] = "`",
    _a[unquoteSym.name] = ",",
    _a[unquoteSplicingSym.name] = ",@",
    _a);
// Make a string representation of Lisp expression
function str(x, quoteString, count, printed) {
    if (quoteString === void 0) { quoteString = true; }
    if (x === null) {
        return "nil";
    }
    else if (x === true) {
        return "t";
    }
    else if (x instanceof Cell) {
        var c = x;
        if (c.car instanceof Sym) {
            var q = quotes[c.car.name];
            if (q !== undefined && c.cdr instanceof Cell)
                if (c.cdr.cdr == null)
                    return q + str(c.cdr.car, true, count, printed);
        }
        return "(" + strListBody(x, count, printed) + ")";
    }
    else if (typeof x === "string") {
        if (!quoteString)
            return x;
        var bf = ['"'];
        for (var _i = 0, x_1 = x; _i < x_1.length; _i++) {
            var ch = x_1[_i];
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
        var s = x.map(function (e) { return str(e, true, count, printed); }).join(", ");
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
    var s = [];
    var y;
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
        var i = printed.indexOf(y);
        if (i >= 0)
            printed.splice(i, 1);
    }
    return s.join(" ");
}
//----------------------------------------------------------------------
// Evaluate a string as a list of Lisp exps; return the result of the last exp.
function run(interp, input) {
    var r = new Reader(input);
    var result;
    for (;;) {
        var e = r.read();
        if (e === EndOfFile)
            return result;
        result = interp.eval(e, null);
    }
}
// Lisp initialization script
var prelude = "\n(setq defmacro\n      (macro (name args &rest body)\n             `(progn (setq ,name (macro ,args ,@body))\n                     ',name)))\n\n(defmacro defun (name args &rest body)\n  `(progn (setq ,name (lambda ,args ,@body))\n          ',name))\n\n(defun caar (x) (car (car x)))\n(defun cadr (x) (car (cdr x)))\n(defun cdar (x) (cdr (car x)))\n(defun cddr (x) (cdr (cdr x)))\n(defun caaar (x) (car (car (car x))))\n(defun caadr (x) (car (car (cdr x))))\n(defun cadar (x) (car (cdr (car x))))\n(defun caddr (x) (car (cdr (cdr x))))\n(defun cdaar (x) (cdr (car (car x))))\n(defun cdadr (x) (cdr (car (cdr x))))\n(defun cddar (x) (cdr (cdr (car x))))\n(defun cdddr (x) (cdr (cdr (cdr x))))\n(defun not (x) (eq x nil))\n(defun consp (x) (not (atom x)))\n(defun print (x) (prin1 x) (terpri) x)\n(defun identity (x) x)\n\n(setq\n = eql\n null not\n setcar rplaca\n setcdr rplacd)\n\n(defun > (x y) (< y x))\n(defun >= (x y) (not (< x y)))\n(defun <= (x y) (not (< y x)))\n(defun /= (x y) (not (= x y)))\n\n(defun equal (x y)\n  (cond ((atom x) (eql x y))\n        ((atom y) nil)\n        ((equal (car x) (car y)) (equal (cdr x) (cdr y)))))\n\n(defmacro if (test then &rest else)\n  `(cond (,test ,then)\n         ,@(cond (else `((t ,@else))))))\n\n(defmacro when (test &rest body)\n  `(cond (,test ,@body)))\n\n(defmacro let (args &rest body)\n  ((lambda (vars vals)\n     (defun vars (x)\n       (cond (x (cons (if (atom (car x))\n                          (car x)\n                        (caar x))\n                      (vars (cdr x))))))\n     (defun vals (x)\n       (cond (x (cons (if (atom (car x))\n                          nil\n                        (cadar x))\n                      (vals (cdr x))))))\n     `((lambda ,(vars args) ,@body) ,@(vals args)))\n   nil nil))\n\n(defmacro letrec (args &rest body)      ; (letrec ((v e) ...) body...)\n  (let (vars setqs)\n    (defun vars (x)\n      (cond (x (cons (caar x)\n                     (vars (cdr x))))))\n    (defun sets (x)\n      (cond (x (cons `(setq ,(caar x) ,(cadar x))\n                     (sets (cdr x))))))\n    `(let ,(vars args) ,@(sets args) ,@body)))\n\n(defun _append (x y)\n  (if (null x)\n      y\n    (cons (car x) (_append (cdr x) y))))\n(defmacro append (x &rest y)\n  (if (null y)\n      x\n    `(_append ,x (append ,@y))))\n\n(defmacro and (x &rest y)\n  (if (null y)\n      x\n    `(cond (,x (and ,@y)))))\n\n(defun mapcar (f x)\n  (and x (cons (f (car x)) (mapcar f (cdr x)))))\n\n(defmacro or (x &rest y)\n  (if (null y)\n      x\n    `(cond (,x)\n           ((or ,@y)))))\n\n(defun listp (x)\n  (or (null x) (consp x)))    ; NB (listp (lambda (x) (+ x 1))) => nil\n\n(defun memq (key x)\n  (cond ((null x) nil)\n        ((eq key (car x)) x)\n        (t (memq key (cdr x)))))\n\n(defun member (key x)\n  (cond ((null x) nil)\n        ((equal key (car x)) x)\n        (t (member key (cdr x)))))\n\n(defun assq (key alist)\n  (cond (alist (let ((e (car alist)))\n                 (if (and (consp e) (eq key (car e)))\n                     e\n                   (assq key (cdr alist)))))))\n\n(defun assoc (key alist)\n  (cond (alist (let ((e (car alist)))\n                 (if (and (consp e) (equal key (car e)))\n                     e\n                   (assoc key (cdr alist)))))))\n\n(defun _nreverse (x prev)\n  (let ((next (cdr x)))\n    (setcdr x prev)\n    (if (null next)\n        x\n      (_nreverse next x))))\n(defun nreverse (list)        ; (nreverse (quote (a b c d))) => (d c b a))\n  (cond (list (_nreverse list nil))))\n\n(defun last (list)\n  (if (atom (cdr list))\n      list\n    (last (cdr list))))\n\n(defun nconc (&rest lists)\n  (if (null (cdr lists))\n      (car lists)\n    (if (null (car lists))\n        (apply nconc (cdr lists))\n      (setcdr (last (car lists))\n              (apply nconc (cdr lists)))\n      (car lists))))\n\n(defmacro while (test &rest body)\n  (let ((loop (gensym)))\n    `(letrec ((,loop (lambda () (cond (,test ,@body (,loop))))))\n       (,loop))))\n\n(defmacro dolist (spec &rest body) ; (dolist (name list [result]) body...)\n  (let ((name (car spec))\n        (list (gensym)))\n    `(let (,name\n           (,list ,(cadr spec)))\n       (while ,list\n         (setq ,name (car ,list))\n         ,@body\n         (setq ,list (cdr ,list)))\n       ,@(if (cddr spec)\n             `((setq ,name nil)\n               ,(caddr spec))))))\n\n(defmacro dotimes (spec &rest body) ; (dotimes (name count [result]) body...)\n  (let ((name (car spec))\n        (count (gensym)))\n    `(let ((,name 0)\n           (,count ,(cadr spec)))\n       (while (< ,name ,count)\n         ,@body\n         (setq ,name (+ ,name 1)))\n       ,@(if (cddr spec)\n             `(,(caddr spec))))))\n";
if (typeof process !== "undefined" && typeof require !== "undefined") {
    write = function (s) { return process.stdout.write(s); };
    exit = process.exit;
    // Run interactively in UTF-8 for no argument or any "-" argument.
    // Run each as a script file in UTF-8 in order for other arguments.
    // XXX An exp must be written in one line when running interactively.
    var argv = process.argv;
    if (argv.length <= 2)
        argv = [undefined, undefined, "-"];
    var stdin = undefined;
    var fs = undefined;
    var interp_1 = new Interp();
    run(interp_1, prelude);
    for (var i = 2; i < argv.length; i++) {
        var fileName = argv[i];
        if (fileName == "-") {
            if (stdin === undefined) {
                stdin = process.openStdin();
                stdin.setEncoding("utf8");
                write("> ");
                stdin.on("data", function (input) {
                    try {
                        var r = run(interp_1, input);
                        write(str(r));
                    }
                    catch (ex) {
                        if (ex instanceof EvalException)
                            write("\n" + ex);
                        else
                            throw ex;
                    }
                    write("\n> ");
                });
                stdin.on("end", function () {
                    write("Goodbye\n");
                });
            }
        }
        else {
            fs = fs || require("fs");
            var text = fs.readFileSync(fileName, "utf8");
            run(interp_1, text);
        }
    }
}
var _a;
