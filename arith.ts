// A little arithmetic in TypeScript 3.7 by SUZUKI Hisao (R01.08.04/R01.11.09)
//  derived from arith.ts at github.com/nukata/little-scheme-in-typescript

'use strict'

type Numeric = number | bigint;

// A Number value is treated as an inexact number.
// A BigInt value is treated as an exact number.
// Any intergers should be represented by BigInt if possible.
// If the runtime does not have BigInt, arithmetic will be done with Number.

const ZERO = (typeof BigInt === 'undefined') ? 0 : BigInt(0);
const ONE = (typeof BigInt === 'undefined') ? 1 : BigInt(1);

// Is x a Numeric?
function isNumeric(x: unknown): x is Numeric {
    let t = typeof x;
    return t === 'number' || t === 'bigint';
}

// x + y
function add(x: Numeric, y: Numeric): Numeric {
    if (typeof x === 'number') {
        if (typeof y === 'number')
            return x + y;
        else
            return x + Number(y);
    } else {
        if (typeof y === 'number')
            return Number(x) + y;
        else
            return x + y;
    }
}

// x - y
function subtract(x: Numeric, y: Numeric): Numeric {
    if (typeof x === 'number') {
        if (typeof y === 'number')
            return x - y;
        else
            return x - Number(y);
    } else {
        if (typeof y === 'number')
            return Number(x) - y;
        else
            return x - y;
    }
}

// x * y
function multiply(x: Numeric, y: Numeric): Numeric {
    if (typeof x === 'number') {
        if (typeof y === 'number')
            return x * y;
        else
            return x * Number(y);
    } else {
        if (typeof y === 'number')
            return Number(x) * y;
        else
            return x * y;
    }
}

// x / y (rounded quotient)
function divide(x: Numeric, y: Numeric): Numeric {
    return Number(x) / Number(y);
}

// Calculate the quotient of x and y.
function quotient(x: Numeric, y: Numeric): Numeric {
    if (typeof x === 'number' || typeof y === 'number') {
        let q = Math.trunc(Number(x) / Number(y));
        if (typeof BigInt === 'undefined')
            return q;
        else
            return BigInt(q);
    } else {
        return x / y;
    }
}

// Calculate the remainder of the quotient of x and y.
function remainder(x: Numeric, y: Numeric): Numeric {
    if (typeof x === 'number' || typeof y === 'number')
        return Number(x) % Number(y);
    else
        return x % y;
}

// Compare x and y.
// -1, 0 or 1 as x is less than, equal to, or greater than y.
function compare(x: Numeric, y: Numeric): number {
    if (typeof x === 'number') {
        if (typeof y === 'number')
            return Math.sign(x - y);
        else
            return Math.sign(x - Number(y));
    } else {
        if (typeof y === 'number')
            return Math.sign(Number(x) - y);
        else
            return (x < y) ? -1 : (y < x) ? 1 : 0;
    }
}

// Try to parse the token as a Numeric or null.
function tryToParse(token: string): Numeric | null {
    try {
        return BigInt(token);
    } catch (ex) {
        const n = Number(token);
        if (isNaN(n))
            return null;
        return n;
    }
}

// Convert x to string.
function convertToString(x: Numeric): string {
    let s = x + '';
    if (typeof BigInt !== 'undefined')
        if (typeof x === 'number')
            if (Number.isInteger(x) && !s.includes('e'))
                return s + '.0';    // 123.0 => '123.0'
    return s;
}
