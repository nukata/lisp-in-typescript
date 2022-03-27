# Lisp in TypeScript

This is a Lisp interpreter compatible with
[lisp-in-dart](https://github.com/nukata/lisp-in-dart),
[lisp-in-cs](https://github.com/nukata/lisp-in-cs) and
[lisp-in-go](https://github.com/nukata/lisp-in-go).

I first wrote it six years ago (2016|H28) in TypeScript 1.7 and revised it slightly
one year later (2017|H29) in TypeScript 2.2.
It had been presented under the MIT License at
<http://www.oki-osk.jp/esc/typescript/lisp-en.html> (broken link)
until the spring of 2017|H29.
I made the repository in GitHub four years ago (2018|H30).

Three yeares ago (2019|R1) I made use of `BigInt` if possible.
If you run it on a recent Node.js, you will enjoy infinite-precision integers.
I also revised REPL.

Now (2022|R4) I revised it again to make it run with Deno and recent Safari.
Note that `BigInt("+")` returns `0n` on the current Safari.

Just as lisp-in-dart, lisp-in-cs and lisp-in-go,
this is a Lisp-1 with TCO (tail call optimization)
and partially hygienic macros but being a subset of Common Lisp
in a loose meaning.
It is easy to write a nontrivial script which runs both in this and in
Common Lisp.
Examples are found in 
[lisp-in-dart/examples](https://github.com/nukata/lisp-in-dart/tree/master/examples).


## How to run

Compile [`lisp.ts`](lisp.ts) with Deno to get `lisp.js`, or just
use [`examples/lisp.js`](examples/lisp.js), which I provided in the same way.

```
$ deno --version
deno 1.20.3 (release, x86_64-apple-darwin)
v8 10.0.139.6
typescript 4.6.2
$ deno bundle lisp.ts lisp.js
Check file:///Users/suzuki/tmp/lisp-in-typescript/lisp.ts
Bundle file:///Users/suzuki/tmp/lisp-in-typescript/lisp.ts
Emit "lisp.js" (39.84KB)
$ 
```

Run `lisp.js` with Node.js and Deno.

```
$ node --version
v17.8.0
$ node lisp.js
> (+ 5 6)
11
> `(a b ,(cons 'c 'd))
(a b (c . d))
> (list
  1
  2
  3)
(1 2 3)
> (defun fact (n)
     (if (= n 0)
         1
       (* n
          (fact (- n 1)) )))
fact
> (fact 100)
93326215443944152681699238856266700490715968264381621468592963895217599993229915
608941463976156518286253697920827223758251185210916864000000000000000000000000
> (dump)
(fact dotimes dolist while nconc last nreverse _nreverse assoc assq member memq
listp or mapcar and append _append letrec let when if equal /= <= >= > setcdr se
tcar null rem = identity print consp not cdddr cddar cdadr cdaar caddr cadar caa
dr caaar cddr cdar cadr caar defun defmacro *version* dump exit apply symbol-nam
e intern make-symbol gensym *gensym-counter* terpri princ prin1 truncate / - * +
 mod % < eql numberp stringp length rplacd rplaca list eq atom cons cdr car)
> *version*
(2.1 "TypeScript" "Nukata Lisp")
> (exit 0)
$
```

You can run it with Lisp script(s).
If you put a "`-`" after the scripts, it will
begin an interactive session after running the scripts.

```
$ cat examples/fib.l
(defun fib (n)
  (if (<= n 1)
      n
    (+ (fib (- n 1))
       (fib (- n 2)))))
$ deno run --allow-read lisp.js examples/fib.l -
> (fib 10)
55
> (fib 20)
6765
> (exit 0)
$ 
```

You can use `lisp.js` also from an HTML file to run the Lisp
interpreter on Web browsers.
See a simple example of [`exmples/run-on-web.html`](examples/run-on-web.html).

```
$ open examples/run-on-web.html
```


## License

This is under the MIT License.
