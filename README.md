# Lisp in TypeScript

This is a Lisp interpreter compatible with
[lisp-in-dart](https://github.com/nukata/lisp-in-dart)
except for numeric types and REPL:
numbers are limited to _the Number type_ of ECMAScript
and an expression must be written in one line at REPL.
I wrote it two years ago (2016 or H28) in TypeScript 1.7 and revised slightly
one year later (2017 or H29) in TypeScript 2.2.
It had been presented under the MIT License at
<http://www.oki-osk.jp/esc/typescript/lisp-en.html> (broken link)
until last spring (2017 or H29).

Just as lisp-in-dart and [lisp-in-go](https://github.com/nukata/lisp-in-go), 
this is a Lisp-1 with TCO (tail call optimization)
and partially hygienic macros but being a subset of Common Lisp
in a loose meaning.
It is easy to write a nontrivial script which runs both in this and in
Common Lisp.
Examples are found in 
[lisp-in-dart/examples](http://github.com/nukata/lisp-in-dart/tree/master/examples).


## How to run

Use TypeScript 2.2 or later to compile [`lisp.ts`](lisp.ts) into `lisp.js`.

```
$ tsc -t ES5 lisp.ts
$ node lisp.js
> (+ 5 6)
11
> *version*
(1.27 "TypeScript" "Nukata Lisp Light")
> (exit 0)
$
```

You can give `lisp.js` a file name of your Lisp script.
If you put a "`-`" after the file name, it will
begin an interactive session after running the file.

```
$ cat examples/fib.l
(defun fib (n)
  (if (<= n 1)
      n
    (+ (fib (- n 1))
       (fib (- n 2)))))
$ node lisp.js examples/fib.l -
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
See [`lisp.ts L1370-L1390`](lisp.ts#L1370-L1390).
