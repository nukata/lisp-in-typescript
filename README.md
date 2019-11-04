# Lisp in TypeScript

This is a Lisp interpreter compatible with
[lisp-in-dart](https://github.com/nukata/lisp-in-dart) and
[lisp-in-go](https://github.com/nukata/lisp-in-go) except for REPL:
an expression must be written in one line at REPL.

I first wrote it three years ago (2016 or H28) in TypeScript 1.7 and revised it slightly
one year later (2017 or H29) in TypeScript 2.2.
It had been presented under the MIT License at
<http://www.oki-osk.jp/esc/typescript/lisp-en.html> (broken link)
until the spring of 2017 (H29).
I made the repository in GitHub last year (2018 or H30).

Now (2019 or R1) I made use of `BigInt` if possible.
If you run it on a recent Node.js, you will enjoy infinite-precision integers.

Just as lisp-in-dart and lisp-in-go,
this is a Lisp-1 with TCO (tail call optimization)
and partially hygienic macros but being a subset of Common Lisp
in a loose meaning.
It is easy to write a nontrivial script which runs both in this and in
Common Lisp.
Examples are found in 
[lisp-in-dart/examples](http://github.com/nukata/lisp-in-dart/tree/master/examples).


## How to run

Use TypeScript 3.6 or later to compile [`lisp.ts`](lisp.ts) into `lisp.js`.

```
$ tsc -t ESNext --outFile list.js lisp.ts
$ node lisp.js
> (+ 5 6)
11
> *version*
(1.9 "TypeScript" "Nukata Lisp")
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
