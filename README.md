# Reazon -- miniKanren for Emacs

Reazon is an implementation of **_miniKanren_**, a small
domain-specific logic programming language. Whereas languages like
Elisp deal with *functions* that take inputs and yield outputs,
miniKanren deals with sets of values that satisfy *relations*. Every
function is a relation, but not vice versa, since a relation might
include the output of a function but nots its inputs. In such a case,
miniKanren would attempt to find inputs yielding the output,
effectively running the function backwards.

### That's vague. How about a concrete example?

Great idea. Consider this simple recursive definition for the function
`append` (actually, we'll call it `_append` so as to avoid clobbering
the builtin `append`):

```elisp
(defun _append (head tail)
  (cond
   ((null head) tail)
   (t (cons (car head)
            (_append (cdr head) tail)))))
```

We take two input lists, and we expect to get back a list consisting
of the elements of the first list followed by the elements of the
second (we'll ignore the case where the inputs aren't proper lists).

```
> (_append '(1 2 3) '(4 5 6))
(1 2 3 4 5 6)

> (_append '() '(4 5 6))
(4 5 6)

> (_append '(1 2 3) '())
(1 2 3)
```

In miniKanren, we'll instead define a relation with three inputs: two
corresponding to the inputs of the `append` function, and one
corresponing to its output.

```elisp
(reazon-defrel appendo (head tail out)
  (reazon-conde
   ((reazon-== head '()) (reazon-== out tail))
   ((reazon-fresh (a d rec)
      (reazon-== (cons a d) head)
      (reazon-== out (cons a rec))
      (appendo d tail rec)))))
```

Since the `appendo` relation strictly includes the `append** function,
it can be run with the same inputs to get the same outputs.

**NB: Reazon _will not work_ without `lexical-binding`, so make sure
to turn it on before running these examples!**
```elisp
> (setq lexical-binding t) ;; Make sure to turn this on!
t
```

```elisp
> (reazon-run* q
    (appendo '(1 2 3) '(4 5 6) q))
((1 2 3 4 5 6))

> (reazon-run* q
    (appendo '(1 2 3) '() q))
((1 2 3))

> (reazon-run* q
    (appendo '() '(4 5 6) q))
((4 5 6))
```

### That's boring. Why would I need a "relation" to do that?

You wouldn't. But with a relation, we can move around the query
variable to search for inputs yielding the given output.

```elisp
> (reazon-run* q
    (appendo q '(4 5 6) '(1 2 3 4 5 6)))
((1 2 3))

> (reazon-run* q
    (appendo '(1 2 3) q '(1 2 3 4 5 6)))
((4 5 6))
```

In fact, we don't need to give any inputs at all. The following query
searches for all pairs of inputs that `append` to form `'(1 2 3 4 5
6)`:

```elisp
> (reazon-run* (p q)
    (appendo p q '(1 2 3 4 5 6)))
((nil
  (1 2 3 4 5 6))
 ((1)
  (2 3 4 5 6))
 ((1 2)
  (3 4 5 6))
 ((1 2 3)
  (4 5 6))
 ((1 2 3 4)
  (5 6))
 ((1 2 3 4 5)
  (6))
 ((1 2 3 4 5 6)
  nil))
```

### Not bad, but appending lists together isn't a very exciting application.

Consider the function `eval`, commonly found in languages like Python
and Javascript. It takes as input an expression and returns as output
the value to which that expression evaluates. The relational
equivalent of `eval` would be the relation `evalo`, which would
associate expressions with values. Supposing we had such a relation,
what would we get from the querying the following?

```elisp
(reazon-run 1 q
  (evalo q q))
```

That's right, we would get a *quine*, that is, an expression that
evaluates to itself! And supposing we had an operator `=/=` to
stipulate that two expressions are distinct, what would we get from
running this?

```elisp
(reazon-run 1 (p q)
  (=/= p q)
  (evalo p q)
  (evalo q p))
```

Right, we would get two *twines*, or distinct expressions that
evaluate to each other.

### Wow, Reazon can do all that?

No. Currently Reazon lacks the ability to constrain values, or really
to handle negation at all. But PRs are welcome!

## FAQ

### Did you come up with miniKanren all by yourself?

No. miniKanren is walked through in detail and implemented in the book
*The Reasoned Schemer* (a sequel to *The Little Schemer*) by Byrd,
Friedman, Kiselyov, and Hemann. Reazon is a straightforward adaptation
to Elisp of the code implementation from the second edition.

### So you just copied some code out of a book?

That's a little reductive, but yeah, pretty much. There were some
gotchas though, both minor and major. The big sticking point, of
course, was rewriting the Scheme macros using `defmacro`. The
difficulty was compounded by the fact that I don't understand how
Scheme macros work.

### I ran a query, but I got `*** Eval error ***  Lisp nesting exceeds ‘max-lisp-eval-depth’`.

The interface operator `reazon-run*` searches for as many solutions as
it can find. If your query has infinitely many solutions, it will keep
searching until it blows the stack. For instance, there are infinitely
many triples `x, y, z` such that `x` and `y` append to form `z`:

```elisp
> (reazon-run* (x y z)
    (appendo x y z))
*** Eval error ***  Lisp nesting exceeds ‘max-lisp-eval-depth’
```

Try using `reazon-run` (no asterisk) with a count to limit the search:

```elisp
> (reazon-run 3 (x y z)
    (appendo x y z))
((nil _0 _0)
 ((_0)
  _1
  (_0 . _1))
 ((_0 _1)
  _2
  (_0 _1 . _2)))
```

### It's a pain in the ass to use the `reazon-` namespace prefix all the time, espcially for an operator like `reazon-==`.

I know. I'm working on it.

### How does this compare to [core.logic](https://github.com/clojure/core.logic)?

`core.logic` is a Clojure implementation of miniKanren. It's
significantly more developed than Reazon (I mean, like way more
developed), but they are the same in spirit. If you need to use
miniKanren for something serious, use that, not this.

### I defined a relation and ran a query, but I got `*** Eval error ***  Symbol’s value as variable is void: x`. I double-checked and I'm sure I wrote the definition correctly.

Reazon relations need to be defined in lexical environments. This can
be set in an interpreter like `ielm` by running `(setq lexical-binding
t)` or by adding `;; -*- lexical-binding: t; -*-` to the top of a
source file.

I learned this the hard way.

```
> (reazon-defrel _five (x) (reazon-== x 5))
_five
> (reazon-run* q (_five q))
*** Eval error ***  Symbol’s value as variable is void: x
> (setq lexical-binding t)
t
> (reazon-run* q (_five q))
*** Eval error ***  Symbol’s value as variable is void: x
> (reazon-defrel _five (x) (reazon-== x 5))
_five
> (reazon-run* q (_five q))
(5)
```

### Why does "Reazon" have a "z" in it? That's stupid.

Maybe. Initially it was just called "Reason" (with an "s") after *The
Reasoned Schemer*, but then I discovered to my dismay that there is
already a package called "Reason" on Melpa (a major mode for some
language). Changing the "s" to "z" seemed like a fine way around that
roadblock, with the added benefit of making the name "pop".
