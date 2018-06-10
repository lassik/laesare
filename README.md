# (laesare reader)

[![Build Status](https://travis-ci.org/weinholt/laesare.svg?branch=master)](https://travis-ci.org/weinholt/laesare)

This is an R6RS Scheme library that provides a reader with some extra
features not found in the standard `read` procedure:

* Compatible mode with support for other RnRS standards.
* Tolerant mode that continues on errors.
* Lexer that returns source code tokens, including whitespace.
* Source annotations, initially designed to be compatible-ish with
  psyntax.

*Läsare* means *reader* in the Swedish language.

## API

```Scheme
(import (läsare reader))
```

### (make-reader port filename)

Return a new reader object for the textual input *port* associated
with *filename* (which is used in annotation objects).

### (get-token reader)

Reads a token and returns it as two values.

### (read-datum reader)

Read a datum from the *reader*, returning it as a standard Scheme
object.

### (read-annotated reader)

The same as `read-datum`, except it returns an `annotation` object.
See the procedures below.

### (reader-mode reader)

Get the current reader mode. These modes exist:

* `rnrs` -- any RnRS syntax is allowed, with reasonable compromises
* `r6rs` -- only R6RS syntax is allowed
* `r7rs` -- only R7RS syntax is allowed

The reader mode will automatically change if the reader encounters
`#!r6rs` or `#!r7rs` (the latter is not standard, but appears here and
there).

The default mode is `rnrs`.

### (reader-mode-set! reader mode)

Changes the *reader* mode to *mode*. See `reader-mode`.

### (reader-fold-case? reader)

Returns `#t` if the reader is in *case folding* mode. A case folding
reader will do `string-foldcase` on identifiers.

This mode will automatically change if the reader encounters
`#!fold-case` or `#!no-fold-case`.

### (reader-fold-case?-set! reader bool)

Sets the case folding mode of the *reader*.

### (reader-tolerant? reader)

Returns `#t` if the reader is in *tolerant* mode. A tolerant reader
raises continuable warnings when it encounters syntax errors. If the
exception handler returns, then the reader ignores the error and goes
on parsing using some unspecified replacement tokens.

```Scheme
(with-exception-handler
  (lambda (con)
    (unless (warning? con)
      (raise con)))
  (lambda ()
    (read-datum reader)))
```

### (reader-tolerant?-set! reader bool)

Sets the *tolerant* mode of the *reader*.

### (reader-warning reader msg . irritants)

Helper that raises a reader warning for the current position of the
*reader*. Usually not needed, but can be useful for programs that do
their own processing of tokens.

### (reader-line reader)

The current line number of the *reader*'s port.

### (reader-column reader)

The current column number of the *reader*'s port.

### (annotation? obj)

Returns `#t` if *obj* is an annotation object; otherwise `#f`.

### (annotation-expression annotation)

The datum contained in the annotation. Datums inside the expression
are also, in turn, annotatation objects.

### (annotation-stripped annotation)

The datum in the annotation, recursively stripped of all annotations.

### (annotation-source annotation)

The source object of the annotation, showing the datum's position in
the reader.

### (annotation-source->condition annotation)

Converts the source object to a *&source-information* condition.
Usually not needed but provided for the same type of programs that
would need `reader-warning`.

### (source-condition? obj)

Returns `#t` if *obj* is a source condition; otherwise `#f`.

### (source-filename condition)

Returns the filename field of the source *condition*

### (source-line condition)

Returns the line number field of the source *condition*

### (source-column condition)

Returns the column number field of the source *condition*

### (detect-scheme-file-type port)

Attempts to guess the type of Scheme source code that lurks behind
*port*. It does this by reading a few of the initial lexemes. The
peculiarities are:

* If the first lexeme is a shebang (e.g. `#!/`) then the type is
  `r6rs-program`.
* If the port starts with a `library` form then the type is
  `r6rs-library`.
* If the port starts with an `import` form then the type is
  `r6rs-program`. This should later be improved to detect
  `r7rs-program`.
* If the port starts with a `define-library` form then the type is
  `r7rs-library`.

In other cases the type is `unknown`.

# Restrictions, bugs

Some R6RS syntax may slip through in the R7RS mode and some R7RS
syntax is not yet implemented. Please report bugs in GitHub issues.
