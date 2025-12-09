# Create a function call AST node

Constructs an AST node representing a function call with arguments. This
is similar to ast_func but specifically for function calls in
expressions.

## Usage

``` r
ast_call(name, args = list())
```

## Arguments

- name:

  Character string, function name

- args:

  List of AST nodes representing arguments

## Value

An object of class `ast` and `call`
