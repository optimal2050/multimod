# Write an equation as MathProg, GAMS or JuMP text

Equations are the part of a model that is hardest to read as nested
constructors: a cost balance takes several levels of
[`ast_expression()`](https://optimal2050.github.io/multimod/r/reference/ast_expression.md).
`mm_equation()` takes the statement in modeling-language notation and
parses it into the same AST the constructors build.

## Usage

``` r
mm_equation(model, text, desc = "", lang = c("gmpl", "gams", "jump"))
```

## Arguments

- model:

  A multimod model. Its declarations resolve the names used in `text`,
  so every set, parameter and variable the equation refers to must
  already be in it.

- text:

  One equation statement. For `lang = "gmpl"` this is MathProg,
  `name{indexing}: lhs <op> rhs` with an optional `s.t.` prefix and
  trailing `;`, where `<op>` is `<=`, `>=` or `=`. For `lang = "gams"`
  it is `name(dims)$domain(dims) .. lhs <op> rhs` with `=l=`, `=g=` or
  `=e=`. For `lang = "jump"` it is one
  `@constraint(model, name[i in S], lhs <op> rhs)` statement, with `==`
  for equality. Newlines are allowed in all three.

- desc:

  Description to attach to the equation.

- lang:

  The notation `text` is written in. MathProg is the default: it is an
  open language and uses the ordinary relational operators. JuMP and
  GAMS are read as well; all three produce the same AST.

## Value

An equation node, ready to assign into `model$equations`.

## Details

Declarations stay in R.
[`new_set()`](https://optimal2050.github.io/multimod/r/reference/new_set.md),
[`new_parameter()`](https://optimal2050.github.io/multimod/r/reference/new_parameter.md)
and
[`new_variable()`](https://optimal2050.github.io/multimod/r/reference/new_variable.md)
are flat and read well; only the expression needs the notation.

## See also

[`new_equation()`](https://optimal2050.github.io/multimod/r/reference/new_equation.md)
builds the same object from constructors, which is the route for an
equation that is generated rather than written.

## Examples

``` r
m <- example_models$toy

# MathProg (default)
eq <- mm_equation(
  m,
  "eqCapLim{(tech,region) in mTR}:
     vGen[tech,region] <= pAvail[tech,region] * vCap[tech,region]",
  desc = "generation is limited by available capacity"
)
cat(as_latex(eq))
#> \begin{flushleft}
#> \subsection*{\textbf{eqCapLim} -- \textit{generation is limited by available capacity}} 
#> \quad$\textbf{eqCapLim} 
#> _{\texttt{tech},\texttt{region}}$ 
#> $\mid\left\{\texttt{tech},\texttt{region}\right\} \in \mathit{mTR}_{\texttt{tech},\texttt{region}}$ \\
#> \begin{equation*}
#> \begin{adjustbox}{max width=\textwidth}
#> $\begin{aligned}
#> &\bm{\mathit{vGen}}_{\text{tech},\text{region}} \le \mathsf{pAvail}_{\text{tech},\text{region}}  \cdot  \bm{\mathit{vCap}}_{\text{tech},\text{region}} \\
#> \end{aligned}$
#> \end{adjustbox}
#> \end{equation*}
#> \vspace{1em}
#> \end{flushleft}

# the same equation in GAMS
eq2 <- mm_equation(
  m,
  "eqCapLim(tech,region)$mTR(tech,region) ..
     vGen(tech,region) =l= pAvail(tech,region) * vCap(tech,region)",
  lang = "gams"
)

# and in JuMP
eq3 <- mm_equation(
  m,
  "@constraint(model, eqCapLim[(h, r) in mTR],
     vGen[(h, r)] <= get(pAvail, (h, r), pAvailDef) * vCap[(h, r)])",
  lang = "jump"
)
```
