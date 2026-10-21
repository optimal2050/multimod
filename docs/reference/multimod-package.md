# multimod: Define and Render Mathematical Models Across Modeling Languages

Represents a linear optimization model as an abstract syntax tree of
sets, mappings, parameters, variables and equations, independent of any
modeling language. Models are built in R or read from 'GAMS' and 'GNU
MathProg' source, rendered to 'GAMS', 'GNU MathProg', 'JuMP', 'Pyomo'
and 'LaTeX', or assembled into a linear programming (LP) matrix and
solved in R with 'HiGHS'. Models can be validated, reduced by folding
repeated parameter values and trimming unused symbols, stored as data
workspaces and exchanged as JSON, and solutions are read back into R.
Linear programs with continuous variables are the tested class; integer
and binary variables are carried in the model and written by the 'JuMP'
and 'Pyomo' writers, and nonlinear expressions have no tested route to a
solver.

## See also

Useful links:

- <https://optimal2050.github.io/multimod/>

- <https://github.com/optimal2050/multimod>

- Report bugs at <https://github.com/optimal2050/multimod/issues>

## Author

**Maintainer**: Oleg Lugovoy <olugovoy@optimalsolution.dev>
([ORCID](https://orcid.org/0000-0001-5546-9875))

Authors:

- Oleg Lugovoy <olugovoy@optimalsolution.dev>
  ([ORCID](https://orcid.org/0000-0001-5546-9875))
