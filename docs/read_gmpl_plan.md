# Plan: Implementing read_gmpl() for GLPK/MathProg Parser

## Overview
Implement `read_gmpl()` function to parse GLPK/MathProg (GNU MathProg) model files into the multimod AST structure. This will complement the existing `read_gams()` function.

## Key Differences: MathProg vs GAMS Syntax

### 1. **Set Declarations**
- **MathProg**: `set YEAR;` (simple)
- **GAMS**: `set YEAR;` or `sets YEAR;` (similar but GAMS allows multiple sets in one declaration)

### 2. **Parameter Declarations**
- **MathProg**: 
  - Simple: `param OperationalLife{r in REGION, t in TECHNOLOGY};`
  - With default: `param DiscountRate{r in REGION}, default 0.05;`
  - With attributes: `param Conversionls{l in TIMESLICE, ls in SEASON} binary;`
  - With formula: `param DiscountFactor{r in REGION, y in YEAR} := (1 + DiscountRate[r]) ^ y;`
- **GAMS**: 
  - Simple: `parameter OperationalLife(r,t);`
  - Uses different syntax for defaults and calculations

### 3. **Variable Declarations**
- **MathProg**: 
  - `var RateOfDemand{r in REGION, l in TIMESLICE, f in FUEL, y in YEAR} >= 0;`
  - `var NumberOfNewTechnologyUnits{r in REGION, t in TECHNOLOGY, y in YEAR} >= 0, integer;`
- **GAMS**: 
  - `positive variable RateOfDemand(r,l,f,y);`
  - `integer variable NumberOfNewTechnologyUnits(r,t,y);`

### 4. **Constraint/Equation Declarations**
- **MathProg**: 
  ```
  s.t. EQ_SpecifiedDemand{r in REGION, l in TIMESLICE, f in FUEL, y in YEAR:
      SpecifiedAnnualDemand[r,f,y] <> 0}:
      SpecifiedAnnualDemand[r,f,y] * SpecifiedDemandProfile[r,f,l,y] / YearSplit[l,y]
      =
      RateOfDemand[r,l,f,y];
  ```
  - Uses `s.t.` (subject to) keyword
  - Colon-separated index declarations and conditions
  - Operators: `=`, `<=`, `>=`
  
- **GAMS**: 
  ```
  equation EQ_SpecifiedDemand(r,l,f,y);
  EQ_SpecifiedDemand(r,l,f,y)$SpecifiedAnnualDemand(r,f,y)..
      SpecifiedAnnualDemand(r,f,y) * SpecifiedDemandProfile(r,f,l,y) / YearSplit(l,y)
      =e=
      RateOfDemand(r,l,f,y);
  ```
  - Separate declaration and definition
  - Uses `$` for conditions
  - Operators: `=e=`, `=l=`, `=g=`

### 5. **Objective Function**
- **MathProg**: `minimize cost: sum{r in REGION, y in YEAR} TotalDiscountedCost[r,y];`
- **GAMS**: Uses equation with `objective` attribute

### 6. **Index Sets and Iteration**
- **MathProg**: `{r in REGION, l in TIMESLICE: condition}`
- **GAMS**: `(r,l)$condition`

### 7. **Aggregation Functions**
- **MathProg**: `sum{i in SET: condition} expr`, `min{i in SET} expr`, `max{i in SET} expr`
- **GAMS**: `sum((i)$condition, expr)`, `smin(i, expr)`, `smax(i, expr)`

### 8. **Array Indexing**
- **MathProg**: `Parameter[index1, index2]` (square brackets)
- **GAMS**: `Parameter(index1, index2)` (parentheses)

### 9. **Conditional Expressions**
- **MathProg**: `if condition then expr1 else expr2`
- **GAMS**: `$(condition) expr` or `$[condition] expr`

### 10. **Comments**
- **MathProg**: `#` for line comments
- **GAMS**: `*` or `$ontext...$offtext` for comments

## Implementation Strategy

### Phase 1: Core Structure Parsing
**Goal**: Parse basic model structure (sets, parameters, variables)

1. **Tokenizer/Lexer**
   - Handle `#` comments
   - Recognize keywords: `set`, `param`, `var`, `s.t.`, `minimize`, `maximize`
   - Handle curly braces `{}` for index sets
   - Handle square brackets `[]` for array access
   - Handle operators: `:=`, `>=`, `<=`, `<>`, `=`

2. **Set Parser**
   ```r
   parse_gmpl_set <- function(line)
   # Input: "set YEAR;"
   # Output: list(type="set", name="YEAR", dims=NULL)
   ```

3. **Parameter Parser**
   ```r
   parse_gmpl_param <- function(declaration)
   # Input: "param OperationalLife{r in REGION, t in TECHNOLOGY};"
   # Output: list(
   #   type="parameter",
   #   name="OperationalLife",
   #   dims=list(
   #     list(name="r", set="REGION"),
   #     list(name="t", set="TECHNOLOGY")
   #   ),
   #   default=NULL,
   #   formula=NULL
   # )
   ```

4. **Variable Parser**
   ```r
   parse_gmpl_var <- function(declaration)
   # Input: "var RateOfDemand{r in REGION, ...} >= 0;"
   # Output: list(
   #   type="variable",
   #   name="RateOfDemand",
   #   dims=[...],
   #   bounds=list(lower=0, upper=Inf),
   #   var_type="continuous"  # or "integer", "binary"
   # )
   ```

### Phase 2: Expression Parsing
**Goal**: Parse mathematical expressions in constraints and objective

1. **Index Set Expression Parser**
   ```r
   parse_index_set <- function(expr)
   # Input: "{r in REGION, l in TIMESLICE: SpecifiedAnnualDemand[r,f,y] <> 0}"
   # Output: list(
   #   indices=list(
   #     list(name="r", set="REGION"),
   #     list(name="l", set="TIMESLICE")
   #   ),
   #   condition="SpecifiedAnnualDemand[r,f,y] <> 0"
   # )
   ```

2. **Summation Parser**
   ```r
   parse_sum <- function(expr)
   # Input: "sum{m in MODE_OF_OPERATION} RateOfActivity[r,l,t,m,y]"
   # Output: list(
   #   type="sum",
   #   index_set=[...],
   #   body="RateOfActivity[r,l,t,m,y]"
   # )
   ```

3. **Array Access Parser**
   ```r
   parse_array_access <- function(expr)
   # Input: "CapacityFactor[r,t,l,y]"
   # Output: list(
   #   type="parameter_ref",
   #   name="CapacityFactor",
   #   indices=["r", "t", "l", "y"]
   # )
   ```

### Phase 3: Constraint Parsing
**Goal**: Parse complete constraint definitions

1. **Constraint Parser**
   ```r
   parse_gmpl_constraint <- function(declaration)
   # Input: "s.t. CAa4_Constraint_Capacity{r in REGION, ...}:
   #         RateOfTotalActivity[r,t,l,y] <= TotalCapacityAnnual[r,t,y] * ...;"
   # Output: list(
   #   type="constraint",
   #   name="CAa4_Constraint_Capacity",
   #   indices=[...],
   #   condition=NULL,  # or condition expression
   #   lhs="RateOfTotalActivity[r,t,l,y]",
   #   sense="<=",
   #   rhs="TotalCapacityAnnual[r,t,y] * ..."
   # )
   ```

2. **Objective Function Parser**
   ```r
   parse_gmpl_objective <- function(declaration)
   # Input: "minimize cost: sum{r in REGION, y in YEAR} TotalDiscountedCost[r,y];"
   # Output: list(
   #   type="objective",
   #   sense="minimize",  # or "maximize"
   #   name="cost",
   #   expr="sum{r in REGION, y in YEAR} TotalDiscountedCost[r,y]"
   # )
   ```

### Phase 4: Main Parser Function
**Goal**: Orchestrate parsing of entire file

```r
read_gmpl <- function(file_path, verbose = TRUE) {
  # 1. Read file and preprocess
  lines <- readLines(file_path)
  lines <- remove_comments(lines)
  lines <- normalize_whitespace(lines)
  
  # 2. Group multi-line declarations
  declarations <- group_declarations(lines)
  
  # 3. Initialize model structure
  model <- list(
    sets = list(),
    parameters = list(),
    variables = list(),
    constraints = list(),
    objective = NULL
  )
  
  # 4. Parse each declaration
  for (decl in declarations) {
    if (is_set_declaration(decl)) {
      model$sets <- c(model$sets, parse_gmpl_set(decl))
    } else if (is_param_declaration(decl)) {
      model$parameters <- c(model$parameters, parse_gmpl_param(decl))
    } else if (is_var_declaration(decl)) {
      model$variables <- c(model$variables, parse_gmpl_var(decl))
    } else if (is_constraint_declaration(decl)) {
      model$constraints <- c(model$constraints, parse_gmpl_constraint(decl))
    } else if (is_objective_declaration(decl)) {
      model$objective <- parse_gmpl_objective(decl)
    }
  }
  
  # 5. Convert to multimod structure
  convert_to_multimod_ast(model)
}
```

## Conversion to Multimod AST

After parsing, convert MathProg-specific constructs to multimod's generic AST:

1. **Index Sets**: `{r in REGION}` → `list(name="r", set="REGION")`
2. **Conditions**: `<>` → `!=`, handle in filter expressions
3. **Array Access**: `Param[i,j]` → already uses square brackets like Julia
4. **Aggregations**: Keep structure similar to GAMS sum/smin/smax

## Testing Strategy

1. **Unit Tests** - Test individual parsers:
   - `test_parse_gmpl_set()`
   - `test_parse_gmpl_param()`
   - `test_parse_gmpl_var()`
   - `test_parse_gmpl_constraint()`

2. **Integration Test** - Parse full OSeMOSYS MathProg file:
   ```r
   test_that("OSeMOSYS MathProg parses correctly", {
     model <- read_gmpl("path/to/osemosys.txt")
     expect_equal(length(model$sets), 11)
     expect_equal(length(model$parameters), 52)
     expect_equal(length(model$variables), 60+)
     expect_equal(length(model$constraints), 70+)
   })
   ```

3. **Comparison Test** - Compare with GAMS parser output:
   ```r
   test_that("MathProg and GAMS produce equivalent AST", {
     gmpl_model <- read_gmpl("osemosys.txt")
     gams_model <- read_gams("osemosys.gms")
     
     # Should produce same set of constraints
     expect_equal(
       sort(names(gmpl_model$constraints)),
       sort(names(gams_model$constraints))
     )
   })
   ```

## File Structure

Create new file: `R/read_gmpl.R` with functions:
- `read_gmpl()` - main entry point
- `remove_gmpl_comments()`
- `parse_gmpl_set()`
- `parse_gmpl_param()`
- `parse_gmpl_var()`
- `parse_gmpl_constraint()`
- `parse_gmpl_objective()`
- `parse_gmpl_index_set()`
- `parse_gmpl_expr()` - recursive expression parser

## Implementation Priority

### High Priority (Phase 1 - Week 1)
1. ✅ Document plan
2. [ ] Implement basic tokenizer/comment remover
3. [ ] Parse set declarations
4. [ ] Parse simple parameter declarations (no formulas)
5. [ ] Parse variable declarations with bounds

### Medium Priority (Phase 2 - Week 2)
6. [ ] Parse parameter formulas (`:=` expressions)
7. [ ] Parse index sets with conditions
8. [ ] Parse simple constraints (linear)
9. [ ] Basic expression parser (operators, array access)

### Lower Priority (Phase 3 - Week 3)
10. [ ] Parse complex constraints (with sum, min, max)
11. [ ] Parse objective function
12. [ ] Handle if-then-else expressions
13. [ ] Handle special MathProg functions (min, max, abs, etc.)
14. [ ] Full integration with existing multimod pipeline

## Success Criteria

1. ✅ Successfully parse all sets from osemosys.txt (11 sets)
2. ✅ Successfully parse all parameters (52+ parameters)
3. ✅ Successfully parse all variables (60+ variables)
4. ✅ Successfully parse all constraints (70+ constraints)
5. ✅ Generate JuMP code equivalent to GAMS-parsed model
6. ✅ Test model solves with same results as GAMS version

## Open Questions

1. **Calculated Parameters**: How to handle parameters defined with formulas?
   - Option A: Treat as derived/calculated parameters (not loaded from data)
   - Option B: Store formula in AST, evaluate during code generation
   - **Decision**: Store formula in AST for transparency

2. **Check Statements**: MathProg has `check` statements for validation
   - Option A: Parse and convert to assertions in Julia
   - Option B: Ignore during parsing
   - **Decision**: Parse and store, optionally generate as assertions

3. **Print Statements**: MathProg has `printf` for output
   - Option A: Parse and convert to Julia output code
   - Option B: Ignore
   - **Decision**: Ignore for now (focus on model structure)

## Next Steps

1. Create `R/read_gmpl.R` skeleton
2. Implement Phase 1 functions
3. Test on simple MathProg examples
4. Iterate based on osemosys.txt parsing results
5. Document any MathProg-specific quirks encountered

---

## GMPL/MathProg Language Reference

### AST Node Types Required

Based on multimod structure and GMPL/MathProg syntax:

#### 1. Declaration Nodes

**SetDecl**: Simple and indexed sets
- `name`: set identifier
- `indexing`: optional parent sets (e.g., `{i in SET}`)
- `attributes`: `dimen`, `within`, etc.

**ParamDecl**: Scalar and indexed parameters
- `name`: parameter identifier
- `indexing`: index domains (e.g., `{i in I, j in J}`)
- `default`: default value expression
- `formula`: assignment expression (`:=`)
- `attributes`: bounds (`>=`, `<=`), `binary`, etc.

**VarDecl**: Decision variables
- `name`: variable identifier
- `indexing`: index domains
- `bounds`: lower/upper bounds
- `type`: continuous (default), `integer`, `binary`

**ConstraintDecl**: Constraints (both `s.t.` and objectives)
- `name`: constraint identifier
- `indexing`: index domains
- `condition`: filtering condition (after `:`)
- `body`: expression tree
- `sense`: `<=`, `>=`, `=`, `minimize`, `maximize`

#### 2. Expression Nodes

**Literal**: Numeric constants, strings

**Reference**: Variable/parameter/set references with optional indexing

**BinaryOp**: `+`, `-`, `*`, `/`, `^`, `mod`, `div`, `<`, `>`, `<=`, `>=`, `=`, `<>`, `and`, `or`

**UnaryOp**: `-`, `+`, `not`

**IndexedExpr**: Sum, prod, min, max expressions with indexing

**ConditionalExpr**: `if-then-else` expressions

**SetExpr**: Set operations (`union`, `inter`, `diff`, `symdiff`, `cross`)

**FunctionCall**: Built-in function calls

#### 3. Indexing Nodes

**IndexingSet**: Domain specification
- `indices`: list of index variables
- `sets`: domains for each index
- `condition`: filtering predicate

---

### GMPL Built-in Functions

Based on AMPL/GMPL specifications (gmpl.pdf):

#### Arithmetic Functions
```
abs(x)          # Absolute value
ceil(x)         # Ceiling (smallest integer >= x)
floor(x)        # Floor (largest integer <= x)
exp(x)          # Exponential (e^x)
log(x)          # Natural logarithm
log10(x)        # Base-10 logarithm
sqrt(x)         # Square root
sin(x)          # Sine
cos(x)          # Cosine
tan(x)          # Tangent
asin(x)         # Arcsine
acos(x)         # Arccosine
atan(x)         # Arctangent
atan2(y,x)      # Two-argument arctangent
round(x)        # Round to nearest integer
round(x,n)      # Round to n decimal places
trunc(x)        # Truncate to integer
trunc(x,n)      # Truncate to n decimal places
```

#### Aggregation Functions (with indexing)
```
sum{indexing} expr      # Summation
prod{indexing} expr     # Product
min{indexing} expr      # Minimum
max{indexing} expr      # Maximum
```

#### Set Functions
```
card(S)                 # Cardinality (size of set)
setof{indexing} expr    # Build a set from expression
```

#### Logical Functions
```
forall{indexing} condition  # Universal quantifier (always true)
exists{indexing} condition  # Existential quantifier (any true)
```

#### String Functions
```
length(s)       # String length
substr(s,i,j)   # Substring from i to j
```

---

### GMPL Operators

#### Arithmetic Operators (precedence high to low)
```
^               # Exponentiation (right-associative)
- +             # Unary minus/plus
* / mod div     # Multiplication, division, modulo, integer division
+ -             # Addition, subtraction
```

#### Comparison Operators
```
<  >  <=  >=    # Less than, greater than, less/greater or equal
=  <>  !=       # Equal, not equal
```

#### Logical Operators
```
not             # Logical NOT
and             # Logical AND
or              # Logical OR
```

#### Set Operators
```
union           # Set union
diff            # Set difference
symdiff         # Symmetric difference
inter           # Set intersection
cross           # Cartesian product
within          # Subset test
not within      # Not subset test
in              # Membership test
not in          # Not membership test
```

---

### GMPL Keywords

```
set param var minimize maximize subject to s.t.
binary integer symbolic
default dimen within
for if then else
sum prod min max
forall exists setof card
union diff symdiff inter cross
and or not
in check solve display printf
end data
```

---

### GMPL Syntax Patterns

#### Indexing Expression Patterns
```
{i in SET}                          # Simple indexing
{i in SET, j in SET2}              # Multiple indices
{i in SET: condition}              # With condition
{(i,j) in SET}                     # Tuple indexing
{i in SET, j in SET2: i < j}      # Cross product with condition
```

#### Parameter Declaration Patterns
```
param name;                                    # Scalar
param name{i in SET};                         # Indexed by set
param name{i in I, j in J};                   # Multi-indexed
param name >= 0;                               # With bound
param name{i in SET} >= 0, <= 10;            # Indexed with bounds
param name{i in SET}, default 0;              # With default
param name{i in SET} := expr;                 # With formula
param name binary;                            # Binary attribute
```

#### Variable Declaration Patterns
```
var name;                                      # Unbounded continuous
var name >= 0;                                 # Lower bound
var name >= 0, <= 100;                        # Bounded
var name{i in SET} >= 0;                      # Indexed with bound
var name integer;                              # Integer variable
var name binary;                               # Binary variable
```

#### Constraint Declaration Patterns
```
s.t. name: expr1 <= expr2;                                    # Simple constraint
s.t. name{i in SET}: expr1[i] >= expr2[i];                   # Indexed constraint
s.t. name{i in SET: cond}: expr1 = expr2;                    # With condition
minimize obj: expr;                                           # Objective
maximize obj: expr;                                           # Objective
```

---

### Special GMPL Constructs

#### Conditional Expressions
```
if condition then expr1 else expr2
if condition then expr1                # else 0 implied for numeric
```

#### Set Construction
```
setof{i in SET: condition} (expr1, expr2)  # Build set of tuples
```

#### Dummy Index Pattern
```
min{yy in YEAR} min(yy)    # Uses dummy variable 'yy' to find minimum element
max{yy in YEAR} max(yy)    # Uses dummy variable 'yy' to find maximum element
```

This pattern is common in GMPL where aggregation functions (`min`, `max`) need both:
1. **Outer iterator**: `{yy in YEAR}` - iterates over the set
2. **Inner function**: `min(yy)` or `max(yy)` - extracts the value

---

### References

- **GLPK/GMPL official documentation**: gmpl.pdf in GLPK distribution (comprehensive reference)
- **AMPL Book**: Fourer, Gay, and Kernighan (2002) - Chapters 1-10 cover GMPL syntax
- **Wikibooks**: https://en.wikibooks.org/wiki/GLPK/GMPL_(MathProg)
- **OSeMOSYS MathProg**: osemosys.txt (1426 lines, comprehensive real-world example)
- **GLPK Examples**: Official GLPK distribution includes ~60 MathProg examples
