# S7 Migration Plan for multimod

## Motivation

### Key Benefits for AST-Heavy Codebase

1. **Robust Method Dispatch**
   - Current S3 multiple inheritance (`c("parameter", "multimod", "ast")`) creates ambiguous dispatch
   - S7 provides clear parent-child hierarchies with deterministic method resolution
   - Particularly important for AST traversal algorithms

2. **Property Typo Prevention**
   ```r
   # S3 Risk: Silent bugs
   param$defVal <- 5    # Correct
   param$defval <- 5    # Creates new field, old code sees NULL
   param$default <- 5   # Same silent failure
   
   # S7 Solution: Immediate error
   param$defval <- 5
   # Error: 'parameter' object has no property 'defval'
   ```

3. **Type Safety in AST Traversal**
   - Guaranteed valid AST node structure
   - No risk of malformed trees from invalid field assignments
   - `instanceof()` checks are reliable
   - Child properties have enforced types

4. **Less Defensive Programming**
   ```r
   # S3: Verbose validation
   as_jump.parameter <- function(x, ...) {
     if (!inherits(x, "parameter")) stop("Not a parameter")
     if (is.null(x$name)) stop("Missing name")
     if (!is.null(x$dims) && !inherits(x$dims, "dims")) stop("Invalid dims")
     ...
   }
   
   # S7: Just implement logic
   method(as_jump, parameter) <- function(x, ...) {
     # x$name guaranteed character
     # x$dims guaranteed dims_node or NULL
     ...
   }
   ```

## Migration Strategy

### Phase 1: Core AST Classes (2-3 weeks)

**Goal:** Establish foundation without breaking existing code

**Classes to Convert:**
```r
# Base class for all AST nodes
ast_node <- new_class("ast_node",
  properties = list(
    class_name = class_character  # For S3 compatibility
  )
)

# Fundamental building blocks
symbol <- new_class("symbol", 
  parent = ast_node,
  properties = list(
    name = class_character,
    dims = new_union(dims_node, NULL)
  ),
  validator = function(self) {
    if (length(self@name) != 1) "name must be scalar"
  }
)

dims_node <- new_class("dims_node",
  parent = ast_node,
  properties = list(
    elements = class_list  # List of symbol/dim objects
  ),
  validator = function(self) {
    if (!all(sapply(self@elements, function(x) {
      inherits(x, "symbol") || inherits(x, "dim_node")
    }))) {
      "dims elements must be symbol or dim_node objects"
    }
  }
)

# Operators
binary_op <- new_class("binary_op",
  parent = ast_node,
  properties = list(
    op = class_character,     # "+", "-", "*", "/", etc.
    left = ast_node,
    right = ast_node
  )
)

unary_op <- new_class("unary_op",
  parent = ast_node,
  properties = list(
    op = class_character,     # "-", "!"
    operand = ast_node
  )
)
```

**Compatibility Layer:**
```r
# S3 -> S7 converter
as_s7 <- function(x) {
  if (inherits(x, "S7_object")) return(x)
  
  if (inherits(x, "symbol")) {
    symbol(
      name = x$name,
      dims = if (!is.null(x$dims)) as_s7(x$dims) else NULL
    )
  } else if (inherits(x, "parameter")) {
    parameter(
      name = x$name,
      dims = if (!is.null(x$dims)) as_s7(x$dims) else NULL,
      defVal = x$defVal,
      data = x$data,
      trimmed = x$trimmed
    )
  }
  # ... other classes
}

# S7 -> S3 converter (for backwards compatibility)
as_s3 <- function(x) {
  if (!inherits(x, "S7_object")) return(x)
  
  # Convert S7 to list-based S3
  result <- list()
  for (prop_name in names(S7::prop_names(x))) {
    result[[prop_name]] <- prop(x, prop_name)
  }
  class(result) <- c(class(x)[[1]], "multimod", "ast")
  result
}

# Make existing functions work with both
as_jump.parameter <- function(x, ...) {
  if (inherits(x, "S7_object")) {
    method(as_jump, parameter)(x, ...)
  } else {
    # Old S3 implementation
    ...
  }
}
```

**Testing:**
- All existing tests must pass with S3 objects
- Add parallel S7 tests
- Add round-trip tests: `S3 -> S7 -> S3` preserves structure

### Phase 2: Validation & Model Structure (1-2 weeks)

**Classes:**
```r
parameter <- new_class("parameter",
  parent = ast_node,
  properties = list(
    name = class_character,
    desc = new_union(class_character, NULL),
    dims = new_union(dims_node, NULL),
    active_dims = new_union(dims_node, NULL),
    data = new_union(class_data.frame, NULL),
    defVal = class_any,  # numeric, ast_formula, character, NULL
    defInt = class_any,
    symbolic = class_logical,
    formula = new_union(ast_node, NULL),
    comment = new_union(class_character, NULL),
    trimmed = new_union(class_logical, NULL),
    folded_data = new_union(class_data.frame, NULL),
    dims_index_aliases = new_union(class_character, NULL)
  ),
  validator = function(self) {
    errors <- character()
    
    # Name must be scalar
    if (length(self@name) != 1) {
      errors <- c(errors, "name must be scalar character")
    }
    
    # Trimmed must be NULL or single logical
    if (!is.null(self@trimmed) && length(self@trimmed) != 1) {
      errors <- c(errors, "trimmed must be NULL or single logical")
    }
    
    # Cannot trim parameter with default value
    if (isTRUE(self@trimmed) && !is.null(self@defVal)) {
      errors <- c(errors, "Cannot trim parameter with default value")
    }
    
    # Data must have value column if present
    if (!is.null(self@data) && nrow(self@data) > 0) {
      if (!"value" %in% names(self@data)) {
        errors <- c(errors, "data must have 'value' column")
      }
    }
    
    if (length(errors) > 0) return(paste(errors, collapse = "; "))
  }
)

variable <- new_class("variable",
  parent = ast_node,
  properties = list(
    name = class_character,
    desc = new_union(class_character, NULL),
    dims = new_union(dims_node, NULL),
    domain = new_union(class_character, NULL),  # Mapping name
    lb = new_union(class_numeric, ast_node, NULL),
    ub = new_union(class_numeric, ast_node, NULL),
    type = class_character,  # "continuous", "integer", "binary"
    comment = new_union(class_character, NULL),
    trimmed = new_union(class_logical, NULL)
  ),
  validator = function(self) {
    if (length(self@name) != 1) return("name must be scalar")
    if (!self@type %in% c("continuous", "integer", "binary")) {
      return("type must be continuous, integer, or binary")
    }
  }
)

equation <- new_class("equation",
  parent = ast_node,
  properties = list(
    name = class_character,
    desc = new_union(class_character, NULL),
    lhs = ast_node,
    rhs = ast_node,
    sense = class_character,  # "==", "<=", ">="
    domain = new_union(class_character, dims_node, NULL),
    comment = new_union(class_character, NULL),
    trimmed = new_union(class_logical, NULL)
  ),
  validator = function(self) {
    if (length(self@name) != 1) return("name must be scalar")
    if (!self@sense %in% c("==", "<=", ">=")) {
      return("sense must be ==, <=, or >=")
    }
  }
)

model <- new_class("model",
  properties = list(
    name = new_union(class_character, NULL),
    sets = class_list,        # Never NULL, possibly empty
    parameters = class_list,
    mappings = class_list,
    variables = class_list,
    equations = class_list,
    objectives = class_list,
    aliases = class_list,
    index_aliases = class_list,
    misc = class_list
  ),
  validator = function(self) {
    # Validate that list elements are correct types
    check_list <- function(lst, expected_class, list_name) {
      if (length(lst) > 0) {
        valid <- sapply(lst, function(x) inherits(x, expected_class))
        if (!all(valid)) {
          return(sprintf("%s contains non-%s objects", list_name, expected_class))
        }
      }
    }
    
    errors <- c(
      check_list(self@parameters, "parameter", "parameters"),
      check_list(self@variables, "variable", "variables"),
      check_list(self@equations, "equation", "equations")
    )
    
    if (length(errors) > 0) return(paste(errors, collapse = "; "))
  }
)
```

**Key Improvements:**
- Empty collections are `list()` not `NULL` (simpler checks)
- Validation catches trimming bugs early
- Type safety prevents field assignment errors

### Phase 3: Method Migration (4-6 weeks)

**Generic Method Conversion:**

```r
# Define S7 generic
as_jump <- new_generic("as_jump", "x")

# Implement for each class
method(as_jump, symbol) <- function(x, context = "equation", ...) {
  # x@name guaranteed to exist and be character
  if (!is.null(x@dims)) {
    idx <- as_jump(x@dims, context = context, ...)
    sprintf("%s%s", x@name, idx)
  } else {
    x@name
  }
}

method(as_jump, parameter) <- function(x, context = "data", ...) {
  # No need to check if x@name exists
  # No risk of typo in x@defVal access
  if (context == "data") {
    # Generate data loading code
    ...
  } else {
    # Generate parameter access code
    sprintf("get(%s, %s, %sDef)", x@name, index, x@name)
  }
}

method(as_jump, binary_op) <- function(x, context = "equation", ...) {
  left_str <- as_jump(x@left, context = context, ...)
  right_str <- as_jump(x@right, context = context, ...)
  
  # Operator precedence handling
  sprintf("(%s %s %s)", left_str, x@op, right_str)
}

# Similar for as_gmpl, as_latex, print, etc.
```

**walk() Function:**
```r
# Current S3 (defensive)
walk <- function(node, fn) {
  if (is.null(node)) return(NULL)
  if (!inherits(node, "ast")) return(node)
  
  node_class <- class(node)
  if (is.null(node_class) || length(node_class) == 0) return(node)
  ...
}

# S7 version (cleaner)
walk <- new_generic("walk", c("node", "fn"))

method(walk, list(ast_node, class_function)) <- function(node, fn) {
  # Apply function to current node
  node <- fn(node)
  
  # Recursively walk children
  for (prop_name in prop_names(node)) {
    prop_val <- prop(node, prop_name)
    
    if (inherits(prop_val, "ast_node")) {
      prop(node, prop_name) <- walk(prop_val, fn)
    } else if (is.list(prop_val)) {
      prop(node, prop_name) <- lapply(prop_val, function(x) {
        if (inherits(x, "ast_node")) walk(x, fn) else x
      })
    }
  }
  
  node
}
```

## Benefits for Recent Bugs

### Bug 1: Empty Mappings List
**S3 Problem:**
```r
# Had to add many defensive checks
if (!is.null(model$mappings) && length(model$mappings) > 0) {
  if (mapping_name %in% names(model$mappings)) {
    model$mappings[[mapping_name]] <- ...
  }
}
```

**S7 Solution:**
```r
# model@mappings is always a list (never NULL)
if (length(model@mappings) > 0) {
  if (mapping_name %in% names(model@mappings)) {
    model@mappings[[mapping_name]] <- ...
  }
}
```

### Bug 2: Parameters with Defaults Being Trimmed
**S3 Problem:**
```r
# Easy to forget the check
if (is.null(param$data) || nrow(param$data) == 0) {
  param$trimmed <- TRUE  # Oops! Forgot to check defVal
}
```

**S7 Solution:**
```r
# Validator catches it immediately
parameter <- new_class(...,
  validator = function(self) {
    if (isTRUE(self@trimmed) && !is.null(self@defVal)) {
      "Cannot trim parameter with default value"
    }
  }
)

# Assignment fails:
param@trimmed <- TRUE
# Error: Cannot trim parameter with default value
```

### Bug 3: Property Typos
**S3 Problem:**
```r
# Typo creates silent bug
param$defVal <- 5
later_code_sees <- param$defval  # NULL! Creates subtle bug
```

**S7 Solution:**
```r
param@defval <- 5
# Error: 'parameter' object has no property 'defval'
```

## Migration Checklist

### Phase 1: Foundation
- [ ] Add S7 dependency to DESCRIPTION
- [ ] Create `R/s7_classes.R` with core AST classes
- [ ] Implement `as_s7()` and `as_s3()` converters
- [ ] Add S7 tests parallel to existing S3 tests
- [ ] Update `walk()` to work with S7 objects
- [ ] Document S7 classes with examples

### Phase 2: Model Structure
- [ ] Convert `parameter`, `variable`, `equation` classes
- [ ] Add validation functions
- [ ] Update `trim_model()` to use S7 validators
- [ ] Update `read_gmpl()` to create S7 objects (optional)
- [ ] Update model storage/loading for S7

### Phase 3: Methods
- [ ] Migrate `as_jump()` generic and methods
- [ ] Migrate `as_gmpl()` generic and methods  
- [ ] Migrate `as_latex()` generic and methods
- [ ] Migrate `print()` methods
- [ ] Update all AST manipulation functions
- [ ] Comprehensive testing

### Phase 4: Cleanup
- [ ] Deprecate S3 compatibility layer (after 1-2 versions)
- [ ] Remove `as_s3()` converter
- [ ] Clean up defensive checks no longer needed
- [ ] Update all documentation
- [ ] Migration guide for users

## Timeline Estimate

- **Phase 1:** 2-3 weeks (foundation, backwards compat)
- **Phase 2:** 1-2 weeks (validation, model structure)
- **Phase 3:** 4-6 weeks (method migration, testing)
- **Phase 4:** 1-2 weeks (cleanup, documentation)

**Total:** ~2-3 months with thorough testing

## Risks & Mitigation

### Risk 1: Breaking Existing Code
**Mitigation:** Maintain S3 compatibility layer for 1-2 major versions

### Risk 2: Performance Regression
**Mitigation:** Benchmark critical paths (AST traversal, method dispatch) before/after

### Risk 3: Complex AST Manipulation
**Mitigation:** Start with simple classes (symbol, dims), learn patterns before tackling complex ones

### Risk 4: Community Adoption
**Mitigation:** 
- Clear migration guide
- Show benefits in release notes
- Provide helper functions for conversion

## Success Metrics

- [ ] All existing tests pass with S7 implementation
- [ ] No performance regression (< 5% slowdown acceptable)
- [ ] Reduction in defensive checks (measure LOC in validation code)
- [ ] Zero property typo bugs reported after migration
- [ ] Improved error messages from validators

## Next Steps

1. Create `s7-migration` branch
2. Implement Phase 1 prototype
3. Evaluate ergonomics and performance
4. Decision point: proceed or revert
5. If proceeding, complete Phase 1 before moving to Phase 2

## References

- S7 Documentation: https://rconsortium.github.io/OOP-WG/
- S7 Vignettes: https://rconsortium.github.io/S7/articles/
- Performance considerations: https://rconsortium.github.io/S7/articles/performance.html
