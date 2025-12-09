## GMPL Data Format Support

The `read_gmpl_data()` and `import_gmpl_data()` functions now support multiple GMPL data representation formats:

### 1. Scalar Parameters
```
param DiscountRate default 0.05 := ;
param DepreciationMethod := 1 ;
```
- Single value assignments
- Optional default value

### 2. Simple 2D Table Format
```
param YearSplit : 1990 1991 1992 ... :=
ID    0.1667 0.1667 0.1667 ...
IN    0.0833 0.0833 0.0833 ...
SD    0.1667 0.1667 0.1667 ... ;
```
- Row headers (left column) × Column headers (after `:` before `:=`)
- Creates 2D parameter with dim1=row, dim2=column, value

### 3. Sliced Multi-Dimensional Table Format
```
param SpecifiedDemandProfile default 0 :=
    [UTOPIA,RH,*,*] : 1990 1991 1992 ... :=
    ID    0.12 0.12 0.12 ...
    IN    0.06 0.06 0.06 ...
    
    [UTOPIA,RL,*,*] : 1990 1991 1992 ... :=
    ID    0.15 0.15 0.15 ...
    IN    0.05 0.05 0.05 ... ;
```
- Multiple table blocks with different fixed dimensions
- `[fixed1,fixed2,*,*]` slices the dimensions
- Wildcards `*` are filled by row and column headers
- Can have multiple sliced blocks in one parameter declaration

### 4. Tuple Format (Bracketed Indices)
```
param Parameter := 
  [idx1,idx2,idx3] value1
  [idx1,idx2,idx4] value2
  [idx1,idx3,idx4] value3 ;
```
- Explicit index tuples with values
- Each `[...]` contains comma-separated indices

### 5. Simple List Format
```
param Parameter := 
  index1 value1
  index2 value2
  index3 value3 ;
```
- Pairs of index and value
- For 1-dimensional parameters

## Implementation Details

### Format Detection Algorithm
The parser uses this detection order:

1. **Check for sliced format**: Starts with `[...] : header :=`
2. **Check for table format**: Has column headers extracted from declaration line
3. **Check for tuple format**: Contains `[indices] value` patterns
4. **Check for scalar**: Just a single number
5. **Fallback to list format**: index value pairs

### Header Extraction
The `extract_data_block()` function now extracts:
- **header**: Text between `:` and `:=` on declaration line
- **data_lines**: All subsequent lines until `;`

This allows proper parsing of table formats where column headers appear on the same line as `param Name :`.

### Multi-Block Handling
For sliced formats with multiple blocks (like `SpecifiedDemandProfile`), the parser:
1. Finds all `[...] : header :=` patterns using regex
2. Splits text into separate blocks
3. Parses each block independently
4. Combines all rows into single data frame

## Usage Example

```r
# Read model structure
model <- read_gmpl("osemosys.txt", as_multimod = TRUE)

# Import data from Utopia
model <- import_gmpl_data(
  model, 
  gmpl_data = "utopia.txt",
  format = "memory",  # or "csv" or "arrow"
  verbose = TRUE
)

# Check loaded data
model$parameters$YearSplit$data        # 126 rows: 6 timeslices × 21 years
model$parameters$InputActivityRatio$data  # 252 rows: multi-dimensional sliced data
```

## Test Results (OSeMOSYS Utopia)

Successfully imported:
- 11 sets (all populated)
- 26 parameters with non-zero data
- Total of 3,444 parameter data rows parsed from various formats

Key parameters:
- YearSplit: 126 rows (2D table)
- SpecifiedDemandProfile: 252 rows (4D sliced table, 2 fuel blocks)
- InputActivityRatio: 252 rows (5D sliced table, 8 fuel×mode blocks)
- CapacityFactor: 630 rows (5D sliced table)
