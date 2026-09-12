# LaTeX Generation

This vignette demonstrates how to generate LaTeX documents from multimod
models, including options for displaying folded and trimmed models with
proper annotations.

## Setup

``` r
library(multimod)
```

## Load Example Model

We’ll use the UTOPIA model that comes with the multimod package:

``` r
data(example_models)
demo_model <- example_models$energyRt$multimod
print(demo_model)
#> Model: 
#> Language:  GAMS 
#> Sets:  13 
#> Mappings:  240 
#> Aliases:  7 
#> Parameters:  151 
#> Variables:  64 
#> Equations:  108
```

## Basic LaTeX Generation

The simplest way to generate LaTeX is to call
[`write_latex()`](https://optimal2050.github.io/multimod/reference/write_latex.md)
on a model:

``` r
# Generate LaTeX as a string
tex <- write_latex(demo_model)

# Save to file
write_latex(demo_model, file = "model.tex")
```

The generated LaTeX document includes: - Document preamble with
necessary packages - Title and metadata - Sets with element listings -
Parameters with dimensions - Variables with dimensions - Equations with
mathematical notation - Mappings (domain definitions)

## Model View Modes

The `model_view` parameter controls how the model is displayed:

### Auto Mode (Default)

Automatically detects if model is optimized and chooses appropriate
view:

``` r
tex <- write_latex(demo_model)  # Auto-detects view mode
```

### Reduced Mode

Shows only active (non-trimmed) elements, ideal for optimized models:

``` r
tex <- write_latex(model_optimized, model_view = "reduced")
```

### Full Mode

Shows all elements with annotations for trimmed/folded items:

``` r
tex <- write_latex(model_optimized, model_view = "full")
```

## Model Optimization and LaTeX Output

### Folding

Folding reduces parameter dimensionality by consolidating dimensions
with uniform values:

``` r
# Create fold specification
fold_spec <- create_fold_spec(
  demo_model,
  fold_dims = list(
    slice = list(
      tech = "mTechSlice",
      comm = "mCommSlice"
    ),
    region = list(
      tech = "mTechRegion",
      sup = "mSupRegion"
    )
  )
)
#> 
#> === Fold Specification Generated ===
#> Total entries: 63 
#> Can fold: 50 
#> Cannot fold: 13 
#> 
#> REVIEW CAREFULLY: validation_mapping may be incorrect for non-standard models

# Apply folding
model_folded <- fold_model(demo_model, fold_spec)
#> 
#> === Folding Model ===
#> Using provided fold specification
#> Parameters specified: 63 
#> Multi-dimensional folding detected: region, slice 
#> Applying folds sequentially by dimension
#> 
#> === Folding dimension: region ===
#> 
#> === Folding Model ===
#> Using provided fold specification
#> Parameters specified: 40 
#> 
#> ✓ pTechOlife: 2D → 1D (30 → 6 rows, 5.0x)
#> ✓ pTechCinp2use: 5D → 4D (1080 → 216 rows, 5.0x)
#> ✓ pTechUse2cact: 5D → 4D (1080 → 216 rows, 5.0x)
#> ✓ pTechCact2cout: 5D → 4D (1080 → 216 rows, 5.0x)
#> ✓ pTechFixom: 3D → 2D (90 → 18 rows, 5.0x)
#> ✓ pTechInvcost: 3D → 2D (90 → 18 rows, 5.0x)
#> ✓ pTechEac: 3D → 2D (87 → 15 rows, 5.8x)
#> ✓ pTechRetCost: 3D → 2D (45 → 9 rows, 5.0x)
#> ✓ pTechAfLo: 4D → 3D (72 → 72 rows, 1.0x)
#> ✓ pTechAfUp: 4D → 3D (1080 → 216 rows, 5.0x)
#> ✓ pTechAfsLo: 4D → 3D (3 → 3 rows, 1.0x)
#> ✓ pTechAfsUp: 4D → 3D (45 → 9 rows, 5.0x)
#> ✓ pTechAfcUp: 5D → 4D (504 → 72 rows, 7.0x)
#> ✓ pTechRetUp: 3D → 2D (3 → 3 rows, 1.0x)
#> ✓ pDiscount: 2D → 1D (21 → 3 rows, 7.0x)
#> ✓ pDiscountFactor: 2D → 1D (21 → 3 rows, 7.0x)
#> ✓ pDiscountFactorMileStone: 2D → 1D (21 → 3 rows, 7.0x)
#> ✓ pSupCost: 5D → 4D (36 → 18 rows, 2.0x)
#> ✓ pSupAvaUp: 5D → 4D (267 → 51 rows, 5.2x)
#> ✓ pSupReserveUp: 3D → 2D (2 → 2 rows, 1.0x)
#> ✓ pDummyImportCost: 4D → 3D (252 → 36 rows, 7.0x)
#> ✓ pDummyExportCost: 4D → 3D (252 → 36 rows, 7.0x)
#> ✓ pStorageInpEff: 5D → 4D (252 → 36 rows, 7.0x)
#> ✓ pStorageOutEff: 5D → 4D (252 → 36 rows, 7.0x)
#> ✓ pStorageStgEff: 5D → 4D (252 → 36 rows, 7.0x)
#> ✓ pStorageOlife: 2D → 1D (7 → 1 rows, 7.0x)
#> ✓ pStorageCostStore: 4D → 3D (36 → 36 rows, 1.0x)
#> ✓ pStorageFixom: 3D → 2D (14 → 2 rows, 7.0x)
#> ✓ pStorageInvcost: 3D → 2D (21 → 3 rows, 7.0x)
#> ✓ pStorageEac: 3D → 2D (21 → 3 rows, 7.0x)
#> ✓ pStorageAfUp: 4D → 3D (252 → 36 rows, 7.0x)
#> ✓ pImportRowPrice: 4D → 3D (105 → 15 rows, 7.0x)
#> ✓ pTradeInvcost: 3D → 2D (48 → 24 rows, 2.0x)
#> ✓ pTradeEac: 3D → 2D (48 → 24 rows, 2.0x)
#> 
#> === Fold Summary ===
#> Parameters folded: 34
#> Parameters skipped: 6
#> Total parameters: 151
#> 
#> === Folding Equations ===
#> Folded parameters: 34
#>   pTechOlife: removed region
#>   pTechCinp2use: removed region
#>   pTechUse2cact: removed region
#>   pTechCact2cout: removed region
#>   pTechFixom: removed region
#>   pTechInvcost: removed region
#>   pTechEac: removed region
#>   pTechRetCost: removed region
#>   pTechAfLo: removed region
#>   pTechAfUp: removed region
#>   pTechAfsLo: removed region
#>   pTechAfsUp: removed region
#>   pTechAfcUp: removed region
#>   pTechRetUp: removed region
#>   pDiscount: removed region
#>   pDiscountFactor: removed region
#>   pDiscountFactorMileStone: removed region
#>   pSupCost: removed region
#>   pSupAvaUp: removed region
#>   pSupReserveUp: removed region
#>   pDummyImportCost: removed region
#>   pDummyExportCost: removed region
#>   pStorageInpEff: removed region
#>   pStorageOutEff: removed region
#>   pStorageStgEff: removed region
#>   pStorageOlife: removed region
#>   pStorageCostStore: removed region
#>   pStorageFixom: removed region
#>   pStorageInvcost: removed region
#>   pStorageEac: removed region
#>   pStorageAfUp: removed region
#>   pImportRowPrice: removed region
#>   pTradeInvcost: removed region
#>   pTradeEac: removed region
#> 
#> Equations scanned: 108
#> Equations with folded params: 33
#> === Folding dimension: slice ===
#> 
#> === Folding Model ===
#> Using provided fold specification
#> Parameters specified: 23 
#> 
#> ✓ pTechCinp2use: 4D → 3D (1080 → 18 rows, 60.0x)
#> ✓ pTechUse2cact: 4D → 3D (1080 → 18 rows, 60.0x)
#> ✓ pTechCact2cout: 4D → 3D (1080 → 18 rows, 60.0x)
#> ✓ pTechAfLo: 3D → 2D (72 → 6 rows, 12.0x)
#> ✓ pTechAfUp: 3D → 2D (1080 → 18 rows, 60.0x)
#> ✓ pTechAfcUp: 4D → 3D (504 → 6 rows, 84.0x)
#> ✓ pSupCost: 4D → 3D (36 → 9 rows, 4.0x)
#> ✓ pSupAvaUp: 4D → 3D (267 → 9 rows, 29.7x)
#> ✓ pDummyImportCost: 3D → 2D (252 → 3 rows, 84.0x)
#> ✓ pDummyExportCost: 3D → 2D (252 → 3 rows, 84.0x)
#> ✓ pStorageInpEff: 4D → 3D (252 → 3 rows, 84.0x)
#> ✓ pStorageOutEff: 4D → 3D (252 → 3 rows, 84.0x)
#> ✓ pStorageStgEff: 4D → 3D (252 → 3 rows, 84.0x)
#> ✓ pStorageCostStore: 3D → 2D (36 → 3 rows, 12.0x)
#> ✓ pStorageAfUp: 3D → 2D (252 → 3 rows, 84.0x)
#> ✓ pImportRowPrice: 3D → 2D (105 → 6 rows, 17.5x)
#> 
#> === Fold Summary ===
#> Parameters folded: 16
#> Parameters skipped: 7
#> Total parameters: 151
#> 
#> === Folding Equations ===
#> Folded parameters: 34
#>   pTechOlife: removed region
#>   pTechCinp2use: removed region, slice
#>   pTechUse2cact: removed region, slice
#>   pTechCact2cout: removed region, slice
#>   pTechFixom: removed region
#>   pTechInvcost: removed region
#>   pTechEac: removed region
#>   pTechRetCost: removed region
#>   pTechAfLo: removed region, slice
#>   pTechAfUp: removed region, slice
#>   pTechAfsLo: removed region
#>   pTechAfsUp: removed region
#>   pTechAfcUp: removed region, slice
#>   pTechRetUp: removed region
#>   pDiscount: removed region
#>   pDiscountFactor: removed region
#>   pDiscountFactorMileStone: removed region
#>   pSupCost: removed region, slice
#>   pSupAvaUp: removed region, slice
#>   pSupReserveUp: removed region
#>   pDummyImportCost: removed region, slice
#>   pDummyExportCost: removed region, slice
#>   pStorageInpEff: removed region, slice
#>   pStorageOutEff: removed region, slice
#>   pStorageStgEff: removed region, slice
#>   pStorageOlife: removed region
#>   pStorageCostStore: removed region, slice
#>   pStorageFixom: removed region
#>   pStorageInvcost: removed region
#>   pStorageEac: removed region
#>   pStorageAfUp: removed region, slice
#>   pImportRowPrice: removed region, slice
#>   pTradeInvcost: removed region
#>   pTradeEac: removed region
#> 
#> Equations scanned: 108
#> Equations with folded params: 33

# Check folding summary
get_fold_summary(model_folded, format = "text")
```

### Trimming

Trimming removes empty and unused model elements:

``` r
# Trim the folded model
model_optimized <- trim_model(model_folded)
#> Starting model trimming...
#>   Phase 1: Identifying empty sets, parameters, and mappings...
#>     Trimmed set: expp
#>     Trimmed set: group
#>     Trimmed parameter: pTradeVarom
#>     Trimmed mapping: mStartMilestone
#>     Trimmed mapping: mEndMilestone
#>     Trimmed mapping: mTechRetirement
#>     Trimmed mapping: mTechUpgrade
#>     Trimmed mapping: mTechInpGroup
#>     Trimmed mapping: mTechOutGroup
#>     Trimmed mapping: mTechGroupComm
#>     Trimmed mapping: mTechAInp
#>     Trimmed mapping: mUpComm
#>     Trimmed mapping: mFxComm
#>     Trimmed mapping: mStorageAInp
#>     Trimmed mapping: mStorageAOut
#>     Trimmed mapping: mTradeIrAInp
#>     Trimmed mapping: mTradeIrAOut
#>     Trimmed mapping: mExpComm
#>     Trimmed mapping: mExpSlice
#>     Trimmed mapping: mDiscountZero
#>     Trimmed mapping: mTradeOlifeInf
#>     Trimmed mapping: mAggregateFactor
#>     Trimmed mapping: mSupWeatherLo
#>     Trimmed mapping: mSupWeatherUp
#>     Trimmed mapping: mTechWeatherAfLo
#>     Trimmed mapping: mTechWeatherAfsLo
#>     Trimmed mapping: mTechWeatherAfsUp
#>     Trimmed mapping: mTechWeatherAfcLo
#>     Trimmed mapping: mTechWeatherAfcUp
#>     Trimmed mapping: mStorageWeatherAfLo
#>     Trimmed mapping: mStorageWeatherAfUp
#>     Trimmed mapping: mStorageWeatherCinpUp
#>     Trimmed mapping: mStorageWeatherCinpLo
#>     Trimmed mapping: mStorageWeatherCoutUp
#>     Trimmed mapping: mStorageWeatherCoutLo
#>     Trimmed mapping: mvTechRetiredNewCap
#>     Trimmed mapping: mvTechRetiredStock
#>     Trimmed mapping: mvTechInpCommSameSlice
#>     Trimmed mapping: mvTechAInp
#>     Trimmed mapping: mvInp2Lo
#>     Trimmed mapping: mvOut2Lo
#>     Trimmed mapping: mInpSub
#>     Trimmed mapping: mOutSub
#>     Trimmed mapping: mTechCapLo
#>     Trimmed mapping: mTechCapUp
#>     Trimmed mapping: mTechNewCapLo
#>     Trimmed mapping: mTechNewCapUp
#>     Trimmed mapping: mTechRetLo
#>     Trimmed mapping: mvStorageAInp
#>     Trimmed mapping: mvStorageAOut
#>     Trimmed mapping: mStorageStg2AOut
#>     Trimmed mapping: mStorageCinp2AOut
#>     Trimmed mapping: mStorageCout2AOut
#>     Trimmed mapping: mStorageCap2AOut
#>     Trimmed mapping: mStorageNCap2AOut
#>     Trimmed mapping: mStorageStg2AInp
#>     Trimmed mapping: mStorageCinp2AInp
#>     Trimmed mapping: mStorageCout2AInp
#>     Trimmed mapping: mStorageCap2AInp
#>     Trimmed mapping: mStorageNCap2AInp
#>     Trimmed mapping: mStorageCapLo
#>     Trimmed mapping: mStorageCapUp
#>     Trimmed mapping: mStorageNewCapLo
#>     Trimmed mapping: mStorageNewCapUp
#>     Trimmed mapping: mStorageRetLo
#>     Trimmed mapping: mStorageRetUp
#>     Trimmed mapping: mTradeIrCsrc2Ainp
#>     Trimmed mapping: mTradeIrCdst2Ainp
#>     Trimmed mapping: mTradeIrCsrc2Aout
#>     Trimmed mapping: mTradeIrCdst2Aout
#>     Trimmed mapping: mvTotalUserCosts
#>     Trimmed mapping: mTradeCapLo
#>     Trimmed mapping: mTradeCapUp
#>     Trimmed mapping: mTradeNewCapLo
#>     Trimmed mapping: mTradeNewCapUp
#>     Trimmed mapping: mTradeRetLo
#>     Trimmed mapping: mTradeRetUp
#>     Trimmed mapping: mTechAInpCommSameSlice
#>     Trimmed mapping: mTechAInpCommAgg
#>     Trimmed mapping: mTechAInpCommAggSlice
#>     Trimmed mapping: mTechOutCommAgg
#>     Trimmed mapping: mTechOutCommAggSlice
#>     Trimmed mapping: mvTradeIrAInp
#>     Trimmed mapping: mvTradeIrAInpTot
#>     Trimmed mapping: mvTradeIrAOut
#>     Trimmed mapping: mvTradeIrAOutTot
#>     Trimmed mapping: mImportRowUp
#>     Trimmed mapping: mImportRowCumUp
#>     Trimmed mapping: mExportRow
#>     Trimmed mapping: mExportRowUp
#>     Trimmed mapping: mExportRowCumUp
#>     Trimmed mapping: mTaxCost
#>     Trimmed mapping: mSubCost
#>     Trimmed mapping: mAggOut
#>     Trimmed mapping: mTechAfUp
#>     Trimmed mapping: mTechRampUp
#>     Trimmed mapping: mTechRampDown
#>     Trimmed mapping: mTechCommOutSliceSliceP
#>     Trimmed mapping: mTechCommAOutSliceSliceP
#>     Trimmed mapping: mTechOlifeInf
#>     Trimmed mapping: mTechAfcUp
#>     Trimmed mapping: mOut2Lo
#>     Trimmed mapping: mInp2Lo
#>     Trimmed mapping: meqTechRetiredNewCap
#>     Trimmed mapping: meqTechGrp2Sng
#>     Trimmed mapping: meqTechSng2Grp
#>     Trimmed mapping: meqTechGrp2Grp
#>     Trimmed mapping: meqTechShareInpLo
#>     Trimmed mapping: meqTechShareInpUp
#>     Trimmed mapping: meqTechShareOutLo
#>     Trimmed mapping: meqTechShareOutUp
#>     Trimmed mapping: meqTechActGrp
#>     Trimmed mapping: meqTechAfcOutLo
#>     Trimmed mapping: meqTechAfcOutUp
#>     Trimmed mapping: meqTechAfcInpLo
#>     Trimmed mapping: meqSupAvaLo
#>     Trimmed mapping: meqSupReserveLo
#>     Trimmed mapping: meqStorageAfLo
#>     Trimmed mapping: meqStorageInpUp
#>     Trimmed mapping: meqStorageInpLo
#>     Trimmed mapping: meqStorageOutUp
#>     Trimmed mapping: meqStorageOutLo
#>     Trimmed mapping: meqTradeFlowUp
#>     Trimmed mapping: meqTradeFlowLo
#>     Trimmed mapping: meqExportRowLo
#>     Trimmed mapping: meqImportRowUp
#>     Trimmed mapping: meqImportRowLo
#>     Trimmed mapping: meqBalUp
#>     Trimmed mapping: meqBalFx
#>     Trimmed mapping: meqLECActivity
#>     Trimmed mapping: mTechAct2AInp
#>     Trimmed mapping: mTechCap2AInp
#>     Trimmed mapping: mTechNCap2AInp
#>     Trimmed mapping: mTechCinp2AInp
#>     Trimmed mapping: mTechCout2AInp
#>     Trimmed mapping: mTechCap2AOut
#>     Trimmed mapping: mTechNCap2AOut
#>     Trimmed mapping: mTechCinp2AOut
#>     Trimmed mapping: mTechCout2AOut
#>     Trimmed mapping: mLECRegion
#>     Total empty data elements: 140
#>   Phase 2: Identifying variables with empty domains...
#>     Trimmed variable (empty domain mapping): vTechRetiredStockCum
#>     Trimmed variable (empty domain mapping): vTechRetiredStock
#>     Trimmed variable (empty domain mapping): vTechRetiredNewCap
#>     Trimmed variable (empty domain mapping): vTechAInp
#>     Trimmed variable (empty domain mapping): vInp2Lo
#>     Trimmed variable (empty domain mapping): vOut2Lo
#>     Trimmed variable (empty domain mapping): vStorageAInp
#>     Trimmed variable (empty domain mapping): vStorageAOut
#>     Trimmed variable (empty domain mapping): vTaxCost
#>     Trimmed variable (empty domain mapping): vSubsCost
#>     Trimmed variable (empty domain mapping): vAggOutTot
#>     Trimmed variable (empty domain mapping): vTradeIrAInp
#>     Trimmed variable (empty domain mapping): vTradeIrAInpTot
#>     Trimmed variable (empty domain mapping): vTradeIrAOut
#>     Trimmed variable (empty domain mapping): vTradeIrAOutTot
#>     Trimmed variable (empty domain mapping): vExportRowCum
#>     Trimmed variable (empty domain mapping): vExportRow
#>     Trimmed variable (empty domain mapping): vTotalUserCosts
#>     Total variables with empty domains: 18
#>   Phase 3: Identifying equations with empty domains...
#>     Trimmed equation (empty domain mapping): eqTechGrp2Sng
#>     Trimmed equation (empty domain mapping): eqTechSng2Grp
#>     Trimmed equation (empty domain mapping): eqTechGrp2Grp
#>     Trimmed equation (empty domain mapping): eqTechShareInpLo
#>     Trimmed equation (empty domain mapping): eqTechShareInpUp
#>     Trimmed equation (empty domain mapping): eqTechShareOutLo
#>     Trimmed equation (empty domain mapping): eqTechShareOutUp
#>     Trimmed equation (empty domain mapping): eqTechAInp
#>     Trimmed equation (empty domain mapping): eqTechRampUp
#>     Trimmed equation (empty domain mapping): eqTechRampDown
#>     Trimmed equation (empty domain mapping): eqTechActGrp
#>     Trimmed equation (empty domain mapping): eqTechAfcOutLo
#>     Trimmed equation (empty domain mapping): eqTechAfcOutUp
#>     Trimmed equation (empty domain mapping): eqTechAfcInpLo
#>     Trimmed equation (empty domain mapping): eqTechCapLo
#>     Trimmed equation (empty domain mapping): eqTechCapUp
#>     Trimmed equation (empty domain mapping): eqTechNewCapLo
#>     Trimmed equation (empty domain mapping): eqTechNewCapUp
#>     Trimmed equation (empty domain mapping): eqTechRetiredStock
#>     Trimmed equation (empty domain mapping): eqTechRetiredStockCum
#>     Trimmed equation (empty domain mapping): eqTechRetiredNewCap
#>     Trimmed equation (empty domain mapping): eqTechRetLo
#>     Trimmed equation (all variables trimmed): eqTechRetUp
#>     Trimmed equation (empty domain mapping): eqSupAvaLo
#>     Trimmed equation (empty domain mapping): eqSupReserveLo
#>     Trimmed equation (empty domain mapping): eqAggOutTot
#>     Trimmed equation (empty domain mapping): eqStorageAfLo
#>     Trimmed equation (empty domain mapping): eqStorageAInp
#>     Trimmed equation (empty domain mapping): eqStorageAOut
#>     Trimmed equation (empty domain mapping): eqStorageInpUp
#>     Trimmed equation (empty domain mapping): eqStorageInpLo
#>     Trimmed equation (empty domain mapping): eqStorageOutUp
#>     Trimmed equation (empty domain mapping): eqStorageOutLo
#>     Trimmed equation (empty domain mapping): eqStorageCapLo
#>     Trimmed equation (empty domain mapping): eqStorageCapUp
#>     Trimmed equation (empty domain mapping): eqStorageNewCapLo
#>     Trimmed equation (empty domain mapping): eqStorageNewCapUp
#>     Trimmed equation (empty domain mapping): eqTradeFlowUp
#>     Trimmed equation (empty domain mapping): eqTradeFlowLo
#>     Trimmed equation (empty domain mapping): eqExportRowUp
#>     Trimmed equation (empty domain mapping): eqExportRowLo
#>     Trimmed equation (empty domain mapping): eqExportRowCum
#>     Trimmed equation (empty domain mapping): eqExportRowResUp
#>     Trimmed equation (empty domain mapping): eqImportRowUp
#>     Trimmed equation (empty domain mapping): eqImportRowLo
#>     Trimmed equation (empty domain mapping): eqImportRowResUp
#>     Trimmed equation (empty domain mapping): eqTradeCapLo
#>     Trimmed equation (empty domain mapping): eqTradeCapUp
#>     Trimmed equation (empty domain mapping): eqTradeNewCapLo
#>     Trimmed equation (empty domain mapping): eqTradeNewCapUp
#>     Trimmed equation (empty domain mapping): eqTradeIrAInp
#>     Trimmed equation (empty domain mapping): eqTradeIrAOut
#>     Trimmed equation (empty domain mapping): eqTradeIrAInpTot
#>     Trimmed equation (empty domain mapping): eqTradeIrAOutTot
#>     Trimmed equation (empty domain mapping): eqBalUp
#>     Trimmed equation (empty domain mapping): eqBalFx
#>     Trimmed equation (empty domain mapping): eqInp2Lo
#>     Trimmed equation (empty domain mapping): eqOut2Lo
#>     Trimmed equation (empty domain mapping): eqTaxCost
#>     Trimmed equation (empty domain mapping): eqSubsCost
#>     Trimmed equation (empty domain mapping): eqLECActivity
#>     Trimmed equation (empty domain mapping): eqTotalUserCosts
#>     Total equations with empty domains: 62
#>   Phase 4: Verifying required elements...
#>     Untrimmed variable (used in equation): vTechRetiredStockCum
#>     Untrimmed variable (used in equation): vTechRetiredNewCap
#>     Untrimmed variable (used in equation): vTechAInp
#>     Untrimmed variable (used in equation): vExportRow
#>     Untrimmed variable (used in equation): vAggOutTot
#>     Untrimmed variable (used in equation): vTradeIrAOutTot
#>     Untrimmed variable (used in equation): vOut2Lo
#>     Untrimmed variable (used in equation): vTradeIrAInpTot
#>     Untrimmed variable (used in equation): vInp2Lo
#>     Untrimmed variable (used in equation): vStorageAInp
#>     Untrimmed variable (used in equation): vStorageAOut
#>     Untrimmed variable (used in equation): vTechRetiredStock
#>     Untrimmed variable (used in equation): vTaxCost
#>     Untrimmed variable (used in equation): vSubsCost
#>     Untrimmed variable (used in equation): vTotalUserCosts
#>     Untrimmed mapping (used in equation domain): mTechCap2AOut
#>     Untrimmed mapping (used in equation domain): mTechNCap2AOut
#>     Untrimmed mapping (used in equation domain): mTechCinp2AOut
#>     Untrimmed mapping (used in equation domain): mTechCout2AOut
#>     Untrimmed mapping (used in equation domain): mTechWeatherAfLo
#>     Untrimmed mapping (used in equation domain): mTechWeatherAfsLo
#>     Untrimmed mapping (used in equation domain): mTechWeatherAfsUp
#>     Untrimmed mapping (used in equation domain): mTechWeatherAfcUp
#>     Untrimmed mapping (used in equation domain): mvTechRetiredStock
#>     Untrimmed mapping (used in equation domain): mTechOlifeInf
#>     Untrimmed mapping (used in equation domain): mvTechRetiredNewCap
#>     Untrimmed mapping (used in equation domain): mvTechAInp
#>     Untrimmed mapping (used in equation domain): mSupWeatherUp
#>     Untrimmed mapping (used in equation domain): mStorageWeatherAfUp
#>     Untrimmed mapping (used in equation domain): mExpComm
#>     Untrimmed mapping (used in equation domain): mExportRow
#>     Untrimmed mapping (used in equation domain): mTradeOlifeInf
#>     Untrimmed mapping (used in equation domain): mAggOut
#>     Untrimmed mapping (used in equation domain): mvTradeIrAOutTot
#>     Untrimmed mapping (used in equation domain): mOutSub
#>     Untrimmed mapping (used in equation domain): mvOut2Lo
#>     Untrimmed mapping (used in equation domain): mvTradeIrAInpTot
#>     Untrimmed mapping (used in equation domain): mInpSub
#>     Untrimmed mapping (used in equation domain): mvInp2Lo
#>     Untrimmed mapping (used in equation domain): mTechAInpCommSameSlice
#>     Untrimmed mapping (used in equation domain): mTechAInpCommAgg
#>     Untrimmed mapping (used in equation domain): mTechAInpCommAggSlice
#>     Untrimmed mapping (used in equation domain): mTechOutCommAgg
#>     Untrimmed mapping (used in equation domain): mTechOutCommAggSlice
#>     Untrimmed mapping (used in equation domain): mvStorageAInp
#>     Untrimmed mapping (used in equation domain): mvStorageAOut
#>     Untrimmed mapping (used in equation domain): mTaxCost
#>     Untrimmed mapping (used in equation domain): mSubCost
#>     Untrimmed mapping (used in equation domain): mvTotalUserCosts
#>     Untrimmed set (used in dimension): group
#>     Untrimmed set (used in dimension): expp
#>     Total elements untrimmed: 51
#>   Synced trim flags to folded equations
#> 
#> === NET Trim Results (after Phase 4) ===
#>   Sets trimmed:       0
#>   Parameters trimmed: 1
#>   Mappings trimmed:   103
#>   Variables trimmed:  3
#>   Equations trimmed:  62
#>   TOTAL trimmed:      169
#> 
#> Model Trimming Summary
#> ======================
#> sets: 0/13 trimmed (0.0%), 13 remaining
#> parameters: 1/151 trimmed (0.7%), 150 remaining
#> mappings: 103/240 trimmed (42.9%), 137 remaining
#> variables: 3/64 trimmed (4.7%), 61 remaining
#> equations: 62/108 trimmed (57.4%), 46 remaining

# Check trimming summary
get_trim_summary(model_optimized, format = "text")
#> [1] "Model Trimming Summary\n======================\nsets: 0/13 trimmed (0.0%), 13 remaining\nparameters: 1/151 trimmed (0.7%), 150 remaining\nmappings: 103/240 trimmed (42.9%), 137 remaining\nvariables: 3/64 trimmed (4.7%), 61 remaining\nequations: 62/108 trimmed (57.4%), 46 remaining"
```

### LaTeX with Optimization Summary

When generating LaTeX for optimized models, an optimization summary is
automatically included:

``` r
tex <- write_latex(model_optimized, model_view = "full")
```

The optimization summary includes: - **Folding statistics**: Number and
percentage of folded parameters - **Dimension reductions**: Details of
each folded parameter (e.g., 5D → 2D) - **Trimming statistics**: Counts
for trimmed sets, parameters, mappings, variables, equations -
**Explanatory text**: Describes what folding and trimming do

### Folded Annotations

In full view mode, folded parameters show green annotations:

``` r
# Parameters will show: (folded: 5D → 2D) in green
tex <- write_latex(model_optimized, model_view = "full")
```

### Trimmed Annotations

Trimmed elements are marked in blue (customizable):

``` r
# Default blue color
tex <- write_latex(model_optimized, 
                  model_view = "full", 
                  trimmed_color = "blue")

# Custom color
tex <- write_latex(model_optimized, 
                  model_view = "full", 
                  trimmed_color = "red")
```

## Data Display Options

### Include Data

Control whether to show set elements and parameter row counts:

``` r
# With data (default)
tex <- write_latex(demo_model, include_data = TRUE)

# Without data (theory mode)
tex <- write_latex(demo_model, include_data = FALSE)
```

### Data Detail Level

Control the amount of data shown:

``` r
# Brief: Shows row counts and first few elements
tex <- write_latex(demo_model, data_detail = "brief")

# Detailed: Shows more information
tex <- write_latex(demo_model, data_detail = "detailed")
```

### Set Element Display

Customize how set elements are displayed:

``` r
tex <- write_latex(demo_model,
                  set_display_max_inline = 10,        # Max elements to show inline
                  set_display_head_tail_threshold = 100,  # Threshold for head/tail
                  set_display_head_n = 5,             # Number of head elements
                  set_display_tail_n = 3)             # Number of tail elements
```

## Index Aliases

Index aliases provide short names for dimensions in equations:

``` r
# Aliases are auto-detected from model$index_aliases
tex <- write_latex(demo_model, use_model_aliases = TRUE)

# This will use 'c' instead of 'comm', 'r' instead of 'region', etc.
```

The LaTeX output includes an “Index Aliases” section showing the
mapping.

## Controlling Alias Usage

Control where aliases appear:

``` r
# Default: Full names in declarations, aliases in equations
tex <- write_latex(demo_model, use_aliases_in_declarations = FALSE)

# Aliases everywhere
tex <- write_latex(demo_model, use_aliases_in_declarations = TRUE)
```

## Section Visibility

Control which sections appear in the output:

``` r
tex <- write_latex(demo_model,
                  include_sets = TRUE,
                  include_aliases = TRUE,
                  include_index_aliases = TRUE,
                  include_parameters = TRUE,
                  include_variables = TRUE,
                  include_equations = TRUE,
                  include_mappings = TRUE)  # Now TRUE by default

# Equations only
tex <- write_latex(demo_model,
                  include_sets = FALSE,
                  include_parameters = FALSE,
                  include_variables = FALSE,
                  include_equations = TRUE)
```

## Table of Contents

Add a table of contents to the document:

``` r
tex <- write_latex(demo_model, include_toc = TRUE)
```

## Complete Example

Here’s a complete example generating a publication-ready LaTeX document:

``` r
# Load and optimize model
data(example_models)
demo_model <- example_models$energyRt$multimod

# Create comprehensive fold specification
fold_spec <- create_fold_spec(
  demo_model,
  fold_dims = list(
    slice = list(
      tech = "mTechSlice",
      comm = "mCommSlice",
      stg = c("mStorageComm", "mCommSlice")
    ),
    region = list(
      tech = "mTechRegion",
      sup = "mSupRegion"
    ),
    year = list(
      tech = "mTechYear",
      stg = "mStorageYear"
    )
  )
)

# Apply folding and trimming
model_folded <- fold_model(demo_model, fold_spec)
model_optimized <- trim_model(model_folded)

# Generate comprehensive LaTeX document
write_latex(
  model_optimized,
  file = "utopia_optimized.tex",
  model_view = "full",              # Show all with annotations
  include_toc = TRUE,               # Include table of contents
  include_data = TRUE,              # Show data details
  data_detail = "brief",            # Brief data format
  use_model_aliases = TRUE,         # Use short names in equations
  use_aliases_in_declarations = FALSE,  # Full names in declarations
  trimmed_color = "blue",           # Color for trimmed annotations
  include_sets = TRUE,
  include_aliases = TRUE,
  include_index_aliases = TRUE,
  include_parameters = TRUE,
  include_variables = TRUE,
  include_equations = TRUE,
  include_mappings = TRUE
)

# Compile with pdflatex
system("pdflatex utopia_optimized.tex")
```

## Underscore Handling

The LaTeX generator automatically escapes underscores in set element
names, ensuring they display correctly:

``` r
# Set elements like "A_D", "RES_HYD" are automatically escaped
# They will appear correctly in the PDF as A_D, RES_HYD
tex <- write_latex(demo_model, include_data = TRUE)
```

## Converting Individual Objects

You can also convert individual model components to LaTeX:

``` r
# Convert a parameter
param_latex <- as_latex(demo_model$parameters$pTechCinp2use)

# Convert a variable
var_latex <- as_latex(demo_model$variables$vTechInp)

# Convert an equation
eq_latex <- as_latex(demo_model$equations$eqTechInp2Out)

# Convert an AST node
ast_latex <- as_latex(demo_model$equations$eqTechInp2Out$rhs)
```

## Tips for Publication-Quality Output

1.  **Use full mode with optimized models** to show readers what was
    removed
2.  **Include table of contents** for longer models
3.  **Add explanatory text** via the optimization summary
4.  **Use consistent colors** for annotations (blue for trimmed, green
    for folded)
5.  **Show brief data** to give readers context without overwhelming
    detail
6.  **Use index aliases** to make equations more readable
7.  **Keep full names in declarations** for clarity

## Compiling LaTeX Documents

After generating the `.tex` file, compile it with:

``` bash
pdflatex model.tex
```

For models with table of contents, run twice:

``` bash
pdflatex model.tex
pdflatex model.tex
```

## Summary

The
[`write_latex()`](https://optimal2050.github.io/multimod/reference/write_latex.md)
function provides comprehensive control over LaTeX generation:

- **Model views**: Auto, reduced, full, or both
- **Data display**: Control detail level and element formatting
- **Optimization**: Automatic summary with explanatory text
- **Annotations**: Visual markers for folded and trimmed elements
- **Aliases**: Short names for readability
- **Sections**: Fine-grained control over content
- **TOC**: Optional table of contents
- **Customization**: Colors, fonts, and display options

This makes it easy to generate publication-ready mathematical
documentation directly from your model code.
