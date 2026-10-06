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
#> Source:  C:/Users/admin/Documents/R/energyRt/gams/energyRt.gms 
#> Language:  GAMS 
#> Sets:  27 
#> Mappings:  324 
#> Aliases:  7 
#> Parameters:  218 
#> Variables:  93 
#> Equations:  153
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
#> Total entries: 23 
#> Can fold: 20 
#> Cannot fold: 3 
#> 
#> REVIEW CAREFULLY: validation_mapping may be incorrect for non-standard models

# Apply folding
model_folded <- fold_model(demo_model, fold_spec)
#> 
#> === Folding Model ===
#> Using provided fold specification
#> Parameters specified: 23 
#> 
#> ✓ pTechOlife: 2D → 1D (44 → 7 rows, 6.3x)
#> ✓ pTechCinp2use: 5D → 4D (1344 → 192 rows, 7.0x)
#> ✓ pTechFixom: 3D → 2D (176 → 28 rows, 6.3x)
#> ✓ pTechInvcost: 3D → 2D (176 → 28 rows, 6.3x)
#> ✓ pTechEac: 3D → 2D (140 → 20 rows, 7.0x)
#> ✓ pTechAfLo: 4D → 3D (336 → 48 rows, 7.0x)
#> ✓ pWacc: 2D → 1D (28 → 4 rows, 7.0x)
#> ✓ pSdr: 2D → 1D (28 → 4 rows, 7.0x)
#> ✓ pDiscountFactor: 2D → 1D (21 → 3 rows, 7.0x)
#> ✓ pSupCost: 5D → 4D (76 → 16 rows, 4.8x)
#> ✓ pStorageInpEff: 5D → 4D (336 → 48 rows, 7.0x)
#> ✓ pStorageOutEff: 5D → 4D (336 → 48 rows, 7.0x)
#> ✓ pStorageOlife: 2D → 1D (7 → 1 rows, 7.0x)
#> ✓ pStorageOutInvcost: 3D → 2D (28 → 4 rows, 7.0x)
#> ✓ pStorageOutEac: 3D → 2D (28 → 4 rows, 7.0x)
#> ✓ pStorageDurationLo: 3D → 2D (28 → 4 rows, 7.0x)
#> ✓ pStorageDurationUp: 3D → 2D (28 → 4 rows, 7.0x)
#> ✓ pImportRowPrice: 4D → 3D (56 → 8 rows, 7.0x)
#> ✓ pTradeInvcost: 3D → 2D (48 → 24 rows, 2.0x)
#> ✓ pTradeEac: 3D → 2D (48 → 24 rows, 2.0x)
#> 
#> === Fold Summary ===
#> Parameters folded: 20
#> Parameters skipped: 3
#> Total parameters: 218
#> 
#> === Folding Equations ===
#> Folded parameters: 20
#>   pTechOlife: removed region
#>   pTechCinp2use: removed region
#>   pTechFixom: removed region
#>   pTechInvcost: removed region
#>   pTechEac: removed region
#>   pTechAfLo: removed region
#>   pWacc: removed region
#>   pSdr: removed region
#>   pDiscountFactor: removed region
#>   pSupCost: removed region
#>   pStorageInpEff: removed region
#>   pStorageOutEff: removed region
#>   pStorageOlife: removed region
#>   pStorageOutInvcost: removed region
#>   pStorageOutEac: removed region
#>   pStorageDurationLo: removed region
#>   pStorageDurationUp: removed region
#>   pImportRowPrice: removed region
#>   pTradeInvcost: removed region
#>   pTradeEac: removed region
#> 
#> Equations scanned: 153
#> Equations with folded params: 23

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
#>     Trimmed set: group
#>     Trimmed set: mTechRetirement
#>     Trimmed set: mUpComm
#>     Trimmed set: mFxComm
#>     Trimmed set: mDiscountZero
#>     Trimmed set: mTradeOlifeInf
#>     Trimmed set: mTradeCapacityVariable
#>     Trimmed parameter: pDiscountFactorMileStone
#>     Trimmed mapping: mStartMilestone
#>     Trimmed mapping: mEndMilestone
#>     Trimmed mapping: mTechRetirement
#>     Trimmed mapping: mTechRetCost
#>     Trimmed mapping: mTechUpgrade
#>     Trimmed mapping: mTechInpGroup
#>     Trimmed mapping: mTechOutGroup
#>     Trimmed mapping: mTechGroupComm
#>     Trimmed mapping: mTechAInp
#>     Trimmed mapping: mTechAOut
#>     Trimmed mapping: mUpComm
#>     Trimmed mapping: mFxComm
#>     Trimmed mapping: mStorageAInp
#>     Trimmed mapping: mStorageAOut
#>     Trimmed mapping: mStorageFixom
#>     Trimmed mapping: mStorageVarom
#>     Trimmed mapping: mTradeIrAInp
#>     Trimmed mapping: mTradeIrAOut
#>     Trimmed mapping: mDiscountZero
#>     Trimmed mapping: mRegionFamily
#>     Trimmed mapping: mCommRegion
#>     Trimmed mapping: mTradeOlifeInf
#>     Trimmed mapping: mTradeCapacityVariable
#>     Trimmed mapping: mTradeFixom
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
#>     Trimmed mapping: mStorageWeatherInpAfUp
#>     Trimmed mapping: mStorageWeatherInpAfLo
#>     Trimmed mapping: mStorageWeatherOutAfUp
#>     Trimmed mapping: mStorageWeatherOutAfLo
#>     Trimmed mapping: mvSupReserve
#>     Trimmed mapping: mvTechRetiredNewCap
#>     Trimmed mapping: mvTechRetiredStock
#>     Trimmed mapping: mTechPho2AInp
#>     Trimmed mapping: mTechPho2AOut
#>     Trimmed mapping: mTechRet2AInp
#>     Trimmed mapping: mTechRet2AOut
#>     Trimmed mapping: mStoragePho2AInp
#>     Trimmed mapping: mStoragePho2AOut
#>     Trimmed mapping: mStorageRet2AInp
#>     Trimmed mapping: mStorageRet2AOut
#>     Trimmed mapping: mvStorageRetiredStock
#>     Trimmed mapping: mvStorageRetiredNewCap
#>     Trimmed mapping: mStorageRetCost
#>     Trimmed mapping: mStorageInpRetLo
#>     Trimmed mapping: mStorageInpRetUp
#>     Trimmed mapping: mStorageStgRetLo
#>     Trimmed mapping: mStorageStgRetUp
#>     Trimmed mapping: mvTradeRetiredStock
#>     Trimmed mapping: mvTradeRetiredNewCap
#>     Trimmed mapping: mTradeRetCost
#>     Trimmed mapping: mvTechInpCommSameTimeslice
#>     Trimmed mapping: mvTechAInp
#>     Trimmed mapping: mvTechAOut
#>     Trimmed mapping: mTechCapLo
#>     Trimmed mapping: mTechCapUp
#>     Trimmed mapping: mTechNewCapLo
#>     Trimmed mapping: mTechNewCapUp
#>     Trimmed mapping: mTechRetLo
#>     Trimmed mapping: mTechRetUp
#>     Trimmed mapping: mvStorageAInp
#>     Trimmed mapping: mvStorageAOut
#>     Trimmed mapping: mStorageStg2AOut
#>     Trimmed mapping: mStorageCinp2AOut
#>     Trimmed mapping: mStorageCout2AOut
#>     Trimmed mapping: mStorageOutCap2AOut
#>     Trimmed mapping: mStorageOutNCap2AOut
#>     Trimmed mapping: mStorageInpCap2AInp
#>     Trimmed mapping: mStorageInpCap2AOut
#>     Trimmed mapping: mStorageInpNCap2AInp
#>     Trimmed mapping: mStorageInpNCap2AOut
#>     Trimmed mapping: mStorageStgCap2AInp
#>     Trimmed mapping: mStorageStgCap2AOut
#>     Trimmed mapping: mStorageStgNCap2AInp
#>     Trimmed mapping: mStorageStgNCap2AOut
#>     Trimmed mapping: mStorageStg2AInp
#>     Trimmed mapping: mStorageCinp2AInp
#>     Trimmed mapping: mStorageCout2AInp
#>     Trimmed mapping: mStorageOutCap2AInp
#>     Trimmed mapping: mStorageOutNCap2AInp
#>     Trimmed mapping: mStorageInpCap
#>     Trimmed mapping: mStorageInpNew
#>     Trimmed mapping: mStorageInpCapLo
#>     Trimmed mapping: mStorageInpCapUp
#>     Trimmed mapping: mStorageInpNewCapLo
#>     Trimmed mapping: mStorageInpNewCapUp
#>     Trimmed mapping: mStorageInpFixom
#>     Trimmed mapping: mStorageInpEac
#>     Trimmed mapping: mStorageInp2outLo
#>     Trimmed mapping: mStorageInp2outUp
#>     Trimmed mapping: mStorageInpStgCap
#>     Trimmed mapping: mStorageInp2stgLo
#>     Trimmed mapping: mStorageInp2stgUp
#>     Trimmed mapping: mStorageStgCap
#>     Trimmed mapping: mStorageStgNew
#>     Trimmed mapping: mStorageStgCapLo
#>     Trimmed mapping: mStorageStgCapUp
#>     Trimmed mapping: mStorageStgNewCapLo
#>     Trimmed mapping: mStorageStgNewCapUp
#>     Trimmed mapping: mStorageStgFixom
#>     Trimmed mapping: mStorageStgEac
#>     Trimmed mapping: mStorageDurationLo
#>     Trimmed mapping: mStorageDurationUp
#>     Trimmed mapping: mStorageOutCapLo
#>     Trimmed mapping: mStorageOutCapUp
#>     Trimmed mapping: mStorageOutNewCapLo
#>     Trimmed mapping: mStorageOutNewCapUp
#>     Trimmed mapping: mStorageOutRetLo
#>     Trimmed mapping: mStorageOutRetUp
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
#>     Trimmed mapping: mTechAInpCommSameTimeslice
#>     Trimmed mapping: mTechAInpCommAgg
#>     Trimmed mapping: mTechAInpCommAggTimeslice
#>     Trimmed mapping: mTechOutCommAgg
#>     Trimmed mapping: mTechOutCommAggTimeslice
#>     Trimmed mapping: mTechAOutCommSameTimeslice
#>     Trimmed mapping: mTechAOutCommAgg
#>     Trimmed mapping: mTechAOutCommAggTimeslice
#>     Trimmed mapping: mTechVarom
#>     Trimmed mapping: mDummyImport
#>     Trimmed mapping: mDummyExport
#>     Trimmed mapping: mDummyImportCost
#>     Trimmed mapping: mDummyExportCost
#>     Trimmed mapping: mvTradeIrAInp
#>     Trimmed mapping: mvTradeIrAInpTot
#>     Trimmed mapping: mvTradeIrAOut
#>     Trimmed mapping: mvTradeIrAOutTot
#>     Trimmed mapping: mImportRowUp
#>     Trimmed mapping: mImportRowCumUp
#>     Trimmed mapping: mExportRowUp
#>     Trimmed mapping: mStorageInpCommAgg
#>     Trimmed mapping: mStorageInpCommAggTimeslice
#>     Trimmed mapping: mStorageAInpCommSameTimeslice
#>     Trimmed mapping: mStorageAInpCommAgg
#>     Trimmed mapping: mStorageAInpCommAggTimeslice
#>     Trimmed mapping: mStorageOutCommAgg
#>     Trimmed mapping: mStorageOutCommAggTimeslice
#>     Trimmed mapping: mStorageAOutCommSameTimeslice
#>     Trimmed mapping: mStorageAOutCommAgg
#>     Trimmed mapping: mStorageAOutCommAggTimeslice
#>     Trimmed mapping: mTaxCost
#>     Trimmed mapping: mSubCost
#>     Trimmed mapping: mAggOut
#>     Trimmed mapping: mTechAfUp
#>     Trimmed mapping: mTechRampUp
#>     Trimmed mapping: mTechRampDown
#>     Trimmed mapping: mTechCommOutTimesliceTimesliceP
#>     Trimmed mapping: mTechCommAOutTimesliceTimesliceP
#>     Trimmed mapping: mTechOlifeInf
#>     Trimmed mapping: mTechAfcUp
#>     Trimmed mapping: mSupAvaUp
#>     Trimmed mapping: mSupReserveUp
#>     Trimmed mapping: meqTechRetiredNewCap
#>     Trimmed mapping: meqStorageRetiredNewCap
#>     Trimmed mapping: meqTradeRetiredNewCap
#>     Trimmed mapping: meqTechGrp2Sng
#>     Trimmed mapping: meqTechSng2Grp
#>     Trimmed mapping: meqTechGrp2Grp
#>     Trimmed mapping: meqTechShareInpLo
#>     Trimmed mapping: meqTechShareInpUp
#>     Trimmed mapping: meqTechShareOutLo
#>     Trimmed mapping: meqTechShareOutUp
#>     Trimmed mapping: meqTechAfsLo
#>     Trimmed mapping: meqTechAfsUp
#>     Trimmed mapping: meqTechActGrp
#>     Trimmed mapping: meqTechAfcOutLo
#>     Trimmed mapping: meqTechAfcOutUp
#>     Trimmed mapping: meqTechAfcInpLo
#>     Trimmed mapping: meqTechAfcInpUp
#>     Trimmed mapping: meqSupAvaLo
#>     Trimmed mapping: meqSupReserveLo
#>     Trimmed mapping: meqStorageAfLo
#>     Trimmed mapping: meqStorageInpUp
#>     Trimmed mapping: meqStorageInpLo
#>     Trimmed mapping: meqStorageOutUp
#>     Trimmed mapping: meqStorageOutLo
#>     Trimmed mapping: meqTradeFlowUp
#>     Trimmed mapping: meqTradeFlowLo
#>     Trimmed mapping: meqTradeIrAfUp
#>     Trimmed mapping: meqTradeIrAfLo
#>     Trimmed mapping: meqExportRowLo
#>     Trimmed mapping: meqImportRowUp
#>     Trimmed mapping: meqImportRowLo
#>     Trimmed mapping: meqBalUp
#>     Trimmed mapping: meqBalFx
#>     Trimmed mapping: mTechAct2AInp
#>     Trimmed mapping: mTechCap2AInp
#>     Trimmed mapping: mTechNCap2AInp
#>     Trimmed mapping: mTechCinp2AInp
#>     Trimmed mapping: mTechCout2AInp
#>     Trimmed mapping: mTechAct2AOut
#>     Trimmed mapping: mTechCap2AOut
#>     Trimmed mapping: mTechNCap2AOut
#>     Trimmed mapping: mTechCinp2AOut
#>     Trimmed mapping: mTechCout2AOut
#>     Total empty data elements: 221
#>   Phase 2: Identifying variables with empty domains...
#>     Trimmed variable (empty domain mapping): vTechRetiredStock
#>     Trimmed variable (empty domain mapping): vTechRetiredNewCap
#>     Trimmed variable (empty domain mapping): vTechAInp
#>     Trimmed variable (empty domain mapping): vTechAOut
#>     Trimmed variable (empty domain mapping): vStorageOutRetiredStock
#>     Trimmed variable (empty domain mapping): vStorageOutRetiredNewCap
#>     Trimmed variable (empty domain mapping): vStorageInpStockCap
#>     Trimmed variable (empty domain mapping): vStorageInpRetiredStock
#>     Trimmed variable (empty domain mapping): vStorageInpRetiredNewCap
#>     Trimmed variable (empty domain mapping): vStorageStgStockCap
#>     Trimmed variable (empty domain mapping): vStorageStgRetiredStock
#>     Trimmed variable (empty domain mapping): vStorageStgRetiredNewCap
#>     Trimmed variable (empty domain mapping): vTradeRetiredStock
#>     Trimmed variable (empty domain mapping): vTradeRetiredNewCap
#>     Trimmed variable (empty domain mapping): vTechRetCost
#>     Trimmed variable (empty domain mapping): vStorageRetCost
#>     Trimmed variable (empty domain mapping): vTradeRetCost
#>     Trimmed variable (empty domain mapping): vTechVarom
#>     Trimmed variable (empty domain mapping): vSupReserve
#>     Trimmed variable (empty domain mapping): vStorageAInp
#>     Trimmed variable (empty domain mapping): vStorageAOut
#>     Trimmed variable (empty domain mapping): vDummyImport
#>     Trimmed variable (empty domain mapping): vDummyExport
#>     Trimmed variable (empty domain mapping): vTaxCost
#>     Trimmed variable (empty domain mapping): vSubsCost
#>     Trimmed variable (empty domain mapping): vAggOutTot
#>     Trimmed variable (empty domain mapping): vDummyImportCost
#>     Trimmed variable (empty domain mapping): vDummyExportCost
#>     Trimmed variable (empty domain mapping): vStorageStgCap
#>     Trimmed variable (empty domain mapping): vStorageInpCap
#>     Trimmed variable (empty domain mapping): vStorageInpNewCap
#>     Trimmed variable (empty domain mapping): vStorageStgNewCap
#>     Trimmed variable (empty domain mapping): vStorageFixom
#>     Trimmed variable (empty domain mapping): vStorageVarom
#>     Trimmed variable (empty domain mapping): vTradeIrAInp
#>     Trimmed variable (empty domain mapping): vTradeIrAInpTot
#>     Trimmed variable (empty domain mapping): vTradeIrAOut
#>     Trimmed variable (empty domain mapping): vTradeIrAOutTot
#>     Trimmed variable (empty domain mapping): vTradeFixom
#>     Trimmed variable (empty domain mapping): vTotalUserCosts
#>     Total variables with empty domains: 40
#>   Phase 3: Identifying equations with empty domains...
#>     Trimmed equation (empty domain mapping): eqTechGrp2Sng
#>     Trimmed equation (empty domain mapping): eqTechSng2Grp
#>     Trimmed equation (empty domain mapping): eqTechGrp2Grp
#>     Trimmed equation (empty domain mapping): eqTechShareInpLo
#>     Trimmed equation (empty domain mapping): eqTechShareInpUp
#>     Trimmed equation (empty domain mapping): eqTechShareOutLo
#>     Trimmed equation (empty domain mapping): eqTechShareOutUp
#>     Trimmed equation (empty domain mapping): eqTechAInp
#>     Trimmed equation (empty domain mapping): eqTechAOut
#>     Trimmed equation (empty domain mapping): eqTechAfsLo
#>     Trimmed equation (empty domain mapping): eqTechAfsUp
#>     Trimmed equation (empty domain mapping): eqTechRampUp
#>     Trimmed equation (empty domain mapping): eqTechRampDown
#>     Trimmed equation (empty domain mapping): eqTechActGrp
#>     Trimmed equation (empty domain mapping): eqTechAfcOutLo
#>     Trimmed equation (empty domain mapping): eqTechAfcOutUp
#>     Trimmed equation (empty domain mapping): eqTechAfcInpLo
#>     Trimmed equation (empty domain mapping): eqTechAfcInpUp
#>     Trimmed equation (empty domain mapping): eqTechCapLo
#>     Trimmed equation (empty domain mapping): eqTechCapUp
#>     Trimmed equation (empty domain mapping): eqTechNewCapLo
#>     Trimmed equation (empty domain mapping): eqTechNewCapUp
#>     Trimmed equation (empty domain mapping): eqTechRetiredNewCap
#>     Trimmed equation (empty domain mapping): eqTechRetLo
#>     Trimmed equation (empty domain mapping): eqTechRetUp
#>     Trimmed equation (empty domain mapping): eqTechVarom
#>     Trimmed equation (empty domain mapping): eqTechRetCost
#>     Trimmed equation (empty domain mapping): eqStorageOutRetiredNewCap
#>     Trimmed equation (empty domain mapping): eqStorageOutRetUp
#>     Trimmed equation (empty domain mapping): eqStorageOutRetLo
#>     Trimmed equation (empty domain mapping): eqStorageInpStockCap
#>     Trimmed equation (empty domain mapping): eqStorageInpRetiredNewCap
#>     Trimmed equation (empty domain mapping): eqStorageInpRetUp
#>     Trimmed equation (empty domain mapping): eqStorageInpRetLo
#>     Trimmed equation (empty domain mapping): eqStorageStgStockCap
#>     Trimmed equation (empty domain mapping): eqStorageStgRetiredNewCap
#>     Trimmed equation (empty domain mapping): eqStorageStgRetUp
#>     Trimmed equation (empty domain mapping): eqStorageStgRetLo
#>     Trimmed equation (empty domain mapping): eqStorageRetCost
#>     Trimmed equation (empty domain mapping): eqTradeRetiredNewCap
#>     Trimmed equation (empty domain mapping): eqTradeRetUp
#>     Trimmed equation (empty domain mapping): eqTradeRetLo
#>     Trimmed equation (empty domain mapping): eqTradeRetCost
#>     Trimmed equation (empty domain mapping): eqSupAvaUp
#>     Trimmed equation (empty domain mapping): eqSupAvaLo
#>     Trimmed equation (empty domain mapping): eqSupReserve
#>     Trimmed equation (empty domain mapping): eqSupReserveUp
#>     Trimmed equation (empty domain mapping): eqSupReserveLo
#>     Trimmed equation (empty domain mapping): eqAggOutTot
#>     Trimmed equation (empty domain mapping): eqStorageAfLo
#>     Trimmed equation (empty domain mapping): eqStorageAInp
#>     Trimmed equation (empty domain mapping): eqStorageAOut
#>     Trimmed equation (empty domain mapping): eqStorageInpUp
#>     Trimmed equation (empty domain mapping): eqStorageInpLo
#>     Trimmed equation (empty domain mapping): eqStorageOutUp
#>     Trimmed equation (empty domain mapping): eqStorageOutLo
#>     Trimmed equation (empty domain mapping): eqStorageInpCap
#>     Trimmed equation (empty domain mapping): eqStorageInpCapLo
#>     Trimmed equation (empty domain mapping): eqStorageInpCapUp
#>     Trimmed equation (empty domain mapping): eqStorageInpNewCapLo
#>     Trimmed equation (empty domain mapping): eqStorageInpNewCapUp
#>     Trimmed equation (empty domain mapping): eqStorageInp2outLo
#>     Trimmed equation (empty domain mapping): eqStorageInp2outUp
#>     Trimmed equation (empty domain mapping): eqStorageInp2stgLo
#>     Trimmed equation (empty domain mapping): eqStorageInp2stgUp
#>     Trimmed equation (empty domain mapping): eqStorageStgCap
#>     Trimmed equation (empty domain mapping): eqStorageStgCapLo
#>     Trimmed equation (empty domain mapping): eqStorageStgCapUp
#>     Trimmed equation (empty domain mapping): eqStorageStgNewCapLo
#>     Trimmed equation (empty domain mapping): eqStorageStgNewCapUp
#>     Trimmed equation (empty domain mapping): eqStorageDurationLo
#>     Trimmed equation (empty domain mapping): eqStorageDurationUp
#>     Trimmed equation (empty domain mapping): eqStorageOutCapLo
#>     Trimmed equation (empty domain mapping): eqStorageOutCapUp
#>     Trimmed equation (empty domain mapping): eqStorageOutNewCapLo
#>     Trimmed equation (empty domain mapping): eqStorageOutNewCapUp
#>     Trimmed equation (empty domain mapping): eqStorageFixom
#>     Trimmed equation (empty domain mapping): eqStorageVarom
#>     Trimmed equation (empty domain mapping): eqTradeFlowUp
#>     Trimmed equation (empty domain mapping): eqTradeFlowLo
#>     Trimmed equation (empty domain mapping): eqTradeIrAfUp
#>     Trimmed equation (empty domain mapping): eqTradeIrAfLo
#>     Trimmed equation (empty domain mapping): eqExportRowUp
#>     Trimmed equation (empty domain mapping): eqExportRowLo
#>     Trimmed equation (empty domain mapping): eqImportRowUp
#>     Trimmed equation (empty domain mapping): eqImportRowLo
#>     Trimmed equation (empty domain mapping): eqImportRowResUp
#>     Trimmed equation (empty domain mapping): eqTradeCapLo
#>     Trimmed equation (empty domain mapping): eqTradeCapUp
#>     Trimmed equation (empty domain mapping): eqTradeNewCapLo
#>     Trimmed equation (empty domain mapping): eqTradeNewCapUp
#>     Trimmed equation (empty domain mapping): eqTradeFixom
#>     Trimmed equation (empty domain mapping): eqTradeIrAInp
#>     Trimmed equation (empty domain mapping): eqTradeIrAOut
#>     Trimmed equation (empty domain mapping): eqTradeIrAInpTot
#>     Trimmed equation (empty domain mapping): eqTradeIrAOutTot
#>     Trimmed equation (empty domain mapping): eqBalUp
#>     Trimmed equation (empty domain mapping): eqBalFx
#>     Trimmed equation (empty domain mapping): eqDummyImportCost
#>     Trimmed equation (empty domain mapping): eqDummyExportCost
#>     Trimmed equation (empty domain mapping): eqTaxCost
#>     Trimmed equation (empty domain mapping): eqSubsCost
#>     Trimmed equation (empty domain mapping): eqTotalUserCosts
#>     Total equations with empty domains: 103
#>   Phase 4: Verifying required elements...
#>     Untrimmed variable (used in equation): vTechRetiredNewCap
#>     Untrimmed variable (used in equation): vTechRetiredStock
#>     Untrimmed variable (used in equation): vStorageOutRetiredStock
#>     Untrimmed variable (used in equation): vStorageOutRetiredNewCap
#>     Untrimmed variable (used in equation): vTradeRetiredStock
#>     Untrimmed variable (used in equation): vTradeRetiredNewCap
#>     Untrimmed variable (used in equation): vStorageStgCap
#>     Untrimmed variable (used in equation): vStorageStgNewCap
#>     Untrimmed variable (used in equation): vStorageInpNewCap
#>     Untrimmed variable (used in equation): vDummyImport
#>     Untrimmed variable (used in equation): vAggOutTot
#>     Untrimmed variable (used in equation): vTradeIrAOutTot
#>     Untrimmed variable (used in equation): vDummyExport
#>     Untrimmed variable (used in equation): vTradeIrAInpTot
#>     Untrimmed variable (used in equation): vTechAInp
#>     Untrimmed variable (used in equation): vTechAOut
#>     Untrimmed variable (used in equation): vStorageAInp
#>     Untrimmed variable (used in equation): vStorageAOut
#>     Untrimmed variable (used in equation): vTechRetCost
#>     Untrimmed variable (used in equation): vStorageRetCost
#>     Untrimmed variable (used in equation): vTradeRetCost
#>     Untrimmed variable (used in equation): vTechVarom
#>     Untrimmed variable (used in equation): vStorageFixom
#>     Untrimmed variable (used in equation): vStorageVarom
#>     Untrimmed variable (used in equation): vTradeFixom
#>     Untrimmed variable (used in equation): vTaxCost
#>     Untrimmed variable (used in equation): vSubsCost
#>     Untrimmed variable (used in equation): vTotalUserCosts
#>     Untrimmed variable (used in equation): vDummyImportCost
#>     Untrimmed variable (used in equation): vDummyExportCost
#>     Untrimmed mapping (used in equation domain): mTechWeatherAfLo
#>     Untrimmed mapping (used in equation domain): mTechOlifeInf
#>     Untrimmed mapping (used in equation domain): mvTechRetiredNewCap
#>     Untrimmed mapping (used in equation domain): mvTechRetiredStock
#>     Untrimmed mapping (used in equation domain): mvStorageRetiredStock
#>     Untrimmed mapping (used in equation domain): mvStorageRetiredNewCap
#>     Untrimmed mapping (used in equation domain): mvTradeRetiredStock
#>     Untrimmed mapping (used in equation domain): mvTradeRetiredNewCap
#>     Untrimmed mapping (used in equation domain): mStorageStgCap
#>     Untrimmed mapping (used in equation domain): mStorageWeatherAfUp
#>     Untrimmed mapping (used in equation domain): mStorageStgNew
#>     Untrimmed mapping (used in equation domain): mStorageInpNew
#>     Untrimmed mapping (used in equation domain): mTradeOlifeInf
#>     Untrimmed mapping (used in equation domain): mDummyImport
#>     Untrimmed mapping (used in equation domain): mAggOut
#>     Untrimmed mapping (used in equation domain): mvTradeIrAOutTot
#>     Untrimmed mapping (used in equation domain): mRegionFamily
#>     Untrimmed mapping (used in equation domain): mDummyExport
#>     Untrimmed mapping (used in equation domain): mvTradeIrAInpTot
#>     Untrimmed mapping (used in equation domain): mTechAInpCommSameTimeslice
#>     Untrimmed mapping (used in equation domain): mvTechAInp
#>     Untrimmed mapping (used in equation domain): mTechAInpCommAgg
#>     Untrimmed mapping (used in equation domain): mTechAInpCommAggTimeslice
#>     Untrimmed mapping (used in equation domain): mTechOutCommAgg
#>     Untrimmed mapping (used in equation domain): mTechOutCommAggTimeslice
#>     Untrimmed mapping (used in equation domain): mTechAOutCommSameTimeslice
#>     Untrimmed mapping (used in equation domain): mvTechAOut
#>     Untrimmed mapping (used in equation domain): mTechAOutCommAgg
#>     Untrimmed mapping (used in equation domain): mTechAOutCommAggTimeslice
#>     Untrimmed mapping (used in equation domain): mStorageInpCommAgg
#>     Untrimmed mapping (used in equation domain): mStorageInpCommAggTimeslice
#>     Untrimmed mapping (used in equation domain): mStorageAInpCommSameTimeslice
#>     Untrimmed mapping (used in equation domain): mvStorageAInp
#>     Untrimmed mapping (used in equation domain): mStorageAInpCommAgg
#>     Untrimmed mapping (used in equation domain): mStorageAInpCommAggTimeslice
#>     Untrimmed mapping (used in equation domain): mStorageOutCommAgg
#>     Untrimmed mapping (used in equation domain): mStorageOutCommAggTimeslice
#>     Untrimmed mapping (used in equation domain): mStorageAOutCommSameTimeslice
#>     Untrimmed mapping (used in equation domain): mvStorageAOut
#>     Untrimmed mapping (used in equation domain): mStorageAOutCommAgg
#>     Untrimmed mapping (used in equation domain): mStorageAOutCommAggTimeslice
#>     Untrimmed mapping (used in equation domain): mTechRetCost
#>     Untrimmed mapping (used in equation domain): mStorageRetCost
#>     Untrimmed mapping (used in equation domain): mTradeRetCost
#>     Untrimmed mapping (used in equation domain): mTechVarom
#>     Untrimmed mapping (used in equation domain): mStorageFixom
#>     Untrimmed mapping (used in equation domain): mStorageVarom
#>     Untrimmed mapping (used in equation domain): mTradeFixom
#>     Untrimmed mapping (used in equation domain): mTaxCost
#>     Untrimmed mapping (used in equation domain): mSubCost
#>     Untrimmed mapping (used in equation domain): mvTotalUserCosts
#>     Untrimmed mapping (used in equation domain): mDummyImportCost
#>     Untrimmed mapping (used in equation domain): mDummyExportCost
#>     Untrimmed set (used in dimension): group
#>     Total elements untrimmed: 84
#>   Synced trim flags to folded equations
#> 
#> === NET Trim Results (after Phase 4) ===
#>   Sets trimmed:       6
#>   Parameters trimmed: 1
#>   Mappings trimmed:   160
#>   Variables trimmed:  10
#>   Equations trimmed:  103
#>   TOTAL trimmed:      280
#> 
#> Model Trimming Summary
#> ======================
#> sets: 6/27 trimmed (22.2%), 21 remaining
#> parameters: 1/218 trimmed (0.5%), 217 remaining
#> mappings: 160/324 trimmed (49.4%), 164 remaining
#> variables: 10/93 trimmed (10.8%), 83 remaining
#> equations: 103/153 trimmed (67.3%), 50 remaining

# Check trimming summary
get_trim_summary(model_optimized, format = "text")
#> [1] "Model Trimming Summary\n======================\nsets: 6/27 trimmed (22.2%), 21 remaining\nparameters: 1/218 trimmed (0.5%), 217 remaining\nmappings: 160/324 trimmed (49.4%), 164 remaining\nvariables: 10/93 trimmed (10.8%), 83 remaining\nequations: 103/153 trimmed (67.3%), 50 remaining"
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
