# Export the bundled energyRt model as a solver workspace

Writes the energyRt model shipped in `example_models` to a directory, as
a model workspace (see
[`save_model()`](https://optimal2050.github.io/multimod/r/reference/save_model.md))
with one sub-directory per requested backend under `solvers/`. The GAMS
source is the file the model was parsed from; the MathProg, JuMP and
Pyomo code is rendered from the parsed model with
[`write_gmpl()`](https://optimal2050.github.io/multimod/r/reference/write_gmpl.md),
[`write_jump()`](https://optimal2050.github.io/multimod/r/reference/write_jump.md)
and
[`write_pyomo()`](https://optimal2050.github.io/multimod/r/reference/write_pyomo.md).

## Usage

``` r
export_model_source(
  dataset = c("energyrt"),
  dir,
  format = "all",
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- dataset:

  Name of the dataset to export. Currently only `"energyrt"`.

- dir:

  Directory path for the workspace. Created if it does not exist.

- format:

  Which backend to export: `"gams"`, `"gmpl"`, `"jump"`, `"pyomo"`, or
  `"all"` (default).

- overwrite:

  Logical. If `TRUE`, existing solver directories are rewritten. Default
  is `FALSE`: a backend whose directory exists is skipped.

- verbose:

  Logical. If `TRUE`, print progress messages. Default is `TRUE`.

## Value

Invisibly, a character vector of the files written.

## Details

The workspace layout is the one every `solve_*()` function expects:

- `model.rds`, `sets/`, `mappings/`, `parameters/`: the saved model and
  its data (CSV), written once by
  [`save_model()`](https://optimal2050.github.io/multimod/r/reference/save_model.md).

- `solvers/gams/model.gms`: the GAMS source of the energyRt model.

- `solvers/gmpl/model.mod` and `data.dat`: MathProg, run with
  `glpsol -m model.mod -d data.dat` from that directory.

- `solvers/jump/model.jl` and `data.jl`: JuMP, run with
  `julia model.jl`.

- `solvers/pyomo/model.py` and `data.py`: Pyomo, run with
  `python model.py`.

- `README.txt`: provenance and licence of the model.

## Examples

``` r
# \donttest{
ws <- file.path(tempdir(), "energyrt_ws")
files <- export_model_source("energyrt", ws, format = "gmpl")
#> Created directory: C:\Users\admin\AppData\Local\Temp\RtmpgRRJWR/energyrt_ws
#> Wrote: model workspace (model.rds, sets/, mappings/, parameters/)
#> Wrote: README.txt
#> Wrote: solvers/gmpl/ (model.mod, data.dat)
#> Export complete: 4 file(s) written to C:\Users\admin\AppData\Local\Temp\RtmpgRRJWR\energyrt_ws
basename(files)
#> [1] "model.rds"  "README.txt" "model.mod"  "data.dat"  

model <- read_gmpl(file.path(ws, "solvers", "gmpl", "model.mod"),
                   file.path(ws, "solvers", "gmpl", "data.dat"))
#> Warning: Could not parse GMPL expression: 
#> (r,y) in mvTotalCost
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mvTechInp
#> Warning: Could not parse GMPL expression: 
#> (h,g,c) in mTechGroupComm
#> Warning: Could not parse GMPL expression: 
#> (h,cp,r,y,t) in mvTechOut
#> Warning: Could not parse GMPL expression: 
#> (h,gp,cp) in mTechGroupComm
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mvTechInp
#> Warning: Could not parse GMPL expression: 
#> (h,g,c) in mTechGroupComm
#> Warning: Could not parse GMPL expression: 
#> (h,cp,r,y,t) in mvTechOut
#> Warning: Could not parse GMPL expression: 
#> (h,gp,cp) in mTechGroupComm
#> Warning: Could not parse GMPL expression: 
#> (h,cp,r,y,t) in mvTechInp
#> Warning: Could not parse GMPL expression: 
#> (h,g,cp) in mTechGroupComm
#> Warning: Could not parse GMPL expression: 
#> (h,cp,r,y,t) in mvTechInp
#> Warning: Could not parse GMPL expression: 
#> (h,g,cp) in mTechGroupComm
#> Warning: Could not parse GMPL expression: 
#> (h,cp,r,y,t) in mvTechOut
#> Warning: Could not parse GMPL expression: 
#> (h,g,cp) in mTechGroupComm
#> Warning: Could not parse GMPL expression: 
#> (h,cp,r,y,t) in mvTechOut
#> Warning: Could not parse GMPL expression: 
#> (h,g,cp) in mTechGroupComm
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mTechCap2AInp
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mTechNCap2AInp
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mvTechPhaseOut
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mTechPho2AInp
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mvTechRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp,y) in mvTechRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mTechRet2AInp
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,tsp) in mTechAct2AInp
#> Warning: Could not parse GMPL expression: 
#> (h,c,cp,r,y,tsp) in mTechCinp2AInp
#> Warning: Could not parse GMPL expression: 
#> (h,c,cp,r,y,tsp) in mTechCout2AInp
#> Warning: Could not parse GMPL expression: 
#> (c,t,tsp) in mCommTimesliceOrParent
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mTechCap2AOut
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mTechNCap2AOut
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mvTechPhaseOut
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mTechPho2AOut
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mvTechRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp,y) in mvTechRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mTechRet2AOut
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,tsp) in mTechAct2AOut
#> Warning: Could not parse GMPL expression: 
#> (h,c,cp,r,y,tsp) in mTechCinp2AOut
#> Warning: Could not parse GMPL expression: 
#> (h,c,cp,r,y,tsp) in mTechCout2AOut
#> Warning: Could not parse GMPL expression: 
#> (c,t,tsp) in mCommTimesliceOrParent
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,h) in mTechWeatherAfLo
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,h) in mTechWeatherAfUp
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,h) in mTechWeatherAfsLo
#> Warning: Could not parse GMPL expression: 
#> (h,r,y,tsp) in mvTechAct
#> Warning: Could not parse GMPL expression: 
#> (t,tsp) in mTimesliceParentChildE
#> Warning: Could not parse GMPL expression: 
#> (h,r,y,tsp) in mvTechAct
#> Warning: Could not parse GMPL expression: 
#> (t,tsp) in mTimesliceParentChildE
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,h) in mTechWeatherAfsUp
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mvTechOut
#> Warning: Could not parse GMPL expression: 
#> (h,g,c) in mTechGroupComm
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,h,c) in mTechWeatherAfcLo
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,h,c) in mTechWeatherAfcUp
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,h,c) in mTechWeatherAfcLo
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,h,c) in mTechWeatherAfcUp
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp,y1) in mvTechRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp) in mTechNew
#> Warning: Could not parse GMPL expression: 
#> (h,r) in mTechOlifeInf
#> Warning: Could not parse GMPL expression: 
#> (yp,y) in mMilestoneNext
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp) in mTechSpan
#> Warning: Could not parse GMPL expression: 
#> y in mMilestoneFirst
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mvTechRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (yp,y) in mMilestoneNext
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp) in mTechSpan
#> Warning: Could not parse GMPL expression: 
#> (h,r,y,yp) in mvTechRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mvTechRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp,y) in mvTechRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mvTechRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp,y) in mvTechRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp,y1) in mvTechRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp) in mTechNew
#> Warning: Could not parse GMPL expression: 
#> (h,r) in mTechOlifeInf
#> Warning: Could not parse GMPL expression: 
#> (h,c) in mTechInpComm
#> Warning: Could not parse GMPL expression: 
#> (h,c) in mTechOutComm
#> Warning: Could not parse GMPL expression: 
#> (h,ts) in mTechTimeslice
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,ts) in mvTechAOut
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,ts) in mvTechAInp
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mvTechRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp,y) in mvTechRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp,y) in mvTechRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (yp,y) in mMilestoneNext
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp) in mTechSpan
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mTechNew
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mvTechRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (h,r,yp,y) in mvTechRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> y in mMilestoneFirst
#> Warning: Could not parse GMPL expression: 
#> (yp,y) in mMilestoneNext
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp) in mStorageSpan
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageNew
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> y in mMilestoneFirst
#> Warning: Could not parse GMPL expression: 
#> (yp,y) in mMilestoneNext
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp) in mStorageSpan
#> Warning: Could not parse GMPL expression: 
#> (yp,y) in mMilestoneNext
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp) in mStorageSpan
#> Warning: Could not parse GMPL expression: 
#> y in mMilestoneFirst
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,y,yp) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (yp,y) in mMilestoneNext
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp) in mStorageInpCap
#> Warning: Could not parse GMPL expression: 
#> y in mMilestoneFirst
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,y,yp) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (yp,y) in mMilestoneNext
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp) in mStorageStgCap
#> Warning: Could not parse GMPL expression: 
#> y in mMilestoneFirst
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,y,yp) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (yp,y) in mMilestoneNext
#> Warning: Could not parse GMPL expression: 
#> (z,yp) in mTradeSpan
#> Warning: Could not parse GMPL expression: 
#> y in mMilestoneFirst
#> Warning: Could not parse GMPL expression: 
#> (z,y) in mvTradeRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (yp,y) in mMilestoneNext
#> Warning: Could not parse GMPL expression: 
#> (z,yp) in mTradeSpan
#> Warning: Could not parse GMPL expression: 
#> (z,y) in mvTradeRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (z,yp,y) in mvTradeRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> y in mMilestoneFirst
#> Warning: Could not parse GMPL expression: 
#> (yp,y) in mMilestoneNext
#> Warning: Could not parse GMPL expression: 
#> (z,yp) in mTradeSpan
#> Warning: Could not parse GMPL expression: 
#> (z,y,yp) in mvTradeRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (z,y) in mvTradeRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (z,yp,y) in mvTradeRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (z,y) in mvTradeRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (z,yp,y) in mvTradeRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (z,y) in mvTradeRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (z,yp,y) in mvTradeRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,u) in mSupWeatherUp
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,u) in mSupWeatherLo
#> Warning: Could not parse GMPL expression: 
#> (u,c,r,y,ts) in mSupAva
#> Warning: Could not parse GMPL expression: 
#> (u,c,r,y,ts) in mSupAva
#> Warning: Could not parse GMPL expression: 
#> (d,c) in mDemComm
#> Warning: Could not parse GMPL expression: 
#> (cp,r,y,tsp) in mvOutTot
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,tsp) in mvOutTot
#> Warning: Could not parse GMPL expression: 
#> (t,tsp) in mTimesliceParentChildE
#> Warning: Could not parse GMPL expression: 
#> (cp,tsp) in mCommTimeslice
#> Warning: Could not parse GMPL expression: 
#> (c,cp) in mAggregateFactor
#> Warning: Could not parse GMPL expression: 
#> (h,c,cp,r,y,tsp) in mTechEmsFuel
#> Warning: Could not parse GMPL expression: 
#> (c,t,tsp) in mCommTimesliceOrParent
#> Warning: Could not parse GMPL expression: 
#> (h,cp) in mTechInpComm
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageNew
#> Warning: Could not parse GMPL expression: 
#> (s,cp,r,y,tp) in mvStorageInp
#> Warning: Could not parse GMPL expression: 
#> (s,cp,r,y,tp) in mvStorageOut
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageStgCap
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageNoStgCap
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,s) in mStorageWeatherAfLo
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageStgCap
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageNoStgCap
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,s) in mStorageWeatherAfUp
#> Warning: Could not parse GMPL expression: 
#> (s,cp,r,y,t) in mvStorageOut
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageOutCap2AInp
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageOutNCap2AInp
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageInpCap2AInp
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageInpNCap2AInp
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageStgCap2AInp
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageStgNCap2AInp
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStoragePhaseOut
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStoragePho2AInp
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageRet2AInp
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,tsp) in mStorageStg2AInp
#> Warning: Could not parse GMPL expression: 
#> (s,cp) in mStorageStgComm
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,tsp) in mStorageCinp2AInp
#> Warning: Could not parse GMPL expression: 
#> (s,cp) in mStorageInpComm
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,tsp) in mStorageCout2AInp
#> Warning: Could not parse GMPL expression: 
#> (s,cp) in mStorageOutComm
#> Warning: Could not parse GMPL expression: 
#> (c,t,tsp) in mCommTimesliceOrParent
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageOutCap2AOut
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageOutNCap2AOut
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageInpCap2AOut
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageInpNCap2AOut
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageStgCap2AOut
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageStgNCap2AOut
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStoragePhaseOut
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStoragePho2AOut
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mvStorageRetiredStock
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,t) in mStorageRet2AOut
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,tsp) in mStorageStg2AOut
#> Warning: Could not parse GMPL expression: 
#> (s,cp) in mStorageStgComm
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,tsp) in mStorageCinp2AOut
#> Warning: Could not parse GMPL expression: 
#> (s,cp) in mStorageInpComm
#> Warning: Could not parse GMPL expression: 
#> (s,c,r,y,tsp) in mStorageCout2AOut
#> Warning: Could not parse GMPL expression: 
#> (s,cp) in mStorageOutComm
#> Warning: Could not parse GMPL expression: 
#> (c,t,tsp) in mCommTimesliceOrParent
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageInpCap
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageNoInpCap
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,s) in mStorageWeatherInpAfUp
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageInpCap
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageNoInpCap
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,s) in mStorageWeatherInpAfLo
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,s) in mStorageWeatherOutAfUp
#> Warning: Could not parse GMPL expression: 
#> (w,r,rp) in mWeatherRegionAt
#> Warning: Could not parse GMPL expression: 
#> (w,s) in mStorageWeatherOutAfLo
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y1) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,r) in mStorageOlifeInf
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp) in mStorageNew
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y1) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,r) in mStorageOlifeInf
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp) in mStorageInpNew
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp,y1) in mvStorageRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (s,r) in mStorageOlifeInf
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp) in mStorageStgNew
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageStgNew
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageInpNew
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp) in mStorageStgNew
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp) in mStorageInpNew
#> Warning: Could not parse GMPL expression: 
#> (s,r,yp) in mStorageNew
#> Warning: Could not parse GMPL expression: 
#> (s,r) in mStorageOlifeInf
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageStgFixom
#> Warning: Could not parse GMPL expression: 
#> (s,r,y) in mStorageInpFixom
#> Warning: Could not parse GMPL expression: 
#> (c,ts) in mCommTimeslice
#> Warning: Could not parse GMPL expression: 
#> (s,c) in mStorageInpComm
#> Warning: Could not parse GMPL expression: 
#> (c,ts) in mCommTimeslice
#> Warning: Could not parse GMPL expression: 
#> (s,c) in mStorageOutComm
#> Warning: Could not parse GMPL expression: 
#> (c,ts) in mCommTimeslice
#> Warning: Could not parse GMPL expression: 
#> (s,c) in mStorageStgComm
#> Warning: Could not parse GMPL expression: 
#> (a,c,s,rd,y,t) in mvTradeIr
#> Warning: Could not parse GMPL expression: 
#> (a,s,rd) in mTradeRoutes
#> Warning: Could not parse GMPL expression: 
#> (a,c) in mTradeComm
#> Warning: Could not parse GMPL expression: 
#> (i,c,rd,y,t) in mImportRow
#> Warning: Could not parse GMPL expression: 
#> (i,c) in mImpComm
#> Warning: Could not parse GMPL expression: 
#> (a,c,rs,d,y,t) in mvTradeIr
#> Warning: Could not parse GMPL expression: 
#> (a,rs,d) in mTradeRoutes
#> Warning: Could not parse GMPL expression: 
#> (a,c) in mTradeComm
#> Warning: Could not parse GMPL expression: 
#> (e,c,rs,y,t) in mExportRow
#> Warning: Could not parse GMPL expression: 
#> (e,c) in mExpComm
#> Warning: Could not parse GMPL expression: 
#> (x,c,r,y,ts) in mExportRow
#> Warning: Could not parse GMPL expression: 
#> (x,c,r,y,ts) in mExportRow
#> Warning: Could not parse GMPL expression: 
#> (m,c,r,y,ts) in mImportRow
#> Warning: Could not parse GMPL expression: 
#> (m,c,r,y,ts) in mImportRow
#> Warning: Could not parse GMPL expression: 
#> (z,yp,y1) in mvTradeRetiredNewCap
#> Warning: Could not parse GMPL expression: 
#> (z,yp) in mTradeNew
#> Warning: Could not parse GMPL expression: 
#> z in mTradeOlifeInf
#> Warning: Could not parse GMPL expression: 
#> (z,c,s,d,y,t) in mvTradeIr
#> Warning: Could not parse GMPL expression: 
#> (z,yp) in mTradeNew
#> Warning: Could not parse GMPL expression: 
#> z in mTradeOlifeInf
#> Warning: Could not parse GMPL expression: 
#> (z,c,s,r,y,ts) in mvTradeIr
#> Warning: Could not parse GMPL expression: 
#> (z,ts) in mTradeTimeslice
#> Warning: Could not parse GMPL expression: 
#> (z,c) in mTradeComm
#> Warning: Could not parse GMPL expression: 
#> (z,s,r) in mTradeRoutes
#> Warning: Could not parse GMPL expression: 
#> (z,c,r,d,y,ts) in mvTradeIr
#> Warning: Could not parse GMPL expression: 
#> (z,ts) in mTradeTimeslice
#> Warning: Could not parse GMPL expression: 
#> (z,c) in mTradeComm
#> Warning: Could not parse GMPL expression: 
#> (z,r,d) in mTradeRoutes
#> Warning: Could not parse GMPL expression: 
#> (z,cp) in mTradeComm
#> Warning: Could not parse GMPL expression: 
#> (z,cp,r,d,y,t) in mvTradeIr
#> Warning: Could not parse GMPL expression: 
#> (z,c,r,d,y,t) in mTradeIrCsrc2Ainp
#> Warning: Could not parse GMPL expression: 
#> (z,cp) in mTradeComm
#> Warning: Could not parse GMPL expression: 
#> (z,cp,s,r,y,t) in mvTradeIr
#> Warning: Could not parse GMPL expression: 
#> (z,c,s,r,y,t) in mTradeIrCdst2Ainp
#> Warning: Could not parse GMPL expression: 
#> (z,cp) in mTradeComm
#> Warning: Could not parse GMPL expression: 
#> (z,cp,r,d,y,t) in mvTradeIr
#> Warning: Could not parse GMPL expression: 
#> (z,c,r,d,y,t) in mTradeIrCsrc2Aout
#> Warning: Could not parse GMPL expression: 
#> (z,cp) in mTradeComm
#> Warning: Could not parse GMPL expression: 
#> (z,cp,s,r,y,t) in mvTradeIr
#> Warning: Could not parse GMPL expression: 
#> (z,c,s,r,y,t) in mTradeIrCdst2Aout
#> Warning: Could not parse GMPL expression: 
#> (c,t,tsp) in mCommTimesliceOrParent
#> Warning: Could not parse GMPL expression: 
#> (a,c,r,y,tsp) in mvTradeIrAInp
#> Warning: Could not parse GMPL expression: 
#> (c,t,tsp) in mCommTimesliceOrParent
#> Warning: Could not parse GMPL expression: 
#> (a,c,r,y,tsp) in mvTradeIrAOut
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mvOutTot
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mvInpTot
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mDummyImport
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mSupOutTot
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mEmsFuelTot
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mAggOut
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mTechOutTot
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mStorageOutTot
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mImport
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mvTradeIrAOutTot
#> Warning: Could not parse GMPL expression: 
#> (t,tsp) in mTimesliceFamily
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,tsp) in mvOutTot
#> Warning: Could not parse GMPL expression: 
#> (r,rp) in mRegionFamily
#> Warning: Could not parse GMPL expression: 
#> (c,rp,y,t) in mvOutTot
#> Warning: Could not parse GMPL expression: 
#> (c,t) in mCommTimeslice
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mvDemInp
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mDummyExport
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mTechInpTot
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mStorageInpTot
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mExport
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,t) in mvTradeIrAInpTot
#> Warning: Could not parse GMPL expression: 
#> (t,tsp) in mTimesliceFamily
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,tsp) in mvInpTot
#> Warning: Could not parse GMPL expression: 
#> (r,rp) in mRegionFamily
#> Warning: Could not parse GMPL expression: 
#> (c,rp,y,t) in mvInpTot
#> Warning: Could not parse GMPL expression: 
#> (c,t) in mCommTimeslice
#> Warning: Could not parse GMPL expression: 
#> (u,c) in mSupComm
#> Warning: Could not parse GMPL expression: 
#> (u,c,r,y,t) in mSupAva
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mvTechInp
#> Warning: Could not parse GMPL expression: 
#> (h,c) in mTechInpCommSameTimeslice
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,tsp) in mvTechInp
#> Warning: Could not parse GMPL expression: 
#> (h,c,tsp,t) in mTechInpCommAggTimeslice
#> Warning: Could not parse GMPL expression: 
#> (h,c) in mTechInpCommAgg
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mvTechAInp
#> Warning: Could not parse GMPL expression: 
#> (h,c) in mTechAInpCommSameTimeslice
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,tsp) in mvTechAInp
#> Warning: Could not parse GMPL expression: 
#> (h,c,tsp,t) in mTechAInpCommAggTimeslice
#> Warning: Could not parse GMPL expression: 
#> (h,c) in mTechAInpCommAgg
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mvTechOut
#> Warning: Could not parse GMPL expression: 
#> (h,c) in mTechOutCommSameTimeslice
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,tsp) in mvTechOut
#> Warning: Could not parse GMPL expression: 
#> (h,c,tsp,t) in mTechOutCommAggTimeslice
#> Warning: Could not parse GMPL expression: 
#> (h,c) in mTechOutCommAgg
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,t) in mvTechAOut
#> Warning: Could not parse GMPL expression: 
#> (h,c) in mTechAOutCommSameTimeslice
#> Warning: Could not parse GMPL expression: 
#> (h,c,r,y,tsp) in mvTechAOut
#> Warning: Could not parse GMPL expression: 
#> (h,c,tsp,t) in mTechAOutCommAggTimeslice
#> Warning: Could not parse GMPL expression: 
#> (h,c) in mTechAOutCommAgg
#> Warning: Could not parse GMPL expression: 
#> (o,c,r,y,t) in mvStorageInp
#> Warning: Could not parse GMPL expression: 
#> (o,c) in mStorageInpCommSameTimeslice
#> Warning: Could not parse GMPL expression: 
#> (o,c,r,y,tsp) in mvStorageInp
#> Warning: Could not parse GMPL expression: 
#> (o,c,tsp,t) in mStorageInpCommAggTimeslice
#> Warning: Could not parse GMPL expression: 
#> (o,c) in mStorageInpCommAgg
#> Warning: Could not parse GMPL expression: 
#> (o,c,r,y,t) in mvStorageAInp
#> Warning: Could not parse GMPL expression: 
#> (o,c) in mStorageAInpCommSameTimeslice
#> Warning: Could not parse GMPL expression: 
#> (o,c,r,y,tsp) in mvStorageAInp
#> Warning: Could not parse GMPL expression: 
#> (o,c,tsp,t) in mStorageAInpCommAggTimeslice
#> Warning: Could not parse GMPL expression: 
#> (o,c) in mStorageAInpCommAgg
#> Warning: Could not parse GMPL expression: 
#> (o,c,r,y,t) in mvStorageOut
#> Warning: Could not parse GMPL expression: 
#> (o,c) in mStorageOutCommSameTimeslice
#> Warning: Could not parse GMPL expression: 
#> (o,c,r,y,tsp) in mvStorageOut
#> Warning: Could not parse GMPL expression: 
#> (o,c,tsp,t) in mStorageOutCommAggTimeslice
#> Warning: Could not parse GMPL expression: 
#> (o,c) in mStorageOutCommAgg
#> Warning: Could not parse GMPL expression: 
#> (o,c,r,y,t) in mvStorageAOut
#> Warning: Could not parse GMPL expression: 
#> (o,c) in mStorageAOutCommSameTimeslice
#> Warning: Could not parse GMPL expression: 
#> (o,c,r,y,tsp) in mvStorageAOut
#> Warning: Could not parse GMPL expression: 
#> (o,c,tsp,t) in mStorageAOutCommAggTimeslice
#> Warning: Could not parse GMPL expression: 
#> (o,c) in mStorageAOutCommAgg
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,ts) in mDummyImport
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,ts) in mDummyImport
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,ts) in mDummyExport
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,ts) in mDummyExport
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,ts) in mvOutTot
#> Warning: Could not parse GMPL expression: 
#> (c,ts) in mCommTimeslice
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,ts) in mvInpTot
#> Warning: Could not parse GMPL expression: 
#> (c,ts) in mCommTimeslice
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,ts) in mvBalance
#> Warning: Could not parse GMPL expression: 
#> (c,ts) in mCommTimeslice
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,ts) in mvOutTot
#> Warning: Could not parse GMPL expression: 
#> (c,ts) in mCommTimeslice
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,ts) in mvInpTot
#> Warning: Could not parse GMPL expression: 
#> (c,ts) in mCommTimeslice
#> Warning: Could not parse GMPL expression: 
#> (c,r,y,ts) in mvBalance
#> Warning: Could not parse GMPL expression: 
#> (c,ts) in mCommTimeslice
#> Warning: Could not parse GMPL expression: 
#> (u,r,y) in mvSupCost
#> Warning: Could not parse GMPL expression: 
#> (u,r,y) in mvSupCost
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mTechEac
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mTechEac
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mTechRetCost
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mTechRetCost
#> Warning: Could not parse GMPL expression: 
#> (o,r,y) in mStorageRetCost
#> Warning: Could not parse GMPL expression: 
#> (o,r,y) in mStorageRetCost
#> Warning: Could not parse GMPL expression: 
#> (a,r,y) in mTradeRetCost
#> Warning: Could not parse GMPL expression: 
#> (a,r,y) in mTradeRetCost
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mTechFixom
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mTechFixom
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mTechVarom
#> Warning: Could not parse GMPL expression: 
#> (h,r,y) in mTechVarom
#> Warning: Could not parse GMPL expression: 
#> (o,r,y) in mStorageEac
#> Warning: Could not parse GMPL expression: 
#> (o,r,y) in mStorageEac
#> Warning: Could not parse GMPL expression: 
#> (o,r,y) in mStorageFixom
#> Warning: Could not parse GMPL expression: 
#> (o,r,y) in mStorageFixom
#> Warning: Could not parse GMPL expression: 
#> (o,r,y) in mStorageVarom
#> Warning: Could not parse GMPL expression: 
#> (o,r,y) in mStorageVarom
#> Warning: Could not parse GMPL expression: 
#> (i,r,y) in mImportRowCost
#> Warning: Could not parse GMPL expression: 
#> (i,r,y) in mImportRowCost
#> Warning: Could not parse GMPL expression: 
#> (e,r,y) in mExportRowCost
#> Warning: Could not parse GMPL expression: 
#> (e,r,y) in mExportRowCost
#> Warning: Could not parse GMPL expression: 
#> (a,r,y) in mTradeEac
#> Warning: Could not parse GMPL expression: 
#> (a,r,y) in mTradeEac
#> Warning: Could not parse GMPL expression: 
#> (a,r,y) in mTradeFixom
#> Warning: Could not parse GMPL expression: 
#> (a,r,y) in mTradeFixom
#> Warning: Could not parse GMPL expression: 
#> (a,r,y) in mImportIrCost
#> Warning: Could not parse GMPL expression: 
#> (a,r,y) in mImportIrCost
#> Warning: Could not parse GMPL expression: 
#> (a,r,y) in mExportIrCost
#> Warning: Could not parse GMPL expression: 
#> (a,r,y) in mExportIrCost
#> Warning: Could not parse GMPL expression: 
#> (c,r,y) in mTaxCost
#> Warning: Could not parse GMPL expression: 
#> (c,r,y) in mTaxCost
#> Warning: Could not parse GMPL expression: 
#> (c,r,y) in mSubCost
#> Warning: Could not parse GMPL expression: 
#> (c,r,y) in mSubCost
#> Warning: Could not parse GMPL expression: 
#> (r,y) in mvTotalUserCosts
#> Warning: Could not parse GMPL expression: 
#> (c,r,y) in mDummyImportCost
#> Warning: Could not parse GMPL expression: 
#> (c,r,y) in mDummyImportCost
#> Warning: Could not parse GMPL expression: 
#> (c,r,y) in mDummyExportCost
#> Warning: Could not parse GMPL expression: 
#> (c,r,y) in mDummyExportCost
unlink(ws, recursive = TRUE)
# }
```
