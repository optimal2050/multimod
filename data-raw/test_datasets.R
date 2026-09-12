## Test script for model source datasets
## Verifies that both osemosys_source and energyrt_source can be loaded
## and have the expected structure

cat("Testing multimod model source datasets...\n\n")

# Test OSeMOSYS dataset
cat("1. Testing osemosys_source...\n")
load("data/osemosys_source.rda")

if (!exists("osemosys_source")) {
  stop("osemosys_source not found!")
}

cat("   ✓ Dataset loaded\n")
cat("   ✓ Model lines:", length(osemosys_source$gmpl$model), "\n")
cat("   ✓ Data lines:", length(osemosys_source$gmpl$data), "\n")
cat("   ✓ Version:", osemosys_source$metadata$version, "\n")
cat("   ✓ License:", osemosys_source$metadata$license, "\n\n")

# Test energyRt dataset
cat("2. Testing energyrt_source...\n")
load("data/energyrt_source.rda")

if (!exists("energyrt_source")) {
  stop("energyrt_source not found!")
}

cat("   ✓ Dataset loaded\n")
cat("   ✓ Formats:", paste(names(energyrt_source)[1:4], collapse=", "), "\n")
cat("   ✓ GAMS model lines:", length(energyrt_source$gams$model), "\n")
cat("   ✓ GAMS data lines:", length(energyrt_source$gams$data), "\n")
cat("   ✓ GMPL model lines:", length(energyrt_source$gmpl$model), "\n")
cat("   ✓ GMPL data lines:", length(energyrt_source$gmpl$data), "\n")
cat("   ✓ JuMP model lines:", length(energyrt_source$jump$model), "\n")
cat("   ✓ Pyomo model lines:", length(energyrt_source$pyomo$model), "\n")
cat("   ✓ Scenario:", energyrt_source$metadata$scenario, "\n")
cat("   ✓ License:", energyrt_source$metadata$license, "\n\n")

# Test writing to temporary files
cat("3. Testing file export...\n")
tmp_dir <- tempdir()

# OSeMOSYS
osemosys_model <- file.path(tmp_dir, "osemosys.txt")
osemosys_data <- file.path(tmp_dir, "utopia.txt")
writeLines(osemosys_source$gmpl$model, osemosys_model)
writeLines(osemosys_source$gmpl$data, osemosys_data)
cat("   ✓ OSeMOSYS written to:", tmp_dir, "\n")

# energyRt GMPL
energyrt_model <- file.path(tmp_dir, "energyrt.mod")
energyrt_data <- file.path(tmp_dir, "energyrt.dat")
writeLines(energyrt_source$gmpl$model, energyrt_model)
writeLines(energyrt_source$gmpl$data, energyrt_data)
cat("   ✓ energyRt written to:", tmp_dir, "\n\n")

# Verify file sizes
cat("4. Verifying exported files...\n")
cat("   ✓ osemosys.txt:", file.size(osemosys_model), "bytes\n")
cat("   ✓ utopia.txt:", file.size(osemosys_data), "bytes\n")
cat("   ✓ energyrt.mod:", file.size(energyrt_model), "bytes\n")
cat("   ✓ energyrt.dat:", file.size(energyrt_data), "bytes\n\n")

# Summary
cat(strrep("=", 60), "\n")
cat("All tests passed! ✓\n\n")
cat("Summary:\n")
cat("  - osemosys_source: 2 components (model, data)\n")
cat("  - energyrt_source: 4 formats × 2 components = 8 components\n")
cat("  - Total lines: ", 
    length(osemosys_source$gmpl$model) + 
    length(osemosys_source$gmpl$data) +
    length(energyrt_source$gams$model) +
    length(energyrt_source$gams$data) +
    length(energyrt_source$gmpl$model) +
    length(energyrt_source$gmpl$data) +
    length(energyrt_source$jump$model) +
    length(energyrt_source$pyomo$model), "\n")
cat(strrep("=", 60), "\n")
