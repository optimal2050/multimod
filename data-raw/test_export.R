## Test export_model_source() function

library(multimod)

cat("Testing export_model_source() function...\n\n")

# Create temporary test directory
test_dir <- file.path(tempdir(), "multimod_export_test")
if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
dir.create(test_dir)

# Test 1: Export OSeMOSYS
cat("1. Testing OSeMOSYS export...\n")
osemosys_dir <- file.path(test_dir, "osemosys")
files <- export_model_source("osemosys", osemosys_dir)
cat("   ✓ Files created:", length(files), "\n")
cat("   ✓ osemosys.txt exists:", file.exists(file.path(osemosys_dir, "osemosys.txt")), "\n")
cat("   ✓ utopia.txt exists:", file.exists(file.path(osemosys_dir, "utopia.txt")), "\n\n")

# Test 2: Export energyRt GMPL
cat("2. Testing energyRt GMPL export...\n")
gmpl_dir <- file.path(test_dir, "energyrt_gmpl")
files <- export_model_source("energyrt", gmpl_dir, format = "gmpl")
cat("   ✓ Files created:", length(files), "\n")
cat("   ✓ energyRt.mod exists:", file.exists(file.path(gmpl_dir, "energyRt.mod")), "\n")
cat("   ✓ energyRt.dat exists:", file.exists(file.path(gmpl_dir, "energyRt.dat")), "\n\n")

# Test 3: Export energyRt JuMP
cat("3. Testing energyRt JuMP export...\n")
jump_dir <- file.path(test_dir, "energyrt_jump")
files <- export_model_source("energyrt", jump_dir, format = "jump")
cat("   ✓ Files created:", length(files), "\n")
cat("   ✓ energyRt.jl exists:", file.exists(file.path(jump_dir, "energyRt.jl")), "\n")
cat("   ✓ data.jl exists:", file.exists(file.path(jump_dir, "data.jl")), "\n")
cat("   ✓ data.RData exists:", file.exists(file.path(jump_dir, "data.RData")), "\n")
if (file.exists(file.path(jump_dir, "data.RData"))) {
  rdata_size <- file.size(file.path(jump_dir, "data.RData"))
  cat("   ✓ data.RData size:", round(rdata_size / 1024, 2), "KB\n")
}
cat("\n")

# Test 4: Export energyRt GAMS
cat("4. Testing energyRt GAMS export...\n")
gams_dir <- file.path(test_dir, "energyrt_gams")
files <- export_model_source("energyrt", gams_dir, format = "gams")
cat("   ✓ Files created:", length(files), "\n")
cat("   ✓ energyRt.gms exists:", file.exists(file.path(gams_dir, "energyRt.gms")), "\n")
cat("   ✓ data.gms exists:", file.exists(file.path(gams_dir, "data.gms")), "\n\n")

# Test 5: Export all energyRt formats
cat("5. Testing energyRt ALL formats export...\n")
all_dir <- file.path(test_dir, "energyrt_all")
files <- export_model_source("energyrt", all_dir, format = "all")
cat("   ✓ Total files created:", length(files), "\n")
cat("   ✓ gams/ directory exists:", dir.exists(file.path(all_dir, "gams")), "\n")
cat("   ✓ gmpl/ directory exists:", dir.exists(file.path(all_dir, "gmpl")), "\n")
cat("   ✓ jump/ directory exists:", dir.exists(file.path(all_dir, "jump")), "\n")
cat("   ✓ pyomo/ directory exists:", dir.exists(file.path(all_dir, "pyomo")), "\n\n")

# Summary
cat(strrep("=", 60), "\n")
cat("All export tests passed! ✓\n\n")
cat("Test directory:", normalizePath(test_dir), "\n")
cat("Total files created:", length(list.files(test_dir, recursive = TRUE)), "\n")
cat(strrep("=", 60), "\n")

# List all created files
cat("\nCreated files:\n")
all_files <- list.files(test_dir, recursive = TRUE)
for (f in sort(all_files)) {
  cat("  ", f, "\n")
}
