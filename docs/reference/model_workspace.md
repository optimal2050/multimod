# Model Workspace and Data Management

Functions for creating, managing, saving, and loading multimod model
workspaces on disk.

## Key Functions

- [`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md):

  Save complete model workspace to disk with format options
  (csv/ipc/parquet)

- [`load_model()`](https://optimal2050.github.io/multimod/reference/load_model.md):

  Load model from workspace with optional lazy loading

- [`get_data()`](https://optimal2050.github.io/multimod/reference/get_data.md):

  Retrieve data with automatic lazy loading from disk

- [`update_parameter()`](https://optimal2050.github.io/multimod/reference/update_parameter.md):

  Low-level function to modify parameter data

- [`update_mapping()`](https://optimal2050.github.io/multimod/reference/update_mapping.md):

  Low-level function to modify mapping data

## Workflow

1.  Import or interpolate data into model (creates data.frames in
    parameters/mappings)

2.  Use
    [`update_parameter()`](https://optimal2050.github.io/multimod/reference/update_parameter.md)
    to set/modify parameter values

3.  Use
    [`save_model()`](https://optimal2050.github.io/multimod/reference/save_model.md)
    to persist model workspace to disk

4.  Use
    [`load_model()`](https://optimal2050.github.io/multimod/reference/load_model.md)
    to reload model from workspace later

5.  Use
    [`get_data()`](https://optimal2050.github.io/multimod/reference/get_data.md)
    to access data (loads from disk if needed)

## Storage Formats

- **csv** - Universal, human-readable (default)

- **ipc** - Arrow IPC/Feather (.arrow extension), fastest, compression:
  zstd level 15 (recommended)

- **parquet** - Popular Arrow format (but may have compatibility issues)
