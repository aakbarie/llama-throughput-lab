# Llama Throughput Lab - Shiny App

A web-based interface for benchmarking and testing llama.cpp inference servers.

## Features

- **Configuration Panel**: Set model paths, server settings, and request parameters
- **Server Control**: Start/stop single servers or round-robin clusters with nginx
- **Quick Tests**: Single, concurrent, and round-robin request tests
- **Parameter Sweeps**: Threads, round-robin, and full parameter sweeps
- **Results Visualization**: Tables, throughput charts, and heatmaps
- **CSV Export**: Save and load sweep results

## Installation

```r
# Install dependencies
source("shiny/install_deps.R")
```

Or manually install:

```r
install.packages(c(
  "shiny", "bslib", "DT", "ggplot2",
  "jsonlite", "httr", "future", "future.apply", "promises"
))

# Optional but recommended
install.packages("processx")
```

## Running the App

From the project root:

```r
shiny::runApp("shiny")
```

Or from the shiny directory:

```r
shiny::runApp()
```

## Requirements

- R >= 4.0.0
- llama.cpp with llama-server binary
- nginx (for round-robin tests/sweeps)

## Usage

### 1. Configuration

1. Open the **Configuration** tab
2. The app auto-detects model files and llama-server binary
3. Adjust server settings (host, port, instances, parallel slots)
4. Configure request parameters (prompt, tokens, temperature)
5. Set sweep parameter lists for sweeps

### 2. Server Control

1. Open the **Server Control** tab
2. Click **Start Server** for single instance or **Start Cluster** for round-robin
3. Use **Health Check** to verify server status
4. View server logs at the bottom

### 3. Running Tests

1. Open the **Tests** tab
2. Select test type:
   - **Single Request**: Basic connectivity test
   - **Concurrent Requests**: Parallel load test
   - **Round-Robin**: Nginx load-balanced test
3. Click **Run Test** and view results

### 4. Running Sweeps

1. Open the **Sweeps** tab
2. Select sweep type:
   - **Threads Sweep**: Optimize thread configuration
   - **Round-Robin Sweep**: batch × ubatch × max_tokens × concurrency
   - **Full Sweep**: instances × parallel × batch × ubatch × concurrency
3. Click **Run Sweep** and monitor progress
4. View results in table, chart, or heatmap format

### 5. Results

1. Open the **Results** tab
2. Select a result file from previous sweeps
3. View data table and visualizations
4. Download CSV for further analysis

## Environment Variables

The app respects standard llama-throughput-lab environment variables:

| Variable | Purpose |
|----------|---------|
| `LLAMA_MODEL_PATH` | Path to GGUF model file |
| `LLAMA_MODEL_DIRS` | Search directories for models |
| `LLAMA_CPP_DIR` | Path to llama.cpp directory |
| `LLAMA_SERVER_BIN` | Path to llama-server binary |

## Output

Sweep results are saved to `results/` directory as CSV files:
- `results/round_robin/round_robin_YYYYMMDD_HHMMSS.csv`
- `results/threads/threads_YYYYMMDD_HHMMSS.csv`
- `results/full/full_YYYYMMDD_HHMMSS.csv`

## Architecture

```
shiny/
├── app.R                    # Main Shiny application
├── modules/
│   ├── utils.R              # Utility functions
│   ├── server_manager.R     # Server start/stop, nginx config
│   ├── test_runner.R        # Test execution logic
│   └── sweep_runner.R       # Sweep execution logic
├── DESCRIPTION              # Package dependencies
├── install_deps.R           # Dependency installer
└── README.md                # This file
```
