# SpinalHDL Base Project

## Software

## Packet parser test

From the root folder:
```bash
cd sw/
# Ensure to start from clean state
make clean
# Compile unit tests
make tests
# Run tests
./tests.out
```

The expected output should end with:
```bash
[doctest] Status: SUCCESS!
```

**NOTE:** Unit tests compilation can take a bit and warnings are expected.

## Benchmarking Parser

### Build

From the root folder:
```bash
cd sw/
# Ensure to start from clean state
make clean
# Compile benchmarking program
make bench
```

### Benchmark

From the root folder:
```bash
# Go to benchmarking folder
cd bench/packetPopulate/
# Run benchmarks
bash launch.sh
```

**NOTE:** The experiment may take some time.

### Process results

From `bench/packetPopulate/`
```bash
# Compile results together
python3 compile_results.py results/
# Find compiled results here:
cat results/merged_output.csv
```
