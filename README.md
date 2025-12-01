# SoftHybrid-paper

This repository contains the analysis code used in the manuscript:

> Shi, Y. *et al.*  **SoftHybrid: A Hybrid Imputation Algorithm Optimised for Single-Cell Proteomics Data.**  bioRxiv, 2025.

## Structure

- `code/` – R scripts for data preprocessing, imputation, benchmarking, and figure generation.
- `data/` – Search result data. Raw MS files are **not** stored here.
- `env/` – Session information and environment files (e.g. `sessionInfo`, `renv.lock`).

## Related R package

The SoftHybrid imputation method is implemented as an R package in a separate repository:

https://github.com/YixinShiProteomics/softHybridImpute
