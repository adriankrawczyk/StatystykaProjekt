# Comparative Analysis of PRS and GA in Function Optimization

## Overview
This repository contains an R implementation of a comparative study between **Pure Random Search (PRS)** and **Genetic Algorithm (GA)** for optimizing two benchmark functions: **Ackley** and **Alpine02**. The experiments analyze the efficiency of these algorithms in minimizing function values in **2D, 10D, and 20D spaces**.

## Authors
- **Adrian Krawczyk**
- **Damian Chłus**

## Introduction
Optimization of multidimensional functions is a crucial problem in computational intelligence and artificial intelligence. This project compares two fundamentally different approaches:
- **Pure Random Search (PRS)**: A simple algorithm that randomly samples points and keeps track of the best solution.
- **Genetic Algorithm (GA)**: A biologically inspired optimization method that employs selection, crossover, and mutation.

The study evaluates both algorithms in terms of accuracy and efficiency across multiple problem dimensions.

## Installation
To run this project, you need **R** installed along with the following packages:

```r
install.packages(c("smoof", "ecr", "ggplot2", "kableExtra", "stats", "dplyr", "gridExtra"))
```
