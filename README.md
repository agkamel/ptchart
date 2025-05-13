
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ptchart

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

## Overview

**ptchart** is an R package that provides precision teachers and
behavior analysts tools to compute and work behavioral measures related
to precision teaching and to generate standardized charts.

There are two main and more global functions for quick computations:

- `ptstat()` for computing all behavioral measures.
- `ptchart()` for generating a Standardized Celeration Chart.

There are multiple smaller but specialized functions to be used with
vectors, which some are used within `ptstat()`:

- `celeration()`
- `bounce_total()`
- `accuracy_ratio()`
- etc.

Please note that this package is currently in active development. If you
find any bug or if you have a specific request, you can open a new
issue.

## Installation

Install the **ptchart** package via CRAN:

``` r
install.packages("ptchart")
```

Install the development version via GitHub:

``` r
# install.packages("devtools")
devtools::install_github("agkamel/ptchart")
```

## A basic example

First, we first need to load the package.

``` r
library(ptchart)
```

To help the user understand the package, the fictional dataset
`example_pt_data` is provided with the package.

``` r
example_pt_data
```

To calculate behavioral measures of behavior, we provide the relevant
information into the `ptstat()` function.

``` r
measures <- ptstat(example_pt_data,
                   day = jour,
                   freq = frequence,
                   phase = phase)
```

The output of `ptstat()` is a S3 object that contains a lot of useful
information like celeration, bounce, jump, turn and accuracy values.
Other variables like time floor are also calculated.

``` r
measures
```

Now, we may want to see the data on a chart that respect the
Standardized Celeration Chart conventions. To do so, we use the
`ptchart()` function.

``` r
ptchart(measures)
```

## More details on computing measures with `ptstat()`

### Dates and days input scenarios

There are multiple input scenarios when using `ptstat()` depending of
the data you have.

If you have observation dates, you can provide them to the argument
`date`. Dates are be automatically converted to days using the earliest
date available. If a specific `date_zero` is provided, dates are
converted using this date as the day 0.

If you don’t have observations dates, you can provide days number to the
argument `day`. Because dates contains more information than days, if
inputs are provided for both `date` and `day`, the priority is given to
`date` and `day` is ignored.

### Count of response, timing and frequency input scenarios

If you have count of responses and observation timings, you can provide
them to arguments `count` and `time`. Both argument contains the most
information: time floors and frequencies are automatically calculated
with these variables. The same reasoning is applied if you have the
count of non-target responses provided to the argument `count_err`.

If you directly have frequencies of a target behavior, you can provide
it to the argument `freq` (or `freq_err` for a non-target behavior).
Because `count` and `time` contain more information, if they are already
provided, the priority is given to them and `freq` (or `freq_err`) is
ignored.

### Phases

If your data contains multiple intervention phases, you can provide them
to the argument `phase`. Calculations will then be made by grouping data
by phases of intervention. If argument `phase` is not provided, data
will be considered as a single phase and calculation will be made on all
data.

## More details on generating chart with `ptchart()`

This section is currently in work. More details are coming.

## More details on specialized functions

There are three types of specialized functions: (a) those that return a
single value, (b) those that return an atomic vector (or multiple
values). If you are familiar with the **dplyr** package, the former can
be used within `summarise()` and the latter within `mutate()`.

| Scalar output | Vector output |
|----|----|
| `b1()`, `b0()` | `predicted_values()`, `res()` |
| `accuracy()` | `accuracy_ratio()` |
| `bounce_up()`, `bounce_down()`, `bounce_total()` |  |
| `celeration()`, `celeration_0()` |  |
