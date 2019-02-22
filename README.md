# sintillate

## Table of contents

1. [Introduction](#introduction)
2. [Installation](#installation)
3. [Dependencies](#dependencies)
4. [Usage](#usage)
    1. [Aims and input data](#aims-and-input-data)
    2. [Basic function](#basic-function)
    3. [Incorporating uncertainty](#incorporating-uncertainty)
5. [Citing sintillate](#citing-sintillate)
6. [Issues](#issues)
7. [Author and license information](#author-and-license-information)
    
## Introduction

An R package for summarising vectors in a bivariate plane through their angle and vector length. This is primarily aimed at researchers studying pleiotropy and genotype-by-environment interactions in biology, but is generally applicable to any situation involving reaction norms or interaction plots.

The name is a pun on the verb *scintillate* and the standard abbreviation for the sine function used to estimate $\tau$ statistics.

As of January 2019 there is currently a manuscript to accompany the package and describe the method in detail which is veyr nearly at the stage of being publishable on bioarxiv. This text will change when that happens.

## Installation

This package is not yet on CRAN, so installation is easiest straight from GitHub using the package `devtools`. If necessary, install this with

```
install.packages("devtools")
```

Then you can install with

```
devtools::install_github("ellisztamas/sintillate")
```

## Dependencies

`sintillate` uses base R functions only.

## Usage

### Aims and input data

The aim is to characterise whether the effect of something, usually a gene, in one context is associated with an effect in the same or the opposite direction in a second context.

The starting point is to quantify an effect (or a vector of effects) of something interesting measured in two or more *contexts*, which might mean the same trait measured under different environmental conditions, or different traits measured on the same organisms. The best way to define your effect sizes will depend on the system and question, so it is assumed this has been done in a sensible way already by the user.

It is nevertheless important that effects can be distributed around zero, or else it is meaningless to assess whether effects can be in opposite directions. It is also vital that measurements be on the same scale in the two contexts. Three obvious examples of ways to define effect sizes would be:

1. Log relative fitness of an arbitrary reference allele relative to the other allele. The log transform makes the ratio symmetrical around zero.
2. Transform phenotypes to z-scores by subtracting the mean and dividing by standard error.
3. Change in allele frequency following selection in the two contexts.

### Basic function

Having defined vectors of effects `x` and `y`, the principle function is `sintillate`. Here is an example using simulated data of 100 pairs of values.

```
n <- 100
x <- rnorm(n)
y <- rnorm(n)
sintillate(x, y)
```

This returns a `data.frame` with six columns:

1. Original `x` values
2. Original `y` values
3. **Vector length** (the L2 norm). This is a measure of the overall magnitude of the effect.
4. Angle in radians
5. Angle in degrees
6. **$\tau$ statistic**. This measure the pleiotropic mechanism acting. Values of 1 indicate positive pleiotropy, -1 indicates antagonistic pleiotropy, and 0 indicates no pleiotropy.

### Incorporating uncertainty

There is always uncertainty in the estimates of `x` and `y`, and this will feed into uncertainty about $\tau$. We can account for this by drawing a distribution of plausible values for each pair of elements in `x` and `y` to generate a cloud of points in bivariate space, each associated with its own vector length, angle and $\tau$.

The best way to do this will depend on your data and system, but three obvious ways to do this would be:
1. Non-parametric bootstrap. Probably the most general solution.
2. Parametric boostrap. Simple to implement if standard errors are available, but assumes errors are multivariate normal, which they may not be.
3. Draw from a Bayesian posterior distribution. The most challenging to implement, but probably most statistically sound if done well.

However you do this, the aim to do create two matrices, each with a row for every element in the original vectors `x` and `y`, and a column for every boostrap or posterior draw. For example, this code simulates vectors of effect sizes in `x` and `y` with standard errors, then draws new values by parametric bootstrapping:

```
# Example parametric bootstrap
# Vectors of n values measured in x and y contexts
n <- 100
x <- rnorm(n)
y <- rnorm(n)
# Standard errors for each value in x and y
SE_x <- rnorm(n, mean=0, sd= 0.01)
SE_y <- rnorm(n, mean=0, sd= 0.01)

# Simulate parmetric bootrstrap draws
replicates <- 1000 # number of bootstrap draws
xmat <- x + t(SE_x * matrix(rnorm(n * replicates), ncol=n))
ymat <- y + t(SE_y * matrix(rnorm(n * replicates), ncol=n))
```

Then, you simply put these matrices into `sintillate` directly instead of vectors.

```
sint <- sintillate(xmat, ymat)
```

Calling `str(sint)` will show that the output returns a list of matrices rather than a data.frame of vectors. The `apply` function is useful to perform operations on whole rows of matrices. For example, to get the 95% confidence intervals for each observed value of $\tau$, call the function `quantile` on each row:

```
apply(sint$tau, 1, quantile, c(0.025, 0.975))
```

## Citing `sintillate`

If you use the package in your work, please cite:

> Ellis, Thomas James (2019), "sintillate: An R package for quantifying trade-offs, pleiotropy and genotype-by-environment interactions", available from www.github.com/ellisztamas/sintillate"

## Issues

Please report any bugs or requests that you have using the GitHub issue tracker.

## Author and license information

Tom Ellis (thomas.ellis@gmi.oeaw.ac.at)

`sintillate` is available under the MIT license. See LICENSE for more information.
