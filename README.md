
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qvirus

<!-- badges: start -->
<!-- badges: end -->

The goal of qvirus is to provide resources, tutorials, and code snippets
dedicated to exploring the intersection of quantum computing and
artificial intelligence (AI) in the context of HIV drug design.

## Installation

You can install the development version of qvirus from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("juanv66x/qvirus")
```

## Example

This is a basic example which shows you how to solve a common problem in
quantum computing.

``` r
library(qvirus)
## basic example code
```

Creating a Hadamard gate: `H(1)`.

    #> An object of class "sqgate"
    #> Slot "bit":
    #> [1] 1
    #> 
    #> Slot "M":
    #>              [,1]          [,2]
    #> [1,] 0.7071068+0i  0.7071068+0i
    #> [2,] 0.7071068+0i -0.7071068+0i
    #> 
    #> Slot "type":
    #> [1] "H"

Applying a gate to a quantum state.

``` r
library(qsimulatR)
H(1)*six_state(1)[[1]]
#>    ( 0.7071068 ) * |0> 
#>  + ( 0.7071068 ) * |1>
```
