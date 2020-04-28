# `cyclomort` package 
## Parametric periodic survival modeling in `R`

The `cyclomort` package provides tools for modeling and analysis of periodic mortality (or other time-to event) processes from right-censored data. The package was inspired by the need to detect annual seasonal patterns in mortality of wildlife, thus, the central assumption is that the period itself is known (e.g. 1 year, 24 hours, etc.), but the number, intensity and duration of the peaks of increased hazard are unknown.  The central function estimates these parameters from right-censored data. Other functions allow for simulation, visualization of fits, and *a priori* hypothesis testing tools.  

Examples are provided in the [package vignette](http://htmlpreview.github.io/?https://github.com/EliGurarie/cyclomort/blob/master/doc/cyclomort.html)

Also - for fun - a [blog post!](https://methodsblog.com/2019/10/31/modelling-mortalities/)

To install from GitHub:

```
library(devtools)
install_github("https://github.com/EliGurarie/cyclomort", vignettes = TRUE)
```

This project is fairly mature, and will soon be uploaded to CRAN, but GitHub is a good place to report issues for further development. 

## References

E. Gurarie, P. Thompson, A. Kelly, N. Larter, W. Fagan and K. Joly. 2020. For Everything There is a Season: Estimating periodic hazard functions with the cyclomort R package. *Methods in Ecology and Evolution*, 11(1):129-139. https://doi.org/10.1111/2041-210X.13305
