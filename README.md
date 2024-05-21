# WELCOME TO GRAVY

**Gravy** stands for _gradient analysis of vegetation_, and was
intended to provide general tools for gradient analysis. The main
purpose of **gravy** was to provide functions for fitting so-called
HOF models, but here it is superseded by CRAN package
[**eHOF**](https://CRAN.R-project.org/package=eHOF), and after release
of **eHOF** the package development stalled.

Some bits and pieces that may still be useful include:

* Derivatives of HOF functions and Beta diversity assessed as the rate
  of change of community composition (Oksanen & Tonteri, _J. Veg.
  Sci._ 6, 815-824, 1995).
* Assessment of niche overlap of HOF responses (Lawesson & Oksanen,
  _J. Veg. Sci._ 13, 279-290, 2002).
* Rescaling of ecological gradients either to constant rate of change
  of community composition or using Hill's measures of betadiversity.

## INSTALLATION

Package contains C source code and you need a C compiler to build and
install the package. If you have such tools and do not want to have
full git repository, you can use **remotes** package in **R**:
```r
install.packages("remotes")
remotes::install_github("jarioksa/gravy")
```
To install binary version without needing to compile C source code,
you can install the package from R-Universe:
```r
install.packages("gravy",
    repos =
	c("https://jarioksa.r-universe.dev","https://cloud.r-project.org"))
```
