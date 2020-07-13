# VisBayesOpt
Visualization of Bayesian Optimization runs from package [mlrMBO](https://mlrmbo.mlr-org.com/index.html)

# General
All functions are implemented in [R6](https://r6.r-lib.org) class design and require only 1 Object as input, a `final.opt.state` (returned by `mbo()`) from mlrMBO. `VisBayesOpt`is build on [mlr2](https://github.com/mlr-org/mlr) (since mlr3 is still under progress).

# Installation
```r
devtools::install_github("PhilippScheller/VisBayesOpt")
```

# Visualization via Shiny App
You can deploy the app on your local machine via the following function

```r
runAppLocal()
```

```{r setup, include = FALSE, cache = FALSE}
library(VisBayesOpt)
```

# Workflow
We use the exemplary `final.opt.state`object [mboObj2](inst/test-data/mboObj2.rds) which contains a mixed search space with numeric and discrete search space parameters. We show the worklfow for the R6 class `MboPlotSearchSpace()`.
Due to the object oriented style of R6, the user can easily apply the workflow to any other class from `VisBayesOpt`.

## Step 1 - Create a R6 object by calling the constructor with $new()
```r{}
library(VisBayesOpt)
mboObj2 = readRDS("inst/test-data/mboObj2.rds")
  mbo_plot_class = MboPlotSearchSpace$new(mboObj2)
  ```
## Step 2 - Plot search space by calling the $plot() function on the R6 object
At this step we can already create our first plot with the default `param_vals`. Every R6 class (`MboPlotProgress`, `MboPlotInputSpace`,...) comes with the unified $plot() function, i.e. we can call $plot() on every R6 object from `VisBayesOpt`.
```r
  mbo_plot_class$plot()
  ```
  
  
