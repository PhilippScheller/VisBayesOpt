# VisBayesOpt
Visualization of Bayesian Optimization runs from package [mlrMBO](https://mlrmbo.mlr-org.com/index.html)

# General
All functions are implemented in [R6](https://r6.r-lib.org) class design and require only 1 Object as input, a `final.opt.state` (returned by `mbo()`) from mlrMBO. `VisBayesOpt`is build on [mlr2](https://github.com/mlr-org/mlr) (since mlr3 is still under progress).

# Installation
```r
devtools::install_github("PhilippScheller/VisBayesOpt")
```

# Documentation
All functions come with a documentation which can be accessed in the help page, e.g. for `MboPlotProgress` we can inspect the help page by
```r
?MboPlotProgress
```

# Visualization via Shiny App
You can deploy the app on your local machine via the following function

```r
runAppLocal()
```

# Workflow
We use the exemplary `final.opt.state`object [mboObj2](inst/test-data/mboObj2.rds) which contains a mixed search space with numeric and discrete search space parameters. We show the worklfow for the R6 class `MboPlotSearchSpace()`.
Due to the object oriented style of R6, the user can easily apply the workflow to any other class from `VisBayesOpt`.

## Step 1 - Create a R6 object by calling the constructor with $new()
```r{}
library(VisBayesOpt)
mboObj2 = readRDS("inst/test-data/mboObj2.rds") #adjust to local file
mbo_plot_class = MboPlotSearchSpace$new(mboObj2)
  ```
## Step 2 - Plot search space by calling the $plot() function on the R6 object
At this step we can already create our first plot with the default `param_vals`. Every R6 class (`MboPlotProgress`, `MboPlotInputSpace`,...) comes with the unified $plot() function, i.e. we can call $plot() on every R6 object from `VisBayesOpt`.
```r
mbo_plot_class$plot()
```
## Step 3 - Modify `param_vals`
All plot classes are inherits from the overall class `MboPlot` which contains a setting method we can use in all classes. Once the constructor is called (in Step 1) the `param_vals`of the plot are initialized with default values. We can change these values by using the $set_param_vals() function.   
`$set_param_vals()`requires a list input with **all** parameters to be specified (e.g. if you want to set only 1 param value you need to pass the other values with their default values; this is no problems since the functions only come with 1 or 2 param vals).  

Now we do not want to include the points from the initial design so we set `include_init_design = FALSE` (to see the default values for all params of a class see the help page of the class), so we specify it by the function `$set_param_vals()` on the createt `mbo_plot_class` object:

```r
mbo_plot_class$set_param_vals(list(include_y = TRUE, include_init_design = FALSE))
```

By doing so we tell the object `mbo_plot_class` to change the parameters of the function `$plot()`. 

Afterwards we can simply create the new plot by calling the `$plot()` function again and see that the changes are applied.
```r
mbo_plot_class$plot()
```


  
  
