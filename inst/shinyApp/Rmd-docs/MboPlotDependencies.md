**MboPlotDependencies**

**Description**    
 The matrix shows the pairwise scatter plots of the values of the chosen search space components.  
 

**Interpretation**   
  * Plots in lower triangle: Shows all pairwise scatterplots of the chosen search space components. The combination which leads to the minimum evaluated value of the objective function $f(x)$ is market with a red triangle. The points are either colored according to the value of $y$ (if $color\_y$) is TRUE. If $color\_y = FALSE$ the points' color indicates the iteration at which the point was evaluated.  
  * Plots on diagonal: Show the histograms over the evaluated values of the chosen search space components.  

**How to use**  
  * Detection of dependencies between search space components: If one of the scatterplots shows a strong relationship between two search space components (e.g. high correlation) conditioned on the target, there might be the option to extract rules to choose the value of one search space component depending on the value of the other one.  
    
**Modifications**
  * $search\_space\_parameters$: We can choose $search\_space\_parameters$ in the sidebar panel which are plotted pairwise in the matrix.  
  * $color\_y$: We can choose $color\_y=TRUE$ if we want the points to be colored according to the value of the optimization target $y$. If $color\_y=FALSE$ the points are colored by the number of the iteration in which the point was evaluated.


  

