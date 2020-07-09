**MboPlotDependencies**

**Description**    
 The matrix shows the pairwise scatter plots of the search space components.  
 

**Interpretation**   
  * Plots in lower triangle: Shows all pairwise scatterplots of the chosen search space components. The combination which leads to the minimum evaluated value of the objective function $f(x)$ is market with a red triangle. The points are either colored according to the value of $y$ (if $color\_y$) is TRUE. If $color\_y = FALSE$ the points are colored by the number of the iteration in which the point was sampled.  
  * Plots on diagonal: Show the frequency of the single search space components.  

**How to use**  
  * Detection of dependencies between search space components: If one of the scatterplots shows a string relationship between 2 search space components (e.g. high correlation) one might think of removing one of these components, since one already bears the entire information.  
  * Check sufficient design: If we choose $color\_y = FALSE$ one can see if the design is set up properly. This includes checking:  
    * if the points are randomly distributed over the 2 dimensional plot  
    * if the points are sufficient in their quantity (i.e. do they cover a good portion (not to much, not to less) of the space).  
    
**Modifications**
  * $search\_space\_parameters$: We can choose $search\_space\_parameters$ in the sidebar panel which are plotted pairwise in the matrix.  
  * $color\_y$: We can choose $color\_y=TRUE$ if we want the points to be colored according to the value of $y$. If $color\_y=FALSE$ the points are colored by the number of the iteration in which the point was sampled.


  

