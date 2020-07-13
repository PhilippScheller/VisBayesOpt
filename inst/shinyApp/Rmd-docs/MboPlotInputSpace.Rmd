**MboPlotInputSpace**

**Description**    
 For each search space component, the plot shows the histograms over the evaluated values. The top (bottom) shows numeric (discrete) search space components.


**Interpretation**   
  * Prior: Values (artificially) sampled according to the specified input space (considering eventual transformations of the input space). This artificially shows the regions that would have been searched over by a random search.  
  * Posterior: Values actually evaluated by the optimizer.  

**How to use**  
  * Comparing prior and posterior: Enables to see how the optimizer searches compared to random search. Shows which range of a single search space component the optimizer focuses on.  
  * Analyzing posterior: If the optimizer searches heavily on an upper/lower boundary of a specified range one may adjust the range of the search space components as it seems the optimizer finds the minimum $f(x)$ at a different range of $x$.
  
**Modifications**
  * $include\_init\_design\_sampling\_distribution$: We can exclude the sampled observations from the specified prior of the search space components by setting $include\_init\_design\_sampling\_distribution = FALSE$ at the sidebar panel.


  

