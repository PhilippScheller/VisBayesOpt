**MboPlotSearchSpace**

**Description**    
 The plot shows the values of the different search space components (y-axis) that have been evaluated by the optimizer over the number of iterations. The top (bottom) shows numeric (discrete) features.  


**Interpretation**   
  * The color of the scatters corresponds to the value of the objective function $y=f(x)$.
  * Convergence of a single search space component can be detected by observing the progress. Under the light of the target $y$ we can distinct between iterations where the optimizer explores or exploits.
  * The linear model shows if a linear tendency becomes apparent.

**How to use**  
  * linear trend : If optimizer shows a positive (negative) linear trend, i.e. no constant, then it may be sufficient to limit the search space to larger (smaller) values in future runs.  
  * discrete params: If optimizer does rarely search for certain level of a search space component (e.g. search space component 'temperature' has 3 levels [(i) 'low', (ii) 'medium', (iii) 'high'] but for large $n$ the optimizer never evaluates e.g. 'low' then it might be a benefit to limit the component 'temperature' to only 2 levels)
  
**Modifications**
  * We can exclude the $y=f(x)$ value from the plot by setting $include\_y$ to $FALSE$ at the sidebar panel


  

