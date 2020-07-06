**MboPlotSearchSpace**

**Description**    
 The plot shows the search space evaluated by the optimizer over the number of iterations. The top (bottom) shows numeric (discrete) features.  


**Interpretation**   
  * The color of the scatters correspong to the target $y=f(x)$.
  * Convergence of a single feature can be detected by observing the progress. Under the light of the target $y$ we can distinct between iterations where the optimizer explores or exploits.
  * The linear model shows if a linear tendency becomes apparent.

**How to use**  
  * linear trend : if optimizer shows poistive (negative) linear trend, i.e. no constant, then it may be sufficient to limit the range to larger (smaller) values to save cost.  
  * discrete params: if optimizer does rarely search for certain level of a feature (e.g. feature 'booster' has 3 levels [(i) 'gbtree', (ii) 'gblinear', (iii) 'dart'] but for higher $n$ the optimizer never evaluates e.g. 'gbtree' then it might be a benefit to limit the feature 'booster' to only 2 levels)
  
**Modifications**
  * We can exclude the $y=f(x)$ value from the plot by setting $include\_y$ to $FALSE$ at the sidebar panel


  

