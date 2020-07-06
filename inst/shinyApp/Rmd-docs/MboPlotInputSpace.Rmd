**MboPlotInputSpace**

**Description**    
 The plot shows the input space (features) of the optimization. The top (bottom) shows numeric (discrete) features.


**Interpretation**   
  * Prior: features samples according to specified input space (equals random search)  
  * Posterior: feature values evaluated by the optimizer

**How to use**  
  * Comparing prior and posterior: enables to see how the optimizer searches comparared to random search. Shows which range of the single features look promising to the optimizer.  
  * Analyzing posterior: if the optimizer searches heavily on an upper/lower boundary of a specified range one may adjust the feature range as it seems the optimizer finds the minimum $f(x)$ at a different range of $x$.
  
**Modifications**
  * We can exclude the sampled observations from the specified prior of the features with by setting $include\_prior$ to $FALSE$ at the sidebar panel


  

