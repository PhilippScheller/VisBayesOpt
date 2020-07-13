**MboPlotOptPath**

**Description**    
 The plot shows the surrogate model for the chosen iteration.
 

**Interpretation**   
  * 1D search space: Shows the graph of the surrogate model and the acquisition function (infill criterion).  
  * Higher dimensional search space: Shows the Partial Dependence Plot [PDP](https://christophm.github.io/interpretable-ml-book/pdp.html) of the surrogate model with regards to the chosen search space component. At iteration $i$, the marginal effect of the chosen search space component on the predicted outcome $\hat y^{(i)}$ of the surrogate model at iteration $i$ is computed and displayed.

**How to use**  
  * Identify the influence of a search space component on the prediction of $\hat y^{(i)}$: A partial dependence plot can show whether the relationship between the target and a search space component is linear, monotonic or more complex.  
  
**Modifications**
  * $highlight\_iter$: We can set $highlight\_iter$ in the sidebar panel if we are interested in a specific iteration to be highlighted.  
  * $feature$: We can select a search space component ($feature$) to inspect the surrogate (or PDP for higher dimensions) for that component.


  

