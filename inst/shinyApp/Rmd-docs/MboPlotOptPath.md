**MboPlotOptPath**

**Description**    
 The plot shows the surrogate model for the respective iteration.
 

**Interpretation**   
  * 1D search space: Shows the surrogate model in the top and the acquisition function in the bottom.  
  * Higher dimensional search space: Shows the Partial Dependence Plot ([PDP])  (https://christophm.github.io/interpretable-ml-book/pdp.html) of the surrogate model. The PDP shows the influence of the chosen search space component on the prediction of $\hat y^{(i)}$ at the current iteration $i$.

**How to use**  
  * Identify the influnce of a feature on the prediction of $\hat y^{(i)}$: If the PDP shows a decreasing function at iteration $i$ we can say that for larger values of the chosen search space component the predicted $y$ value decreases.  
  * Identify quality of surrogate at single iteration: If the PDP shows a constant at iteration $i$ the search space component under review does not influence $\hat y^{(i)}$ thus the surrogate might not be suited for this problem.  
  
**Modifications**
  * $highlight\_iter$: We can set $highlight\_iter$ in the sidebar panel if we are interested in a specific iteration to be highlighted.  
  * $feature$: We can select a search space component ($feature$) to inspect the surrogate (or PDP for higher dimensions) for that component.


  

