**MboPlotFit**

**Description**    
 The plot shows the in sample R-squared at a certain iteration.  
 $$
 R^2=1-\frac{SSR}{SST}=1-\frac{\sum_{i=1}^n (y_i-\hat y_i)^2}{\sum_{i=1}^n(y_i-\bar y)^2}
 $$
where:  
$y_i$  evaluated (true) target  
$\hat y_i$  propsoed (predicted) target based on surrogate
$n$  number of iterations

**Interpretation**   
  * The calculation considers for each iteration the predicted target $\hat y$

**How to use**  
  * Shows how well the surrogate model fits the real (unknown) objective function  
  * If $R^2$ low: surrogate might be unsuited for the problem thus another model should be considered
  
**Modifications**
  * We can set $highlight\_iter$ in the sidebar panel if we are interested in a specific iteration to be highlighted.


  

