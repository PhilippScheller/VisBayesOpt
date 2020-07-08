**MboPlotFit**

**Description**    
 The plots show the fit of the model. shows the in sample R-squared at a certain iteration.  


The right plot shows the predicted target $\hat y$ against the true target $y$. The angle bisector shows a 'perfect' 

**Interpretation**   
  * Left plot: Shows the in-sample R-squared at a certain iteration
  $$
    R^2=1-\frac{SSR}{SST}=1-\frac{\sum_{i=1}^n (y_i-\hat y_i)^2}{\sum_{i=1}^n(y_i-\bar y)^2}
  $$
    where:  
    $y_i$  evaluated (true) target  
    $\hat y_i$  predicted target based on surrogate
    $n$  number of iterations   
    
  * Right plot: Shows the predicted target $\hat y$ against the real target $y$. The length of the range corresponds to the estimated standard error (uncertainty) of the prediction of $\hat y$. The color corresponds to the iteration. The angle bisector shows the perfect fit ($\hat y_i - y = 0, \forall i=1,\ldots, n$).  

**How to use**  
  * Shows how well the surrogate model fits the real (unknown) objective function.
  * Bad model fit is identified by the following characteristics:  
   * Left plot: If $R^2$ is low.  
   * Right plot: If the points for higher iterations frequently (i.e. not only once which could be exploration) fall away from angle bisector.
  * Actions if bad fit identified: choose another surrogate model class to improve the quality of the surrogate fit and thus the overall optimization result.  
  
**Modifications**
  * We can set $highlight\_iter$ in the sidebar panel if we are interested in a specific iteration to be highlighted. The right plot only shows the target ($(y, \hat y)$) up to the $highlight\_iter$.  
  * We can set $predict\_y\_iter\_surrogate = TRUE$ to predict all estimated target values $\hat y$ by using the surrogate model of the iteration $highlight\_iter$. If $predict\_y\_iter\_surrogate = FALSE$ the predicted target values $\hat y$ are taken from the design matrix, i.e. $\hat y^{(i)}$ is predicted by the surrogate model of iteration $i$ $\forall i=1,\ldots,highlight\_iter$.  
  * Choosing $predict\_y\_iter\_surrogate = TRUE$ may result in a bad prediction of $\hat y$ if the surrogate of $highlight\_iter$ is bad (e.g. constant).


  

