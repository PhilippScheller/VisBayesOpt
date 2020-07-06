**MboPlotEstimationUncertainty**

**Description**    
 The plots show how the degree of uncertainty in the estimation of new points. 


**Interpretation**   
  * Left plot: Absolute difference between estimated $\hat y$ and evaluated (true) point $y$ over the Iterations  
  * Center plot: Frequency of differences between $\hat y$ and $y$  
  * Right plot: $\hat y$ vs. $y$. The length of the range corresponds to the estimated standard error (uncertainty) of the prediction $\hat y$. The color corresponds to the iteration. The angle bisector shows the perfect fit ($\hat y_i - y = 0, i=1,\ldots, n$)  
  
**How to use**  
  * Quantify estimation uncertainty: if the overall frequency of deviations is high, the estimation error large for higher itertions, and the points for higher iterations frequently (i.e. not only once which could be exploration) away from angle bisector then the surrogate does not well and should be changed  
  
  
**Modifications**
  * We can select the $highlight\_iter$ to calculate the plots for exactly this iteration


  

