**MboPlotProgress**

**Description**    
 The plot shows the cumulative minimum value of the objective function evaluations $f(x)$ at design points $x \in D$ after $n$ iterations, leading to a monotone decreasing function.


**Interpretation**   
  * If the graph shows a decrease, the optimization improves.  
  * If the graphs stays (more or less) constant, the optimizer might have converged. 

**How to use**  
  * If the plot still shows major (no) decreases at larger $n$ one may adjust the constraints of the optimization to grant a larger (lower) number of maximum iterations (to save cost).  
  * If the plot still shows a steep decrease for $n \to n_{max}$, increasing the maximum number of iterations could yield further gain in optimization performance.


  

