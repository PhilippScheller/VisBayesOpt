**MboPlotRuntime**

**Description**    
 The plots show the split of the time spent during optimization. The vertical line marks a single iteration.  
 

**Interpretation**   
  * Left plot: Shows the time spent executing the objective function $f(x)$ that has been passed to the optimizer.  
  * Right plot:  
   * Train time: Time to train the surrogate model (which proposes the new points).  
   * Propose time: Time spent by infill optimization to propose a new point, given the trained surrogate model.

**How to use**  
  * Train (Propose) time: The single elapsed times offer an insight how much of the overall time is spent on the different task. If training of the surrogate takes too much time, compared to the execution time, one can consider using another (more lean, better suited) surrogate or another optimization technique.  
  * Train and propose time as portion: Sometimes the surrogate model fitting gets very expensive if the surrogate model does not scale well with the number of data points used for fitting. Combining this information with the ones from other analyses one may cut the number of iterations to save cost or use a different surrogate.
  
**Modifications**
  * We can set $highlight\_iter$ in the sidebar panel if we are interested in a specific iteration to be highlighted.


  

