**MboPlotRuntime**

**Description**    
 The plots show the time spent during optimization on the different tasks. The vertical line highlights a single iteration exactly.
 

**Interpretation**   
  * Left plot: Shows the time spent executing the function passed to the optimizer  
  * Right plot:  
   * Train time: Time to train the surrogate model (which prooses the new points)  
   * Propose time: Time needed to propose a new point (with the trained surrogate model)

**How to use**  
  * Train (Propose) time: The single elapsed times offer an insight how much of the overall time is spent on the differnt task. It training of the surrogate takes too much time one can consider using another (more lean, better suited) surrogate.  
  * Train and propose time as portion: Sometimes the surrogate model fitting gets very expensive during the search, i.e. the train time is way higher relative to the propose time. Combining this information with the ones from other analyses one may cut the number of iterations to save cost or use a differnt surrogate.
  
**Modifications**
  * We can set $highlight\_iter$ in the sidebar panel if we are interested in a specific iteration to be highlighted.


  

