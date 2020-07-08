**MboPlotDistToNeighbor**

**Description**    
 The plot shows the distance between the search space components We use Gower distance to handle discrete search space components as well. 
 $$
 d_{ij}^{Gower}=\frac{\sum_{k=1}^pw_k\delta_{ij}^{(k)} d_{ij}^{(k)}}{\sum_{k=1}^pw_k \delta_{ij}^{(k)}}
 $$
where:  
$w_k$   weight of the $k$-th search space components  
$\delta_{ij}^{(k)}$  0 when variable $k$ is missing or NA, else 1  
$d_{ij}^{(k)}$  is the distance contribution of the $k$-th variable to the total distance, i.e. x[i,k] to x[j, k]  
$i,j = 1, 2\ldots, n$  observations for each search space component, i.e. rows of design matrix  
$p$       number of search space components x
 
reference: taken from documentation of function [*daisy()*]  (https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/daisy)  (pkg. *cluster*)  

* $dist\_measure \in \{min, max, mean\}$: Once the distances are calculated we get a lower triangular matrix $M$ with 
$n$ (number of iterations) rows and $p$ (number of search space components) columns. The $dist\_measure$ is applied to each
row of $M$, i.e. giving the $min,max, mean$ of each row which is then plotted.


**Interpretation**   
  * In each iteration the distance between the values of search space components is calculated (i.e. the plot shows at iteration i how close the search space components $\mathbf{x}^{(i)}$are to each other).
  * Comparing 2 adjacent points: How far are the proposed search space components (sample points) away from the sampled search space components in the previous iteration.  
  * Choosing the initial design: If there is no significant drop after the initial design, one may choose a bigger initial design.  

**How to use**  
  * Trade-off exploration/exploitation: If distance shows 'peak' this might indicate that the optimizer explores leading to a combination of values of the search space components which is far from the previous values.   
  * Adjustment of infill criterion (if infill criterion bears parameter measuring the degree of exploration): If heavy peaks also at later iterations one may consider to adjust the infill criterion towards a lower degree of exploration to yield better results (as the optimizer then exploits more often).  
  
**Modifications**
  * We can use different $dist\_measure$s from the dropdown in the sidebar panel.


  

