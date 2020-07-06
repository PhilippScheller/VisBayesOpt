**MboPlotDistToNeighbor**

**Description**    
 The plot shows the distance between the features of the search space. We use gower distance to be able to include discrete features. 
 $$
 d_{ij}^{Gower}=\frac{\sum_{k=1}^pw_k\delta_{ij}^{(k)} d_{ij}^{(k)}}{\sum_{k=1}^pw_k \delta_{ij}^{(k)}}
 $$
where:  
$w_k$   weight of the $k$-th feature  
$\delta_{ij}^{(k)}$  0 when variable $k$ is missing or NA, else 1  
$d_{ij}^{(k)}$  is the contribution of the $k$-th variable to the total distance  
 
reference: taken from documentation of function [*daisy()*]  (https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/daisy)  (pkg. *cluster*)


**Interpretation**   
  * In each iteration the distance between the feature-values sampled (up to the current iteration) is calculated  
  * Comparing 2 adjacent points: How far are the proposed features (sample points) away from the sampled features in the previous iteration

**How to use**  
  * Trade-off exploration/exploitation: if distance shows 'peak' this might indicate that the optimizer explores leading to a combination of feature-values which is far from the previous values  
  * Adjustment of infill criterion (if infill criterion bears parameter measuring the degree of exploration): if heavy peaks also at later iterations one may consider to adjust the infill criterion towards a lower degree of exploration to yield better results (as the optimizer then exploits more often)
  
**Modifications**
  * We can use different $dist\_measures$ from the dropdown in the sidebar panel


  

