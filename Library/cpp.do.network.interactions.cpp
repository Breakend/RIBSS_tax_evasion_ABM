#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector doNetworkInteractions(NumericVector initialPerceptions, IntegerVector taxIds, double beta,
                                    List nnInt, NumericVector propInt, String method = "mean")
{
  int n = taxIds.size();
  NumericVector influence(n);
  
  if(method == "mean")
  {
    
    for(int i=0;i<n;i++)
    {
      int id = taxIds[i]-1;
      IntegerVector nnList(nnInt[id]);
      int m = nnList.size();
      
      double betaInd = beta*propInt[id];
      double personalInfluence = (1.0-betaInd)*initialPerceptions[id];
      double neighboursInfluence = 0.0;
      
      if(propInt[id] > 0.0)
      {
        //Calculate mean of initial perceptions for nnList indices
        double sum = 0.0;
        for(int j=0; j<m; j++)
        {
          int index = nnList[j]-1;
          sum = sum + initialPerceptions[index];
        }
        neighboursInfluence = sum/double(m);
      }
      else
      {
        neighboursInfluence = 0.0;
      }
      influence[i] = personalInfluence + neighboursInfluence;
    }
  }
  else if(method == "max")
  {
    for(int i=0;i<n;i++)
    {
      int id = taxIds[i]-1;
      IntegerVector nnList(nnInt[id]);
      int m = nnList.size();
      
      double personalInfluence = initialPerceptions[id];
      double neighboursInfluence = 0.0;
      
      if(propInt[id] > 0.0)
      {
        //Probability of affecting at least one
        double prob = (1.0 - pow(beta, m));
        
        bool rvLogical = bool(rbinom(1, 1, prob));
        
        if(rvLogical)
        {
          double maxInf = 0.0;
          for(int k = 0; k<nnList.size();k++)
          {
            int index = nnList[k]-1;
            (initialPerceptions[index]>maxInf)? maxInf = initialPerceptions[index]: maxInf = maxInf;
          }
          neighboursInfluence = maxInf;
        }
        else
        {
          neighboursInfluence = 0.0;
        }
        
        (neighboursInfluence > personalInfluence)? influence[i] = (neighboursInfluence) : influence[i] = (personalInfluence);
      }
      
    }
  }
  
  return influence;
}
