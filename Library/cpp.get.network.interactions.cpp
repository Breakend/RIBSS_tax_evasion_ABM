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
DataFrame getNetworkInteractions(NumericVector initialPerception, IntegerVector taxIds, double beta, List nnInt, 
                                 NumericVector propInt, String method="mean")
{
  int n = taxIds.size();
  NumericVector betaNetwork(n);
  NumericVector neighboursInfluence(n);
  DataFrame results;
    
  if(method == "mean")
  {
    for(int i=0;i<n;i++)
    {
      int id = taxIds[i]-1;
      IntegerVector nnList(nnInt[id]);
      int m = nnList.size();
      double sum = 0.0;
      double betaNet = beta*propInt[id];
      betaNetwork[i] = (betaNet);
      
      if(propInt[id] > 0.0)
      {
        for(int j=0; j<m; j++ )
        {
          int index = nnList[j]-1;
          sum = sum + initialPerception[index];
        }
        neighboursInfluence[i] = (sum/double(m));
      }
      else
      {
        neighboursInfluence[i] = (0.0);
      }
   }
    results = DataFrame::create(Named("beta.network")=betaNetwork, Named("unweighted.neighbours.influence") = neighboursInfluence);
  }
  else if(method == "max")
  {
    double neigboursInf = 0.0;
    double personalInfluence = 0.0;
    NumericVector influence(n);
    
    for(int i=0; i<taxIds.size(); i++)
    {
      int id = taxIds[i]-1;
      IntegerVector nnList(nnInt[id]);
      int m = nnList.size();
      personalInfluence = initialPerception[id];
      neigboursInf = 0.0;
      
      if(propInt[id] > 0.0)
      {
        //Probablity of being affected by atleast one
        double prob = 1.0 - double(pow(beta, m));
        bool rvLogical = bool(rbinom(1, 1, prob));
        
        if(rvLogical)
        {
          double maxInf = 0.0;
          for(int k = 0; k<nnList.size();k++)
          {
            int index = nnList[k]-1;
            (initialPerception[index]>maxInf)? maxInf = initialPerception[index]: maxInf = maxInf;
          }
          neigboursInf = maxInf;
        }
        else
        {
          neigboursInf = 0.0;
        }
      }
      else
      {
        neigboursInf = 0.0;
      }
      
      (neigboursInf > personalInfluence)? influence[i] = (neigboursInf) : influence[i] = (personalInfluence);
    }
    
    results = DataFrame::create(Named("ego.alters.influence")=influence);
  }
  
  return results;
}




