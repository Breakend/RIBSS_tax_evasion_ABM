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
List getNNInt(IntegerVector taxIds, List nn, double yearlyPropInteractions = 0.1)
{
  int n = taxIds.size();
  List nnInt(n);
  
  
  for(int i=0;i<n;i++)
  {
    int id = taxIds[i]-1;
    IntegerVector nnList(nn[id]);
    int m = nnList.size();
    
    if(m > 0)
    {
      LogicalVector bools = as<LogicalVector>(rbinom(m, 1, yearlyPropInteractions));
      IntegerVector interactions;
      
      for(int j=0; j<m; j++)
      {
        if(bools[j])
        {
          interactions.push_back(nnList[j]);
        }
      }
      nnInt[i] = interactions;
    }
    else
    {
      nnInt[i] = nnList;
    }
  }
  
  return nnInt;
}