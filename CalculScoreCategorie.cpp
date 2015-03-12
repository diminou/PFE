
#include <Rcpp.h>
#include <string>
#include <fstream>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<double> testonsRcpp(std::vector<std::string> setCat, std::vector<std::string> vectNom, std::vector<double> vectScore)
{
  std::vector<double> vecteur ;
  for(int i=0; i<setCat.size();i++){
    double score = 0;
    for(int j=0; j<vectNom.size(); j++){
      if(setCat[i]==vectNom[j]){
        score = score + vectScore[j];
      }
    }
    vecteur.push_back(score);
  }  
  return vecteur;
}
