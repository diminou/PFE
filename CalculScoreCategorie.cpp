
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

// [[Rcpp::export]]
double TFIDF(double tf, double nb, double df){
  double res =tf *log(nb/df);
  return res;
}



std::vector<double> carre(std::vector<double> v){
   std::vector<double> res;
    for(int i=0; i<v.size();i++){
      double tempo = v[i]*v[i];
      res.push_back(tempo);
    }
  return res;
}


// [[Rcpp::export]]
double sim_cos(std::vector<double> v1,  std::vector<double> v2){
  double num =0.0;
  double part1 =0.0;
  double part2 =0.0;
      
  num = std::accumulate(v1.begin(), v1.end(),0) + std::accumulate(v2.begin(), v2.end(),0);
  
  std::vector<double> v1carre = carre(v1);
  part1 = std::accumulate(v1carre.begin(), v1carre.end(),0);
  
  std::vector<double> v2carre = carre(v2);
  part2 = std::accumulate(v2carre.begin(), v2carre.end(),0);
  
  
  double denom = std::sqrt(part1) * std::sqrt(part2);
//  std::cout<< "num" << num << "\n";
//  std::cout<< "denom" << denom << "\n";
  double res =num/denom;
  
  return res;
}




//carre <- function(x){
//  return(x*x)
//}
//
//
//# calcul la similarité coinus enter 2 vecteurs v1 et v2 de même longueur (on prendra le tfidf dans noter cas.)
//sim_cos <- function(v1, v2){
//  num <- 0
//  part1 <-  0
//  part2 <-  0
//
//  num = sum(v1,v2)
//  part1 = sum(carre(v1))
//  part2 = sum(carre(v2))
//  
//  denom = (sqrt(part1)*sqrt(part2))
//  res= (num /denom)
//  return (res)
//}

