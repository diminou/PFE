#include <Rcpp.h>
#include <string>
#include <fstream>
using namespace Rcpp;


// [[Rcpp::export]]
std::string outputString()
{
	std::string str;
	str="test2";
	return str;
}

//[[Rcpp::export]]
void printChars(const std::string& infix)
{
  const char *exp = infix.c_str();
  while(*exp!='\0')
  {
    std::cout << *exp << std::endl;
    exp++;
   }
}

//[[Rcpp::export]]
std::string getFirstBracketedExp(std::string& line)
{
  const char *exp = line.c_str();
  std::string value = "";
  bool flag = false;
  for(char & c : line)
  {
    if(c == '<')
    {
      flag = true;
    }
    if(c == '>')
    {
      flag = false;
      break;
    }
    if(flag)
    {
      value.append(&c);
    }
  }
  /*while(*exp!='\0')
  {
    if(*exp == '<')
    {
      flag = true;
    }
    if(*exp == '>')
    {
      flag = false;
      break;
    }
    if(flag)
    {
      value.append(exp);
    }
    *exp++;
   }*/
   if(flag || value.size()==0)
   {
     throw Rcpp::exception("invalid string");
   }
   return value;
}

//[[Rcpp::export]]
std::string readFirstLine(const char* filepath){
  std::ifstream instream(filepath);
  std::string str;
  std::getline(instream, str);
  instream.close();
  return str;
}


/*
std::istream & openFile(std::string filepath)
{
  const char * c = filepath.c_str();
  std::ifstream instream(c);
  return instream;
}
*/

void closeIfstream(std::ifstream *str)
{
  str->close();
}






/*** R
# This is R code
*/
