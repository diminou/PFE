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
std::string sTos(String line)
{
  std::string str = line;
  return str;
}


//[[Rcpp::export]]
std::vector<std::string> getBracketedExps(std::string& line)
{
  std::vector<std::string> result;
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
      if(value.size() > 0)
      {
        result.push_back(value);
        value = "" ;
      }
    }
    if(flag && c != '<')
    {
      
      value+=c;
    }
  }
   if(flag || result.size()==0)
   {
     throw Rcpp::exception("invalid string");
   }
   return result;
}

//[[Rcpp::export]]
std::string getResource(std::string str)
{
  std::reverse(str.begin(), str.end());
  std::string result;
  for(char & c : str)
  {
    if(c == '/' || c == ':')
    {
      break;
    }
    else 
    {
      result += c;
    }
  }
  reverse(result.begin(), result.end());
  return result;
}

//[[Rcpp::export]]
std::string extractArticle(std::string line)
{
  return getResource(getBracketedExps(line).front());
}

//[[Rcpp::export]]
std::string extractCategory(std::string line)
{
  return getResource(getBracketedExps(line).back());
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
