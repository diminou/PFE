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
     return {};
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

std::string getLinkTypeCat(std::string str)
{
  std::reverse(str.begin(), str.end());
  std::string result;
  for(char & c : str)
  {
    if(c == '#')
    {
      break;
    }
    else 
    {
      result += c;
    }
  }
  reverse(result.begin(), result.end());
  if(result != "broader" && result != "related")
  {
    return "";
  }
  return result;
}

//[[Rcpp::export]]
std::string catLinkLine(std::string str)
{
  std::vector<std::string> bracketedExps = getBracketedExps(str);
  if(bracketedExps.size()!=3)
  {
    return "";
  } else {
    std::string cat1 = getResource(bracketedExps[0]);
    std::string cat2 = getResource(bracketedExps[2]);
    std::string link = getLinkTypeCat(bracketedExps[1]);
    
    if(cat1.size()>0 && cat2.size()>0 && link.size()>0)
    {
      return cat1+","+link+","+cat2;
    } else {
      return "";
    }
  }
  
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

std::string produceArticleMQ(std::string title)
{
  std::string result = "merge (node:article {title:'";
  result+=title;
  result+="'})";
  return result;
}

std::string produceCategoryMQ(std::string label)
{
  std::string result = "merge (node:category {label:'";
  result+=label;
  result+="'})";
  return result;
}

std::string produceIs_underMQ(std::string aTitle, std::string cLabel)
{
  std::string result = "match(article:article {title:'";
  result += aTitle;
  result += "'}), (cat:category {label:'";
  result += cLabel;
  result += "'}) merge (article)-[:is_under]->(cat)";
  return result;
}

//[[Rcpp::export]]
std::string extractArticleMQ(std::string line)
{
  std::string title = extractArticle(line);
  return produceArticleMQ(title);
}

//[[Rcpp::export]]
std::string extractCategoryMQ(std::string line)
{
  std::string label = extractCategory(line);
  return produceCategoryMQ(label);
}

//[[Rcpp::export]]
std::string extractIs_underMQ(std::string line)
{
  std::string title = extractArticle(line);
  std::string label = extractCategory(line);
  return produceIs_underMQ(title, label);
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

//[[Rcpp::export]]
void categoryLinksToCsv(std::string in_path, std:: string out_path)
{
  std::ifstream instream(in_path);
  std::ofstream ostream(out_path);
  std::string str;
  std::string result;
  ostream << "cat0,link,cat1"<<std::endl;
  while(std::getline(instream, str))
  {
    result = catLinkLine(str);
    if(result.size() > 0)
    {
      ostream << result << std::endl;
    }
  }
  instream.close();
  ostream.close();
}

//[[Rcpp::export]]
void acceptCharVect(std::vector<std::string> arg)
{
  for(int i=0; i < arg.size(); i++){
    Rcout << arg[i] << std::endl;
  }
}


/*** R
# This is R code
*/
