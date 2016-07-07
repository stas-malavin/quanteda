#include <Rcpp.h>
#include <vector>
#include <mutex>

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>
using namespace Rcpp;
using namespace RcppParallel;
std::mutex lock_input;
std::mutex lock_output;


struct select_tokens : public Worker
{
  List input;
  List output;
  const std::unordered_set<String> set_types;
  const bool remove;
  const bool spacer;
  
  // Constructor
  select_tokens(const List input, List output, const std::unordered_set<String> set_types, bool remove, bool spacer): 
                input(input), output(output), set_types(set_types), remove(remove), spacer(spacer){}
  
  // parallelFor calles this function with size_t
  void operator()(std::size_t begin, std::size_t end){
    //Rcout << "Range " << begin << " " << end << "\n";
    for (int h = begin; h < end; h++){
      //Rcout << "H " << h << "\n";
      lock_input.lock();
      CharacterVector text = input[h];
      //CharacterVector text_temp(text.size()); // make vector in the same length as original
      lock_input.unlock();
      
      //std::vector<std::string> text_temp(text.size());
      
      // int j = 0;
      // for (int i = 0; i < text.size(); i++){
      //   String token = text[i];
      //   bool is_in = set_types.find(token) != set_types.end();
      //   if(is_in == remove){
      //     //Rcout << "Match " << i << ' ' << token.get_cstring() << "\n";
      //     if(spacer){
      //       text_temp[j] = "";
      //       j++;
      //     }
      //   }else{
      //     text_temp[j] = token;
      //     j++;
      //   }
      // }
      // 
      // lock_output.lock();
      // if(j == 0){
      //   output[h] = CharacterVector(); // nothing left
      // }else{
      //   output[h] = text_temp[seq(0, j - 1)];
      // }
      // lock_output.unlock();
      lock_output.lock();
      output[h] = text;
      lock_output.unlock();
      
    }
    return;
  }
};

// [[Rcpp::export]]
List select_tokens_cppl_mt(List input, 
                           CharacterVector &types,
                           const bool &remove,
                           const bool &spacer) {
  
  // allocate the output matrix
  List output(input.size());
  
  std::unordered_set<String> set_types;
  for (int g = 0; g < types.size(); g++){
    set_types.insert(types[g]);
  }
  
  // SquareRoot functor (pass input and output matrixes)
  select_tokens select_tokens(input, output, set_types, remove, spacer);
  
  // call parallelFor to do the work
  parallelFor(0, input.size(), select_tokens);
  
  // return the output matrix
  return output;
}


/*** R

toks <- list(letters, LETTERS)
#select_tokens_cppl_mt(toks, c('b', 'O', 'g', 'N'), TRUE, TRUE)
out <- replicate(100,
  select_tokens_cppl_mt(rep(toks, 10000), c('b', 'O', 'g', 'N'), FALSE, TRUE)
)
*/
