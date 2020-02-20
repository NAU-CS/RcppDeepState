#include "deepstate/DeepState.h"
#include <Rcpp.h>
#include <assert.h>
#include <limits.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>

TEST(Random_Set, Ranges) {
    Rcpp::RObject rcpp_result_gen;
  int min_val = DeepState_MinInt();
   int max_val = DeepState_MaxInt();
   size_t size_limit ={max_val,DeepState_RandInt()};
   int rand_val = OneOf(size_limit);
   int num_vector_size = DeepState_IntInRange(0,rand_val);
   int low_val = DeepState_Double();
   int high_val = DeepState_Double();
   int int_vector_size = DeepState_IntInRange(0,max_val);
   NumericVector NumericRand_vec(num_vector_size);
   IntegerVector IntegerRand_vec(int_vector_size);
   for(int i=0; i< num_vector_size; i++){
     OneOf(
	   [&] {
                 NumericRand_vec[i] = DeepState_Float();
           },
	   [&] {
                 if(low_val > high_val)  
                 NumericRand_vec[i] = DeepState_DoubleInRange(high_val,low_val);
                 else
                 NumericRand_vec[i] = DeepState_DoubleInRange(low_val,high_val);
               }
           [&] {
                   // need to check on this for NA,Nan,Inf,-Inf
                 NumericRand_vec[i] = Missing(); 
               },
      );
          }
   for(int i=0; i< int_vector_size; i++){
  
 OneOf(
	   [&] {
                 IntegerRand_vec[i] = DeepState_Int();
           },
	   [&] {
                 IntegerRand_vec[i] = DeepState_IntInRange(min_val,max_val);
           },
	   [&] {
                    // 2^0 - 2^256
                 IntegerRand_vec[i] = DeepState_UIntInRange(0, 1);
           },
           [&] {
                 
           });


  char* str_randval ;
 OneOf(
	    [&] {
                 str_randval = malloc(MAX_STR_LEN);
	         str_randval = DeepState_CStrUpToLen(MAX_STR_LEN,"abcdef0123456789");
	      },
	    [&] 
            {
               str_randval = malloc(MAX_STR_LEN);
               str_randval = DeepState_CStrUpToLen(MAX_STR_LEN,"abcdefghijklmnopqrstuvwxyz");
            },
	    [&] 
            {  int size_val = OneOf(DeepState_Int);
               str_randval = malloc(size_val);
               str_randval = DeepState_CStrUpToLen(size_val,"abcdefghijklmnopqrstuvwxyz");
            }
     );
  char** str_vector_randval ;
 int size_val = OneOf(DeepState_Int);
  str_vector_randval = new str_vector_randval*[max_val];
  for(int i = 0; i < max_val ; i++ ){
     if(size_val > max_val)
     str_vector_randval[i] = new str_vector_randval[size_val];
     else
     str_vector_randval[i] = new str_vector_randval[max_val];
  }
for(int j =0 ; j < max_val; j++){
 OneOf(     
	    [&] {
                 str_randval[i] = DeepState_CStrUpToLen(MAX_STR_LEN,"abcdef0123456789");
	      },
	    [&] 
            {
                   str_randval[i] = DeepState_CStrUpToLen(MAX_STR_LEN,"abcdefghijklmnopqrstuvwxyz");
            },
	    [&] 
            {         
               str_randval = DeepState_CStrUpToLen(size_val,"abcdefghijklmnopqrstuvwxyz");
            }
     );
}
  rcpp_result_gen = Rcpp::wrap(rcpp_binseg_normal(NumericRand_vec, IntegerRand_vec));
}


 
