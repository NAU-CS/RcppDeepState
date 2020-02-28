#include <deepstate/DeepState.hpp>
#include "TestRcppharness.h"
using namespace deepstate;

class DeepState_Test{
public:
    Rcpp::List Missing_values(){
        
        Rcpp::NumericVector values = Rcpp::NumericVector::create(NA_REAL,R_NaN,R_PosInf,R_NegInf);
        return Rcpp::List::create(values);
    }
    Rcpp::List RcppDeepState_NumericVector()
    {
        int max_val = DeepState_MaxInt();
        size_t size_limit ={max_val,DeepState_RandInt()};
        int rand_val = OneOf(size_limit);
        int num_vector_size = DeepState_IntInRange(0,rand_val);
        int low_val = DeepState_Double();
        int high_val = DeepState_Double();
        Rcpp::NumericVector NumericRand_vec[num_vector_size];
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
                },
                [&] {
                    // need to check on this for NA,Nan,Inf,-Inf
                    NumericRand_vec[i] = OneOf(Missing_values());
                },
            );
            return Rcpp::List::create(NumericRand_vec);
        }
    }
    Rcpp::List RcppDeepState_NumericVector(int size_of_vector)
    {
        int low_val = DeepState_Double();
        int high_val = DeepState_Double();
        Rcpp::NumericVector NumericRand_vec[size_of_vector];
        for(int i=0; i< size_of_vector; i++){
            OneOf(
                [&] {
                    NumericRand_vec[i] = DeepState_Float();
                },
                [&] {
                    if(low_val > high_val)  
                        NumericRand_vec[i] = DeepState_DoubleInRange(high_val,low_val);
                    else
                        NumericRand_vec[i] = DeepState_DoubleInRange(low_val,high_val);
                },
                [&] {
                    // need to check on this for NA,Nan,Inf,-Inf
                    NumericRand_vec[i] = OneOf(Missing_values());
                },
            );
            return Rcpp::List::create(NumericRand_vec);
        }
    }
Rcpp::List RcppDeepState_IntegerVector(){
        int min_val = DeepState_MinInt();
        int max_val = DeepState_MaxInt();
        int int_vector_size = DeepState_IntInRange(0,max_val);
        Rcpp::IntegerVector IntegerRand_vec[int_vector_size];
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
            );
            return Rcpp::List::create(IntegerRand_vec);
        }
        
}
Rcpp::List RcppDeepState_IntegerVector(int size_of_vector){
        Rcpp::IntegerVector IntegerRand_vec[size_of_vector];
        for(int i=0; i< size_of_vector; i++){
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
            );
            return Rcpp::List::create(IntegerRand_vec);
        }
};
    
TEST(Random_Set, Ranges) {
    DeepState_Test deeptest= new deeptest();
    Rcpp::RObject rcpp_result_gen;
    Rcpp::NumericVector data_vec = deeptest.RcppDeepState_NumericVector();
    Rcpp::IntegerVector max_segments = deeptest.RcppDeepState_IntegerVector();
    rcpp_result_gen = Rcpp::wrap(rcpp_binseg_normal(data_vec, max_segments));
}   

