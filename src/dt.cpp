#include <multinode.h>
#include <mlnode.h>
#include <dtnode.h>
#include <node.h>

#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

RCPP_MODULE(dt){
    
    class_<node>("Node")
        //.constructor<double,double>()
        .field( "edges", &node::edges)
    		.field( "ichildren", &node::ichildren)
    		.field( "maxind", &node::maxind)
    		.field( "leafstart", &node::leafstart)
        .method( "numLeaves", &node::get_num_leaves )
        ;
    class_<dtnode>( "DTNode" )
		.derives<node>("Node")
        .constructor<NumericVector, List, IntegerVector, int>()
        
        ;
    class_<mlnode>( "MLNode" )
        .derives<node>("Node")
        .constructor<NumericVector, List, IntegerVector, int, IntegerVector>()
        .field( "words", &mlnode::words)
    ; 
    class_<multinode>( "MultiNode" )
        .derives<node>("Node")
        .constructor<NumericVector, List, IntegerVector, int, IntegerVector, List>()
        .field( "words", &multinode::words )
        //.field( "variants", &multinode::variants )
        //.method( "numLeaves", &multinode::get_num_leaves )
    ; 
        
};
