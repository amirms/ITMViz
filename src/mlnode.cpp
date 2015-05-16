#include <dtnode.h>
#include <multinode.h>
#include <mlnode.h>

using namespace std;
using namespace Rcpp;

/**
 * Constructor
 */
mlnode :: mlnode(NumericVector edges, List ichildren, IntegerVector maxind,
  		int leafstart, IntegerVector words)
{       
  // Do some argument checking
  if(ichildren.size() != maxind.size())
    {
      // ERROR
      //PyErr_SetString(PyExc_RuntimeError,
      //                "Different sizes for children/maxind in dirtree node");
	  stop("Different sizes for children/maxind in dirtree node");
      throw 1;
    }    

	
  // Populate data members
  
  this->words = words;
  this->leafstart = leafstart;
  this->edges = edges;
  this->edgesum = sum(this->edges);
//  Rcout << "edgesum is: " << edgesum << "\n";
  this->orig_edges = Rcpp::clone(edges);
  this->orig_edgesum = sum(this->orig_edges);
  this->ichildren = ichildren;
  this->maxind = maxind;
  
  int nci = ichildren.size();
  int c;
  for(c = 0; c < nci; c++)
    {    
      // Get max leaf index under this child, check it
      if(maxind[c] < 0)
        {
          // ERROR
          //PyErr_SetString(PyExc_RuntimeError,
          //                "Negative maxind value in dirtree node");
  	      stop("Negative maxind value in dirtree node");
          throw 1;
        }
      // exploiting boolean short-circuit here...
      if(c > 0 && maxind[c] <= maxind[c-1])
        {
          // ERROR
          //PyErr_SetString(PyExc_RuntimeError,
          //                "Non-increasing maxind value in dirtree node");
		      stop("Non-increasing maxind value in dirtree node");
          throw 1;
        }
    }
  
}

/**
 * Copy constructor
 */

mlnode* mlnode::clone()
{ 

  Rcout << "cloning must-link node \n";

  NumericVector edges_clone(Rcpp::clone(this -> edges));
  IntegerVector maxind_clone(Rcpp::clone(this-> maxind));
  IntegerVector words_clone(Rcpp::clone(this->words));


  List ichildren_clone;
  
  
  int nci = (this->ichildren).size();
  
  // Recursively build children
  // 
  int c;
  for(c = 0; c < nci; c++)
    {    
      // Get max leaf index under this child, check it
      if(maxind[c] < 0)
        {
          // ERROR
          //PyErr_SetString(PyExc_RuntimeError,
          //                "Negative maxind value in dirtree node");
      stop("Negative maxind value in dirtree node");
          throw 1;
        }
      // exploiting boolean short-circuit here...
      if(c > 0 && maxind[c] <= maxind[c-1])
        {
          // ERROR
          //PyErr_SetString(PyExc_RuntimeError,
          //                "Non-increasing maxind value in dirtree node");
		  stop("Non-increasing maxind value in dirtree node");
          throw 1;
        }
     
      // Add children
      //
    
      XPtr<dtnode> ichild = unwrap_robject<dtnode>((this -> ichildren)[c]);
      
      dtnode* ichild_clone = (*ichild).clone();
      
//      SEXP ichild_clone_wrapped = 
//        wrap_in_reference_class(*ichild_clone);
    
      XPtr<dtnode> ichild_clone_wrapped(ichild_clone); 
      
	    ichildren_clone.push_back(ichild_clone_wrapped);
	  
    }
    
    
    mlnode* mlnode_clone = new mlnode(edges_clone, ichildren_clone,
                 maxind_clone, this->leafstart, words_clone);
                 
    return(mlnode_clone);
}

/**
 * Destuctor
 */
mlnode :: ~mlnode()
{
//  //
//  // Decrease ref cts on NumPy arrays, ichildren will be 
//  // deleted by base class destructor (node)
//  
//  //FIXME how to delete R objects through dtnode destructor
//  //Py_DECREF(edges);
//  //Py_DECREF(orig_edges);
}