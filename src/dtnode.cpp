/**
   This file is a part of

   DF-LDA - Implementation of the Dirichlet Forest (DF) prior
   for the Latent Dirichlet Allocation (LDA) model

   Copyright (C) 2009 David Andrzejewski (andrzeje@cs.wisc.edu)
 
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <dtnode.h>
#include <multinode.h>

using namespace std;
using namespace Rcpp;

/**
 * Return the vector of y-values for this topic
 */
vector<int> dtnode :: get_yvec()
{
  vector<node*> multi = get_multinodes();
  vector<int> yvals;
  for(unsigned int mi = 0; mi < multi.size(); mi++)
    {
      //check if i need to check the casting
      Rcpp::XPtr<multinode> mu = unwrap_robject<multinode>(ichildren[mi]);
      
//      multinode* mu = dynamic_cast<multinode*>(ichild);
      yvals.push_back((*(mu)).get_y());
    }
  return yvals;
}

/**
 * Init from a given set of y-values 
 *
 */
int dtnode :: y_init(NumericMatrix yinit, int ti)
{
  vector<node*> multi = get_multinodes();
  // Make sure dimensionality is correct 
  if(multi.size() != yinit.ncol())
    {
      // ERROR 
//      PyErr_SetString(PyExc_RuntimeError,
//                      "q-init dimensionality mismatch - wrong # q-values");
      stop("q-init dimensionality mismatch - wrong # q-values");
      return BAD;
    }
  for(unsigned int mi = 0; mi < multi.size(); mi++)
    {
      //multinode* mu = dynamic_cast<multinode*>(ichildren[mi]);
      Rcpp::XPtr<multinode> mu = unwrap_robject<multinode>(ichildren[mi]);

      //int yval = *((int*)PyArray_GETPTR2(yinit,ti,mi));
      //FIXME If yval is passed by ref, it may change, if the alias is changed! Beware.
      int yval = yinit(ti,mi);
//      Rcout << "initing w/ yval: " << yval << "\n";
      // Make sure y-value in valid range [0,Q-1]
      if(yval < 0 || yval >= (*mu).num_variants())
        {
          // ERROR
//          PyErr_SetString(PyExc_RuntimeError,
//                          "Out-of-range value in q-init");
          stop("Out-of-range value in q-init");
          return BAD;
        }
      else
        (*(mu)).set_y(yval);  
    }
  return OK;
}


/**
 * Do y-sampling for this topic
 */
void dtnode :: y_sampling()
{
  // Sample a new y-value for each multinode
  //
//  Rcout << "Y-sampling\n";
  vector<node*> multi = get_multinodes();
//  Rcout << "Size of ichildren: " << ichildren.size() << "\n";
//    Rcout << "Size of multi-nodes: " << multi.size() << "\n";

  for(unsigned int mi = 0; mi < multi.size(); mi++)
    {
//      Rcout << "ith multinode: " << mi << "\n";
      XPtr<multinode> mu = unwrap_robject<multinode>(ichildren[mi]);
//   Rcout << "Casted ichildren to multinode\n";
   
      // Get y-sampling values foreach variant
      //
      vector<double> vals;
      int numvar = (*(mu)).num_variants();
      
//      Rcout << "The number of variants: "<< numvar <<"\n";  
      for(int vi = 0; vi < numvar; vi++)
        {
          // Log-contribution of the agreeable subtree
          // +
          // Log-contribution of likely set weight
          //
          vals.push_back((*(mu)).calc_logpwz(vi) + 
                         (*(mu)).var_logweight(vi));
        }
      
//      Rcout << "Sampling a new multinomial value from these log-likelihoods\n";  
//      Rcout << "vals size is: " << vals.size() << "\n";  
      // Sample a new multinomial value from these log-likelihoods
      int y = node::log_mult_sample(vals);
      
//      Rcout << "The value of y is: " << y << "\n";  
      (*(mu)).set_y(y);
    }
//    Rcout << "Got out\n";
}

/**
 * Do a simple q-report on multinodes
 */
void dtnode :: qreport()
{
  vector<node*> multi = get_multinodes();
  for(unsigned int mi = 0; mi < multi.size(); mi++)
    {
      XPtr<multinode> mu = unwrap_robject<multinode>(ichildren[mi]);

      printf("%d",(*mu).get_y());
    }
}

/**
 * Do a report on multinodes
 */
void dtnode :: report()
{
  vector<node*> multi = get_multinodes();
  for(unsigned int mi = 0; mi < multi.size(); mi++)
    {
      XPtr<multinode> mu = unwrap_robject<multinode>(ichildren[mi]);

      //double e = *((double*)(edges(mi));
      double e = (double)(edges(mi));
      printf("Edge = %f\n",e);
      printf("Multinode %d, y = %d (of %d variants)\n",
             mi,(*mu).get_y(),(*mu).num_variants());
    }
}


/**
 * Calculate the log of the (uncollapsed) P(w|z) expression
 * (used for the y-sampling step)
 */
double dtnode :: calc_logpwz()
{ 
//  Rcout << "calc_logpwz for dtnode\n";
  // Calculate for this node
  double logpwz = lgamma(orig_edgesum) - lgamma(edgesum);
  for(unsigned int ei = 0; ei < edges.size(); ei++)
    {
      logpwz += lgamma((double)(edges(ei))) -
        lgamma((double)(orig_edges(ei)));
    }
  // Calculate for all interior children
  for(unsigned int i = 0; i < ichildren.size(); i++)
    {
      XPtr<dtnode> child = unwrap_robject<dtnode>(this->ichildren[i]);

      logpwz += (*child).calc_logpwz();
    }
  return logpwz;
}


/**
 * Calculate topic-word term of Gibbs sampling eqn
 */
double dtnode :: calc_wordterm(double val, int leaf)
{
  //Rcout << "dtnode calc_wordterm val: " << val << "\n";
  //Rcout << "dtnode calc_wordterm leaf: " << leaf << "\n";
  
  // Is this leaf under one of our interior children?
  double newval;
  for(unsigned int i = 0; i < ichildren.size(); i++)
    {
//      Rcout << "maxind[i] " << maxind[i] << "\n";
      
      //FIXED (maxind[i] -1)
      if(leaf <= (maxind[i] - 1))
        {
//          Rcout << "this->edges(i) " << this->edges(i) << "\n";          
//          Rcout << "this->edgesum " << this->edgesum << "\n";
//          Rcout << "this->edges(i) " << this->edges(i)  << "\n";

          // Update value
          newval = ((double)(this->edges(i)) / this->edgesum);
          
//          Rcout << "newval: " << newval << "\n";
          
          // Recurisively mult value by child
          XPtr<node> ichild = unwrap_robject<node>(this->ichildren[i]);
          return (*ichild).calc_wordterm(newval*val,leaf);
        }
    }

  // No, this leaf must be one of our leaves
  //Rcout << "No, this leaf must be one of our leaves!\n";
  
//  Rcout << "Number of edges: " << this->edges.size() << "\n";
//  Rcout << "ichildren size: " << ichildren.size() << "\n";
//  Rcout << "leaf is: " << leaf << "\n";
//  Rcout << "leafstart is: " << this->leafstart << "\n";
  
  // Get leaf edge index
  //Fixed the leafstart
    int ei = ichildren.size() + (leaf - (leafstart - 1));
  
//  Rcout << "edge index is: " << ei << "\n";
//  Rcout << "edgesum is: " << this->edgesum << "\n";
//  Rcout << "this->edges(ei): " << this->edges(ei) << "\n";
  
  // Update value
  newval = ((double)(this->edges(ei)) / this->edgesum);

  //Rcout << "newval is: " << newval << "\n";
//  
//  Rcout << "val * newval: " << val *  newval << "\n";

  return (val * newval);
}

/**
 * Update all branch counts associated with a given leaf
 */

void dtnode :: modify_count(double val, int leaf)
{
  
//  Rcout << "DTNODE\n";
//
//  Rcout << "val is: " << val << "\n";
//  Rcout << "leaf is: " << leaf << "\n";
  
  // Is this leaf under one of our interior children?
  for(unsigned int i = 0; i < ichildren.size(); i++)
    {
//      Rcout << "maxind[i] is: " << maxind[i] << "\n";
      
      //FIXED the maxind[i] index
      if(leaf <= (maxind[i]-1))
        {
          // Update edge and sum
          //((double)(this->edges(i))) += val;
          (this->edges(i)) += val;
          edgesum += val;
          // Recursively update child cts
          XPtr<node> ichild = unwrap_robject<node>(this->ichildren[i]);
          (*ichild).modify_count(val,leaf);
          return;
        }
    }

  // No, this leaf must be one of our leaves
  
  
  // Get leaf edge index
  //FIXED leafstart <- leafstart - 1
  int ei = ichildren.size() + (leaf - (leafstart - 1));
  // Update edge and sum
  //(*((double*)(edges(ei))) += val;
  edges(ei) += val;
  edgesum += val;
  return;  
}

/*
* get the number of leaves: number of leaves of children + current number of edges - number of children
*/
int dtnode::get_num_leaves()
{
  int n = 0;
	
  for(unsigned int i = 0; i < ichildren.size(); i++)
  {
    XPtr<node> ichild = unwrap_robject<node>(this->ichildren[i]);
	  n += (*ichild).get_num_leaves();
  }
  
  return (n + (this -> edges).size() - (this -> ichildren).size());
}

/**
 * Constructor
 */
dtnode :: dtnode(NumericVector edges, List ichildren,
                 IntegerVector maxind, int leafstart)
{       
  // Do some argument checking
  //if(!PyList_Check(arg_children) || !PyList_Check(arg_maxind))
   // {    
      // ERROR
      //PyErr_SetString(PyExc_RuntimeError,
      //                "Non-List children/maxind element in dirtree node");
      //throw 1;
    //}
  if(leafstart < 0) 
    {
      // ERROR
      //PyErr_SetString(PyExc_RuntimeError,
      //                "Negative leafstart value in dirtree node");
	  stop("Negative leafstart value in dirtree node");
      throw 1;
    }
  if(ichildren.size() != maxind.size())
    {
      // ERROR
      //PyErr_SetString(PyExc_RuntimeError,
      //                "Different sizes for children/maxind in dirtree node");
	  stop("Different sizes for children/maxind in dirtree node");
      throw 1;
    }    
  //double edgemin = PyFloat_AsDouble(PyArray_Min(arg_edges,NPY_MAXDIMS,NULL));    
  if (edges.size() > 0){
    double edgemin = min(edges);
    if(edgemin <= 0) 
      {
        // ERROR
        //PyErr_SetString(PyExc_RuntimeError,
        //                "Negative/zero edge value in dirtree node");
        stop("Negative/zero edge value in dirtree node");
        throw 1;
      }
  } 
  // Populate data members
//  int nci = sizeof(ichildren);

  this->leafstart = leafstart;
  
  // Get edge values and sum
  // (note that this *must* be a copy!)
  //npy_intp* edims = new npy_intp[1];
  //edims[0] = PyArray_DIM(arg_edges,0);
  //this->edges = (PyArrayObject*) PyArray_ZEROS(1,edims,PyArray_DOUBLE,0);
  //PyArray_CopyInto(this->edges,arg_edges);
  this->edges = edges;
  this->edgesum = sum(this->edges);
//  Rcout << "edgesum is: " << edgesum << "\n";

  // Also make a copy of *original* edge values 
  // (not augmented by counts)
  //this->orig_edges = (PyArrayObject*) PyArray_ZEROS(1,edims,PyArray_DOUBLE,0);
  //PyArray_CopyInto(this->orig_edges,arg_edges);
  this->orig_edges = NumericVector(Rcpp::clone(edges));
  //this->orig_edgesum = PyFloat_AsDouble(PyArray_Sum(this->orig_edges,NPY_MAXDIMS,
  //                               PyArray_DOUBLE,NULL));   
  //this->orig_edgesum = sum(this->orig_edges);
  this->orig_edgesum = sum(this->orig_edges);
  //delete[] edims;

  this->maxind = maxind;
  this->ichildren = ichildren;


  // Recursively build children
  //
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
     
      // Add children
      //
	  
//	  ichildren.push_back(arg_children(c));
	  
    }
}

/**
 * Copy constructor
 */

dtnode* dtnode::clone()
{ 
  
  Rcout << "if you see this twice, there is something wrong!\n";

  NumericVector edges_clone(Rcpp::clone(this -> edges));
  IntegerVector maxind_clone(Rcpp::clone(this-> maxind));
  

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
      
      
//      string childtype(typeid(*ichild).name());
//      if(childtype.find("multinode") != string::npos)
//      {
//          Rcout
//      }
      
      dtnode* ichild_clone = (*ichild).clone();
      
//      SEXP ichild_clone_wrapped = 
//        wrap_in_reference_class(ichild_clone);
        
      XPtr<dtnode> ichild_clone_wrapped(ichild_clone);  
      
	    ichildren_clone.push_back(ichild_clone_wrapped);
	  
    }
    
    
    dtnode* dtnode_clone = new dtnode(edges_clone, ichildren_clone,
                 maxind_clone, this->leafstart);
                 
    return(dtnode_clone);
}


/**
 * Destuctor
 */
dtnode :: ~dtnode()
{
//  //
//  // Decrease ref cts on NumPy arrays, ichildren will be 
//  // deleted by base class destructor (node)
//  
//  //FIXME how to delete R objects through dtnode destructor
//  //Py_DECREF(edges);
//  //Py_DECREF(orig_edges);
}
