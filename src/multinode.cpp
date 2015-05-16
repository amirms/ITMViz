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
#include <multinode.h>
#include <dtnode.h>

using namespace std;
using namespace Rcpp;

/**
 * Return the log of the weight for a given variant
 * (the weight is equal to the sum of all leaf weights
 * under the likely internal node for this variant)
 */
double multinode :: var_logweight(int given_y) 
{
//  Rcout << "Calculating logweights for given_y: "<< given_y <<"\n"; 
//   Rcout << "The size of variant_logweights: "<< variant_logweights.size() <<"\n"; 

  return variant_logweights[given_y];
}

/**
 * Calculate logpwz term for a given y value
 */
double multinode :: calc_logpwz(int given_y)
{
//  Rcout << "Calculating logpwz for given_y: "<< given_y <<"\n";  
  dtnode* dtv = dynamic_cast<dtnode*>(variants[given_y]);
//  Rcout << "Dynamically casts variants[given_y]\n"; 
  //XPtr<dtnode> dtv = unwrap_robject<dtnode>(variants[given_y]);

  return (*dtv).calc_logpwz();
}

/**
 * Set a new y value
 */
void multinode :: set_y(int given_y)
{
  y = given_y;
}

/**
 * Return y value (which variant is 'active') for this multinode
 */
int multinode :: get_y()
{
  return y;
}

/**
 * Return number of variants represented by this multinode
 */
int multinode :: num_variants()
{
  return variants.size();
}

/**
 * Calculate topic-word term of Gibbs sampling eqn
 */
double multinode :: calc_wordterm(double val, int leaf)
{ 
  // Is this leaf under one of our interior children?
  double newval;
//  Rcout << "multinode calc_wordterm val: " << val << "\n";
//  Rcout << "multinode calc_wordterm leaf: " << leaf << "\n";

  for(unsigned int i = 0; i < ichildren.size(); i++)
    {
      //FIXED maxind[i]- 1
      if(leaf <= (maxind[i] - 1))
        {
          // Update value with call to appropriate variant
          //FIXED fake_leafmaps[v][ei] - 1
          newval = (*variants[y]).calc_wordterm(val,fake_leafmaps[y][i] - 1);

          // Pass value down to actual child
          XPtr<node> ichild = unwrap_robject<node>(this->ichildren[i]);
          return (*ichild).calc_wordterm(newval,leaf);
        }
    }

  // No, this leaf must be one of our leaves
  
  // Get leaf edge index
  int ei = ichildren.size() + (leaf - (leafstart - 1));
  
//  Rcout << "The value of y is: " << y << "\n";
//  Rcout << "calling variants[y]\n";
//  Rcout <<"edge index ei: " << ei << "\n";
//  Rcout <<"(*variants[y])" << variants[y] << "\n";
    
  // Update value with call to appropriate variant
  //FIXED fake_leafmaps - 1
  return (*variants[y]).calc_wordterm(val,fake_leafmaps[y][ei] - 1);
}

/**
 * Update all branch counts associated with a given leaf
 */
void multinode :: modify_count(double val, int leaf)
{
 
  // Is this leaf under one of our interior children?
  for(unsigned int i = 0; i < ichildren.size(); i++)
    {
      if(leaf <= (maxind[i] - 1))
        {
          // Update all variants
          for(unsigned int v = 0; v < variants.size(); v++)
            {
              //FIXED fake_leafmaps[v][ei] -1
              (*variants[v]).modify_count(val,fake_leafmaps[v][i] - 1);
            }

          // Recursively update child cts
          XPtr<node> ichild = unwrap_robject<node>(ichildren[i]);

          (*ichild).modify_count(val,leaf);
          return;
        }
    }

  // No, this leaf must be one of our leaves
  
  // Get leaf edge index
  int ei = ichildren.size() + (leaf - (leafstart - 1));
  // Update all variants
  for(unsigned int v = 0; v < variants.size(); v++)
    {
      //FIXED fake_leafmaps[v][ei] - 1
      (*variants[v]).modify_count(val,fake_leafmaps[v][ei]- 1);
    }

  return;  
}

/*
* get the number of leaves: number of leaves of children + current number of edges - number of children
*/
int multinode :: get_num_leaves()
{
	//Can't take len(edges) b/c edges==None by convention
	int n = 0;
	
  for(unsigned int i = 0; i < ichildren.size(); i++)
  {
    XPtr<node> ichild = unwrap_robject<node>(this->ichildren[i]);
	//	if ((*ichild) != NULL)
			n += (*ichild).get_num_leaves();
		
  }
    return (n + words.size());
}


/**
 * Constructor
 */
multinode :: multinode(NumericVector edges, List ichildren, IntegerVector maxind,
			int leafstart, IntegerVector words, List arg_variants)
{       
  // Do some argument checking
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

	
  // Populate data members
  this->edges = edges;
  this->edgesum = sum(this->edges);
  
  //FIXME original edges
 // this->orig_edges = NumericVector(Rcpp::clone(edges));
//  this->orig_edgesum = sum(this->orig_edges);
//  Rcout << "edgesum is: " << edgesum << "\n";
  this->words = words;
  this->leafstart = leafstart;
  
  Rcout << "The number of variants: " << arg_variants.size()  << "\n";
  
//  stop("asas");

  // Build variants
  for(unsigned int v = 0; v < arg_variants.size(); v++)
    {
      // Unpack variant dtnode and fake leafmap
      List newvar = arg_variants(v);
      //dtnode* vdtn = newvar(0); // alternative/variant dtnodes
      XPtr<dtnode> vdtn = unwrap_robject<dtnode>(newvar(0));

      IntegerVector flm = newvar(1); //fake_leafmap
      
      try {
        
        //        // Unpack tuple contents
//        PyArrayObject* vedges = (PyArrayObject*) PyTuple_GetItem(vdtn,0);
//        PyObject* vchildren = PyTuple_GetItem(vdtn,1);
//        PyObject* vmaxind = PyTuple_GetItem(vdtn,2);
//        int vleafstart = PyInt_AsLong(PyTuple_GetItem(vdtn,3));
//        
//        // Build child dtnode
//        dtnode* newvardt = new dtnode(vedges,vchildren,
//                                      vmaxind,vleafstart); 
//        variants.push_back(newvardt);
//
//        // Calculate log of weighing term (for y-sampling)
//        PyArrayObject* likely_edges = (PyArrayObject*) 
//          PyTuple_GetItem(PyList_GetItem(vchildren,0),0);
//        double le_sum = PyFloat_AsDouble(PyArray_Sum(likely_edges,NPY_MAXDIMS,
//                                                     PyArray_DOUBLE,NULL));
//        variant_logweights.push_back(log(le_sum));
        
//        Rcout << "The size of fake leafmap: " << flm.size()  << "\n";
        
        // Extract fake leaf map for this variant
        vector<int> cur_flm;
        for(unsigned int fli = 0; fli < flm.size(); fli++)
          cur_flm.push_back(flm(fli));
          
        fake_leafmaps.push_back(cur_flm);

        //Add the variant dtnode
        (this -> variants).push_back(vdtn); 
        // Calculate log of weighing term (for y-sampling)        
//        PyArrayObject* likely_edges = (PyArrayObject*) 
//          PyTuple_GetItem(PyList_GetItem(vchildren,0),0);

        XPtr<dtnode> ichild = unwrap_robject<dtnode>((*vdtn).ichildren[0]);
        NumericVector likely_edges = (*ichild).edges;
//          Rcout << "The number of likely edges: " <<likely_edges.size()  << "\n";
//        double le_sum = PyFloat_AsDouble(PyArray_Sum(likely_edges,NPY_MAXDIMS,
//                                                     PyArray_DOUBLE,NULL));
        double le_sum = (*ichild).edgesum;
//          Rcout << "likely edges sum: " << le_sum  << "\n";
        variant_logweights.push_back(log(le_sum));
      }

      catch (int err)
        {
          throw 1;
        }                             
    }

  // Init y to -1 
  // (will raise errors if not properly initialized elsewhere)
  y = -1;

  // Recursively build children
  // 
  this->maxind = maxind;
  this->ichildren = ichildren;
  
  int c;
  int nci = ichildren.size();
  for(c = 0; c < nci; c++)
    {    
//      // Get max leaf index under this child, check it
//      maxind.push_back(arg_maxind(c));
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


multinode* multinode::clone()
{ 
  
  IntegerVector maxind_clone(Rcpp::clone(this-> maxind));
  IntegerVector words_clone(Rcpp::clone(this->words));
  
  Rcout <<"Printing the edges of a multinode to be cloned: " << this->edges.size() << "\n";
  NumericVector edges_clone(Rcpp::clone(this -> edges));
//
  List ichildren_clone;
  
  Rcout <<"Printing the number of ichildren: " << (this->ichildren).size() << "\n";
  
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

      XPtr<dtnode> ichild_clone_wrapped(ichild_clone);  
      
	    ichildren_clone.push_back(ichild_clone_wrapped);
	  
    }
  
    List variants_clone;
//    
//    
    multinode* multinode_clone = new multinode(edges_clone, ichildren_clone,
                 maxind_clone, this->leafstart, words_clone, variants_clone);

  Rcout <<"Printing the size of variants: " << (this->variants).size() << "\n";

/*
    *Re-construct variants
    */
    
    //Make sure the size of variants and fake_leafmap is the same
    // Build variants
  for(unsigned int v = 0; v < (this -> variants).size(); v++)
    {
      try {
          Rcout << "Reconstructing the variant v: " << v << "\n";
          // Extract fake leaf map for this variant
          vector<int> cur_flm(this -> fake_leafmaps[v]);
          Rcout << "Length of cur_flm is: " << cur_flm.size() << "\n";
          
          ((*multinode_clone).fake_leafmaps).push_back(cur_flm);
          
          Rcout << "Length of cur_flm in fake_leafmaps is: " << (multinode_clone->fake_leafmaps)[v].size() << "\n";
          
          //Add the variant dtnode
          dtnode* vdtn = (this -> variants)[v];
          dtnode* vdtn_clone = (*vdtn).clone(); 
          ((*multinode_clone).variants).push_back(vdtn_clone);
          
          // Rcout << "Length of cur_flm in fake_leafmaps is: " << (multinode_clone->fake_leafmaps)[v].size() << "\n";
                   
          ((*multinode_clone).variant_logweights).push_back(this -> variant_logweights[v]);
      }

      catch (int err)
        {
          throw 1;
        }                             
    }


return(multinode_clone);
}



/**
 * Destuctor
 */
multinode :: ~multinode()
{
  //
  // Delete variants, ichildren will be 
  // deleted by base class destructor (node)
  //
  //for(unsigned int vi = 0; vi < variants.size(); vi++)
  //  delete variants[vi];
}
