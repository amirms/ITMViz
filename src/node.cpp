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
#include <node.h>

using namespace std;
using namespace Rcpp;

/**
 * Uniform rand between [0,1] (inclusive)
 */

double node :: unif01()
{
  return unif();
}

/**
 * Use the randseed to init random number generator for node
 */
void node :: set_randseed(unsigned int randseed)
{
  srand(randseed);
}

/**
 * Draw a multinomial sample propto exp(vals)
 * (i.e., given log-values, sample wrt actual values)
 *
 * Use logsumexp trick variant to avoid numerical problems
 * 
 */
int node :: log_mult_sample(vector<double> vals)
{  
//  Rcout << "log mult sample\n";
  
//  Rcout << "vals size is: " << vals.size()  << "\n";
  // First, get max value
  double maxval = vals[0];
  
//  Rcout << "max val is: " << maxval  << "\n";
  
  for(unsigned int vi = 0; vi < vals.size(); vi++)
    {
      if(vals[vi] > maxval)
        {
          maxval = vals[vi];
        }
    }

  // Now use max val to normalize in log-domain
  vector<double> newvals;
  double normsum = 0;
  for(unsigned int vi = 0; vi < vals.size(); vi++)
    {
      newvals.push_back(exp(vals[vi] - maxval));
      normsum += exp(vals[vi] - maxval);
    }  
  
  // Now just call standard multinomial sampler
  return node::mult_sample(newvals,normsum);
}

/**
 * Draw a multinomial sample propto vals
 * 
 * (!!! we're assuming sum is the correct sum for vals !!!)
 * 
 */
int node :: mult_sample(double* vals, double norm_sum)
{   
  double rand_sample = unif() * norm_sum;
  double tmp_sum = 0;
  int j = 0;
  while(tmp_sum < rand_sample || j == 0) {
    tmp_sum += vals[j];
    j++;
  }
  return j - 1;
}


// Simple overload for vector instead of array...
int node :: mult_sample(vector<double> vals, double norm_sum)
{  
  double rand_sample = unif() * norm_sum;
  double tmp_sum = 0;
  int j = 0;
  while(tmp_sum < rand_sample || j == 0) {
    tmp_sum += vals[j];
    j++;
  }
  return j - 1;
}


vector<node*> node :: get_multinodes()
{
  vector<node*> multinodes;
  for(unsigned int i = 0; i < ichildren.size(); i++) 
    {
      // 1st, is this child a multinode?
      
      XPtr<node> ichild = unwrap_robject<node>(ichildren[i]);

      string childtype(typeid(*ichild).name());
      if(childtype.find("multinode") != string::npos)
        {
          multinodes.push_back(ichild);
        }

      // Next, recursively have children check for multinodes
      vector<node*> childmulti = 
        (*ichild).get_multinodes();
      for(unsigned int ci = 0; ci < childmulti.size(); ci++)
        {
          multinodes.push_back(childmulti[ci]);
        }
    }
  return multinodes;
}

int node :: get_num_leaves()
{
  return 0;
}

void node :: modify_count(double val, int leaf)
{
  return;
}

double node :: calc_wordterm(double val, int leaf)
{
//    Rcout << "node calc_wordterm val: " << val << "\n";
//  Rcout << "node calc_wordterm leaf: " << leaf << "\n";
  
  return 0;
}  

node :: ~node() {  
  // 
  // Since node is base class, this will be called by descendants
  //
  //for(unsigned int ci = 0; ci < ichildren.size(); ci++)    
  //  delete ichildren[ci];
}
