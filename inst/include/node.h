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
#include <vector>
#include <stdlib.h>
#include <math.h>
#include <string>
#include <typeinfo>
#include <Rcpp.h>
#include <rmaths.h>

using namespace Rcpp;
using namespace std;

#ifndef H_NODE
#define H_NODE

// Uniform rand between [0,1] (inclusive)
#define unif() ((double) rand()) / ((double) RAND_MAX)

class node
{
 public:
  NumericVector edges;
  double edgesum;
  NumericVector orig_edges;
  double orig_edgesum;
 // vector<node*> ichildren;
  //vector<int> maxind;
  List ichildren;
  IntegerVector maxind;
  int leafstart;
  
  template<class T>
  XPtr<T> unwrap_robject(const SEXP& sexp){
    RObject ro(sexp);
    if(ro.isObject()){
    	Language call("as.environment",sexp);
  		SEXP ev = call.eval();
  		Language call1("get",".pointer",-1,ev);
  		SEXP ev1 = call1.eval();
  		XPtr<T > xp(ev1);
  		return xp;
  	}else{
  		XPtr<T > xp(sexp);
  		return xp;
  	}
  };
  
 // template<class T>
 // SEXP wrap_in_reference_class(const T& obj,std::string class_name){
 // 	XPtr< T > xp(new T(obj));
 // 	Language call( "new", Symbol( class_name ),xp);
 // 	return call.eval();
 // };
  
    template<class T>
  SEXP wrap_in_reference_class(const T& obj){
    XPtr< T > xp(obj);
  	return xp;
  };

 
  static double unif01();
  static int mult_sample(double* vals, double sum);
  static int mult_sample(vector<double> vals, double sum);

  static int log_mult_sample(vector<double> vals);
  
  static void set_randseed(unsigned int randseed);
  vector<node*> get_multinodes();
  virtual int get_num_leaves();
  virtual void modify_count(double val, int leaf);
  virtual double calc_wordterm(double val, int leaf);  

  virtual ~node();
};
#endif
