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
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vector>
#include <Rcpp.h>

#include <rmaths.h>
#include <dtnode.h>
#include <multinode.h>

#define OK 0
#define BAD 1

typedef struct {
  vector<dtnode*> topics;
  NumericMatrix nd; //must be integermatrix
} counts;

typedef struct {
  NumericMatrix alpha;
  vector<double> alphasum;
  vector<int> leafmap;
  int T;
} model_params;

typedef struct {
  int D;
  int W;
  vector<int> doclens;
  vector<vector<int> > docs;
  vector<vector<int> > sample;
  vector<int> f;
} dataset;

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

SEXP intLDA(List docs, NumericVector alpha, SEXP root, 
                  IntegerVector leafmap, int numsamp, int randseed,
                  List init, SEXP qinit_, IntegerVector f_arg);

int convert_args(List docs_arg, NumericMatrix alpha,
                        dtnode* dirtree, IntegerVector leafmap,
                        IntegerVector f_arg, 
                        model_params* mp, dataset* ds, counts* c);

int init_model(model_params* mp, dataset* ds,counts* c, 
                      int randseed, List zinit, SEXP qinit_);

void gibbs_chain(model_params* mp, dataset* ds, counts* c);

NumericMatrix est_phi(model_params* mp, dataset* ds, counts* c);
NumericMatrix est_theta(model_params* mp, dataset* ds, counts* c);
