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
#include <math.h>
#include <Rcpp.h>
#include <rmaths.h>

#include <node.h>

using namespace std;
using namespace Rcpp;

#ifndef H_DTNODE
#define H_DTNODE

#define OK 0
#define BAD 1

/**
 * Dirichlet Tree node
 */
class dtnode: public node
{
 public:
  dtnode(NumericVector edges, List children,
         IntegerVector maxind, int leafstart);
  dtnode() {;}
  void report();
  void qreport();
  vector<int> get_yvec();
  void y_sampling();
  int y_init(NumericMatrix yinit, int ti);
  virtual int get_num_leaves();
  virtual void modify_count(double val, int leaf);
  virtual double calc_wordterm(double val, int leaf);  
  virtual double calc_logpwz();
  virtual dtnode* clone();
  virtual ~dtnode();
};
#endif
