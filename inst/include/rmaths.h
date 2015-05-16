#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vector>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;


vector<double> sumR(NumericMatrix xx);
vector<double> minR(NumericMatrix xx);
double sum(NumericVector x);
double min(NumericVector x);