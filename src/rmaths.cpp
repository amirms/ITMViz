#include <rmaths.h>

/*
* sumR calculates the sum of each row in the matrix
* returns a vector of doubles
*/
vector<double> sumR(NumericMatrix xx) {
  int n = xx.nrow();
  vector<double> sr(n);

  for(int i = 0; i < n; ++i) {
    sr[i] = sum(xx(i, _));
  }
  return sr;
}

double sum(NumericVector x){
  Rcout << "size of x: " << x.size() << "\n";  

  
  int n = x.size();
  double r= 0;
  
  for (int i = 0; i < n; ++i)
    r += x(i);
  
  return r;
  
}

vector<double> minR(NumericMatrix xx) {
  int n = xx.nrow();
	vector<double> mr(n);

	for(int i = 0; i < n; ++i) {
		mr[i] = min(xx(i, _));
	}
	return mr;
}


double min(NumericVector x){
  int n = x.size();
  
  if (n <= 0)
    return NULL;
  
  double r= x(0);
  
  for (int i = 1; i < n; ++i)
    if (x(i) < r)
      r = x(i);
  
  return r;
  
}