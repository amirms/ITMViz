#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <vector>
#include <math.h>
#include <rmaths.h>

#include <dtnode.h>

class mlnode: public dtnode
{
  
   public:
    IntegerVector words;  
   
    mlnode(NumericVector edges, List ichildren, IntegerVector maxind,
  		  int leafstart, IntegerVector words);
    virtual mlnode* clone();
    virtual ~mlnode();


};