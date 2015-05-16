#include <intLDA.h>

#include <stdio.h>

using namespace std;
using namespace Rcpp;

/**
 * This is the exposed method which is called from R using Rcpp
 */

//[[Rcpp::export]]
SEXP intLDA(List docs, NumericMatrix alpha, SEXP root,
                IntegerVector leafmap, int numsamp, int randseed,
                List init, SEXP qinit_, IntegerVector f_arg)
{
	
	XPtr<dtnode> dirtree = unwrap_robject<dtnode>(root);
 
	// Optional args
	// 
	//PyObject* f_arg = NULL; // List of f-labels foreach doc
	//PyObject* init = NULL; // List of Lists to initialize Gibbs
	int verbose = 0; // 1 = verbose output (sampler progress)
	int sampout = 0; // 1 = output actual samples
	int burnin = 0; // Number of initial samples to throw away
	int interval = 1; // Number of samples between printed samples
//	PyArrayObject* qinit = NULL; // NumPy Array
  //SEXP init(init_); // List of Lists to initialize Gibbs
	
	//Results
	
	//NumericMatrix phi; //T x W
	//NumericMatrix theta; // D x W
	//NumericMatrix beta; //T x W
	
	// Init random number generator
	//
	node::set_randseed(static_cast<unsigned>(randseed));
	 
	// Use args to populate structs
	// (also check for *validity*)
	//
	model_params* mp = new model_params;
	dataset* ds = new dataset;
	counts* c = new counts;
  
  Rcout << "Converting the arguments.\n";
    if(BAD == convert_args(docs, alpha, dirtree,
                          leafmap, f_arg, mp, ds, c))
                         
    {
      // Args bad! Return to Python...error condition should be set
      delete mp;
      delete ds;
      delete c;
      return R_NilValue;
    }
    
    Rcout << "Conversion Successful.\n";
    
//    stop("conversion finished!");
  // Initialize model, and make sure init went OK
  //
  int initstatus = init_model(mp,ds,c,randseed,init,qinit_);
  
  Rcout << "Initializing Complete.\n";
  if(initstatus == BAD)
    {
      // ERROR - something went wrong with our init            
      for(int ti=0; ti < mp->T; ti++)
        {
          delete c->topics[ti];
        }          
      delete c;

      //Py_DECREF(mp->alphasum);      
      delete mp;

      delete ds;
      return R_NilValue;
    }   	


 Rcout << "Burning-in Started.\n";
	// Do burn-in
  //
  for(int si=0; si < burnin; si++)
    {
      gibbs_chain(mp,ds,c);
    }
  
  Rcout << "Burning-in Complete\n";
  
  // Do the requested number of Gibbs samples 
  //
  for(int si=0; si < numsamp; si++)
    {
      // In order to make it easier to test against deltaLDA
      node::set_randseed(static_cast<unsigned>(randseed+si));      

      if(verbose == 1)
        {
          printf("Gibbs sample %d of %d\n",si,numsamp);
          for(int ti=0; ti < mp->T; ti++)
            {
              printf("Topic %d q=",ti);
              (*(c->topics[ti])).qreport();
              printf("\n");
            }          
        }

      // Sample z values for each word, 
      // then sample y values for each topic
      //
      // (repeat <interval> times for good sampling practice)
      //
      for(int ii=0; ii < interval; ii++) 
        {
          gibbs_chain(mp,ds,c);
          for(int ti=0; ti < mp->T; ti++)
            {
              (*(c->topics[ti])).y_sampling();                       
            }          
        }

      // If sampout flag set, output 
      // -z-sample
      // -q-sample
      // -phi estimate 
      // 
      if(sampout == 1) 
        {
          // z-sample
          printf("z=");
          int d,i;
          for(d = 0; d < ds->D; d++) 
            {
              for(i = 0; i < ds->doclens[d]; i++) 
                {
                  printf("%d",ds->sample[d][i]);
                }
            }
          printf(" ");

          // q-sample
          printf("q=");
          for(int ti=0; ti < mp->T; ti++)
            {
              (*(c->topics[ti])).qreport();
              if(ti < (mp->T-1))
                printf(",");
            }
			printf("\n");
		
		// phi estimate
          NumericMatrix phi = est_phi(mp, ds, c);
          printf("[");
          for(int pt = 0; pt < mp->T; pt++)
            {
              printf("[");
              for(int pw = 0; pw < ds->W; pw++)
                {
                  //double val = *((double*)phi(pt, pw));
                  double val = phi(pt, pw);
                  if(pw < (ds->W - 1))                
                    printf("%f,",val);
                  else
                    printf("%f",val);
                }
              if(pt < (mp->T -1))
                printf("],");
              else
                printf("]");
            }
          printf("]\n");
          //Py_DECREF(phi);     
        }
    }
	
	// Estimate phi and theta
	//
  Rcout << "Estimating phi\n";
  NumericMatrix phi = est_phi(mp, ds, c);
//  Rcout << "Reached here after phi\n";
  Rcout << "Estimating phi completed\n";

Rcout << "Estimating theta\n";
  NumericMatrix theta = est_theta(mp, ds, c);
  Rcout << "Estimating theta completed\n";
//  Rcout << "Reached here after theta\n";
	
	
	//?? Convert final z-sample back to List of Lists for return
  //
  List finalsamp = List(ds->D);
  
//  Rcout << "size of finalsamp" << finalsamp.size() << "\n";
  
  for(int d = 0; d < ds->D; d++)
    {
      NumericVector docz = NumericVector(ds->doclens[d]);
      for(int i = 0; i < ds->doclens[d]; i++)
        docz(i) = ds->sample[d][i];
      finalsamp(d) = docz;
    }
//   Rcout << "Reached here after final z-sample\n";

   //?? Get final q-sample
  //
  int P = ((*(c->topics[0])).get_multinodes()).size();
  NumericMatrix qsamp(mp->T, P);
  for(int ti = 0; ti < mp->T; ti++)
    {
      vector<int> q = (*(c->topics[ti])).get_yvec();
      for(int yi = 0; yi < P; yi++) 
        qsamp(ti,yi) = q[yi];
        
    }
//  Rcout << "Reached here after final q-sample\n";  
	
//	Rcout << "number of rows of qsamp: " << qsamp.nrow() << "\n"; 
//  Rcout << "number of columns of qsamp: " << qsamp.ncol()  << "\n"; 
  
  
	// Package phi, theta, and final sample in tuple for return
  //
  List retval;
  retval["phi"] = phi; 
  retval["theta"] = theta;
  retval["finalsamp"] = finalsamp;
  retval["qsamp"] = qsamp; 
    // Counts
  //Py_DECREF(c->nd);
    
  //delete c->nd;
  //FIXME delete c->nd and c->topics
//  for(int ti=0; ti < mp->T; ti++)
//    {
//      delete c->topics[ti];
//    }

  delete c;
  // Model params
  //Py_DECREF(mp->alphasum);
  // delete mp->alphasum;
  delete mp;
  // Dataset
  delete ds;

  return retval;
	
}

/**
 * Do an "online" init of Gibbs chain, adding one word
 * position at a time and then sampling for each new position
 */
int init_model(model_params* mp, dataset* ds, counts* c,
                      int randseed, List zinit, SEXP qinit_)
{ 
  //zinit is a list of lists
  //qinit is numeric matrix of size T X ?

// Init y-values for each topic
  //
  int status = OK;
  if (!Rf_isNull(qinit_) && Rf_length(qinit_) > 1)
    {
      NumericMatrix qinit(qinit_);
      
      // Initialize from user-provided values
      Rcout << "Initializing Y from user-provided values.\n";
      // (checking these as we go)
      if(mp->T != qinit.nrow()) 
        {
          // ERROR
//          PyErr_SetString(PyExc_RuntimeError,
//                          "q-init dimensionality mismatch - wrong # topics");
          stop("q-init dimensionality mismatch - wrong # topics");
          return BAD;
        }

      for(int ti=0; ti < mp->T; ti++)
        {
          status = (*(c->topics[ti])).y_init(qinit,ti);
          if(status == BAD)
            {
              // ERROR - flag should already be set
              return BAD;
            }
        }
    }
  else
    {
      // Initialize by sampling proportionally
      //
      Rcout << "Initializing Y by sampling proportionally.\n";
//                        stop("noreason");
      for(int ti=0; ti < mp->T; ti++)
        {
          (*(c->topics[ti])).y_sampling();
        }
   
    }

    Rcout << "Y-sampling successful\n";
  //  stop("no reason");

// Init ND count matrix, a D by T matrix of 0 entries
//  NumericMatrix nddims(ds->D, mp->T);
 
  //c->nd =  (PyArrayObject*) PyArray_ZEROS(2,nddims,PyArray_INT,0);
  //c->nd = nddims;
  c->nd = NumericMatrix(ds->D, mp->T);
//  Rcout << "Created c->nd \n";
  // Build init z sample, one word at a time
  //
  ds->sample.resize(ds->D);
//  Rcout << "resized sample";
  // Temporary array used for sampling  
  double* num = new double[mp->T];

  Rcout << "Initializing Sample.\n";


  // For each doc in corpus
  for(int d = 0; d < ds->D; d++) 
    {
//      Rcout << "doing for d" << d << "\n";
      
      // Create this sample
      vector<int> newsample;
      newsample.resize(ds->doclens[d]);
      ds->sample[d] = newsample;
      
      // Do we have a z-init or not?
      if (!Rf_isNull(zinit) && Rf_length(zinit) > 1)
        {

          //
          // INIT FROM Z-INIT
          //

          List docinit = zinit(d);
          // verify that this is a List
//          if(!PyList_Check(docinit))
//            {
//              // ERROR
//              //PyErr_SetString(PyExc_RuntimeError,
//              //                "Non-List element in initial sample");
//              stop("Non-List element in initial sample");
//              Py_DECREF(c->nd);
//              delete[] num;
//              return BAD;
//            }
          // verify that it has the right length
//          else 
          if(ds->doclens[d] != docinit.size())
            {
              // ERROR
              //PyErr_SetString(PyExc_RuntimeError,
               //               "Init sample/doc length mismatch");
			        stop("Init sample/doc length mismatch");				  
              //Py_DECREF(c->nd);
			        delete c->nd;
              delete[] num;
              return BAD;
            }      
          // Copy over values from List to our sample[]
          for(int i = 0; i < ds->doclens[d]; i++) 
            {
              // Get topic from init sample
              //int zi = PyInt_AsLong(PyList_GetItem(docinit,i));
			        int zi = docinit(i);
              if(zi < 0 || zi > (mp->T-1))
                {
                  // ERROR
                  //PyErr_SetString(PyExc_RuntimeError,
                  //                "Bad init sample value");
		          stop("Bad init sample value");

                  //Py_DECREF(c->nd);
				          delete c->nd;
                  delete[] num;
                  return BAD;
                }
 
              // Get the corresponding word
              int wi = ds->docs[d][i];

              // Store topic and increment counts
              ds->sample[d][i] = zi;
              
//              Rcout << "(c->nd)(d,zi): " << (c->nd)(d,zi) << "\n";
              
              (c->nd)(d,zi)++;
              
//              Rcout << "(c->nd)(d,zi)++: " << (c->nd)(d,zi) << "\n";
              //(*((int*)(c->nd)(d,zi)))++;
//              Rcout << "wi is: " << wi << "\n";
//              Rcout << "mp->leafmap[wi] is: " << mp->leafmap[wi] << "\n";
//              Rcout << "zi is: " << zi << "\n";
             
              (*(c->topics[zi])).modify_count(1,mp->leafmap[wi]);
//              Rcout << "Done fo\n";
            }
      }
      else
        {
          
          Rcout << "Online initializing for document "<< d << " .\n";
          
          //
          // INIT "ONLINE"
          //
          for(int i = 0; i < ds->doclens[d]; i++)
            {
              
 //             Rcout << "i index: " << i << "\n";
              
              int w_i = ds->docs[d][i];
	
  //            Rcout << "word index: " << w_i << "\n";
  
              // For each topic, calculate numerators
              double norm_sum = 0;
              for(int j = 0; j < mp->T; j++) 
                {               
                  // Calculate numerator for this topic
                  
 //                  Rcout << "topic numerator j: " << j << "\n";
                  // (NOTE: alpha denom omitted, since same for all topics) 
                  //double alpha_j = 
                  //  *((double*)PyArray_GETPTR2(mp->alpha,ds->f[d],j));
                  

                  
                  double alpha_j = (mp->alpha)(ds->f[d],j);
//                  Rcout << "alpha_j: " << alpha_j << "\n";
                  
//                  Rcout << "mp->leafmap[w_i]: " << mp->leafmap[w_i] << "\n";
                  
//                  Rcout << "size of (*(c->topics[j])).edges: " << (*(c->topics[j])).edges.size() << "\n";
                  

                  
                  double wordterm = 
                    (*(c->topics[j])).calc_wordterm(1,mp->leafmap[w_i]);
                    
                    
//                  Rcout << "wordterm: " << wordterm << "\n";  
                  
                  //TODO check if the it is a * (b+c)
                  num[j] = wordterm * ((c->nd)(d,j)+alpha_j);
                  norm_sum += num[j];
                }
	
  
              
              // Draw a sample
              //   
              int newz = node::mult_sample(num,norm_sum);
	
              // Update counts and initial sample vec
              //
              ds->sample[d][i] = newz;
              ((c->nd)(d,newz))++;          
              (*(c->topics[newz])).modify_count(1,mp->leafmap[w_i]);
              
            }

                        
            
//           Rcout << "DONE WITH INITIALIZATION\n";

        }
    }
    
  // Cleanup and return
  //
  delete[] num;
  return OK;
}

/**
 * Run Gibbs chain to get a new full sample
 */
void gibbs_chain(model_params* mp, dataset* ds, counts* c)
{ 
  // Use Gibbs sampling to get a new z
  // sample, one position at a time
  //

  // Temporary array used for sampling
  double* num = new double[mp->T];

  // For each doc in corpus
  int d,j,i;
  for(d = 0; d < ds->D; d++) 
    {
      // For each word in doc
      for(i = 0; i < ds->doclens[d]; i++)
        {      
          // remove this w/z pair from all count/cache matrices 
          int z_i = ds->sample[d][i];
          int w_i = ds->docs[d][i];
          
          (*(c->topics[z_i])).modify_count(-1,mp->leafmap[w_i]);
          ((c->nd)(d,z_i))--;
      	
          // For each topic, calculate numerators
          double norm_sum = 0;
          for(j = 0; j < mp->T; j++) 
            { 
              // Calculate numerator for each topic
              // (NOTE: alpha denom omitted, since same for all topics)
              //double alpha_j = 
              //  *((double*)PyArray_GETPTR2(mp->alpha,ds->f[d],j));
              //FIXME is this really a numeric matrix
              double alpha_j = (mp->alpha)(ds->f[d],j);
              double wordterm = 
                (*(c->topics[j])).calc_wordterm(1,mp->leafmap[w_i]);
                
              num[j] = wordterm * ((c->nd)(d,j)+alpha_j);
              norm_sum += num[j];
            }
	
          // Draw a sample
          //
          j = node::mult_sample(num,norm_sum);
	
          // update count/cache matrices and sample vec
          //
          ds->sample[d][i] = j;
          ((c->nd)(d,j))++;
          (*(c->topics[j])).modify_count(1,mp->leafmap[w_i]);
        }
    }
  // Just cleanup and return 
  // (new sample will be returned in ds->sample
  //
  delete[] num;
  return;
}

/**
 * Use final sample to estimate theta = P(z|d)
 */
NumericMatrix est_theta(model_params* mp, dataset* ds, counts* c)
{ 
  //npy_intp* tdims = new npy_intp[2];
  //tdims[0] = ds->D;
  //tdims[1] = mp->T;
  NumericMatrix theta(ds->D, mp->T);
  //delete[] tdims;

  vector<double> rowsums = sumR(c->nd);
  int d,t;
  for(d = 0; d < ds->D; d++) 
    {
      double rowsum = rowsums[d];
      int f = ds->f[d];
      double alphasum = (mp->alphasum)[f];
      for(t = 0; t < mp->T; t++)
        {
          double alpha_t = (mp->alpha)(f,t);
          int ndct = (c->nd)(d,t);

          // Calc and assign theta entry
          double newval = (ndct + alpha_t) / (rowsum+alphasum);
          theta(d,t) = newval;
        }
    }
  return theta;
}


/**
 * Use final sample to estimate phi = P(w|z)
 */
NumericMatrix est_phi(model_params* mp, dataset* ds, counts* c)
{   
  //npy_intp* pdims = new npy_intp[2];
  //pdims[0] = mp->T;
  //pdims[1] = ds->W;
  //PyArrayObject* phi = 
  //  (PyArrayObject*) PyArray_ZEROS(2,pdims,PyArray_DOUBLE,0);
  //delete[] pdims;

  NumericMatrix phi(mp->T, ds->W);
  
  Rcout << "mp->T: " << mp->T <<"\n";
  Rcout << "ds->W: " << ds->W <<"\n";

  int t,w;
  for(t = 0; t < mp->T; t++) 
    {
      Rcout << "topic: " << t <<"\n";
      for(w = 0; w < ds->W; w++) 
        {
          //Rcout << "word: " << w <<"\n";
          phi(t,w) = 
            (*(c->topics[t])).calc_wordterm(1,mp->leafmap[w]);
        }
    }
  return phi;
}

/**
 * Simultaneously check args and populate structs
 */
//alpha is a numeric matrix
// DONE - need to ensure the indices are fixed as 
// data structures in R have their indices start at 1
int convert_args(List docs_arg, NumericMatrix alpha,
                        dtnode* dirtree, IntegerVector leafmap,
                        IntegerVector f_arg,
                        model_params* mp, dataset* ds, counts* c)
                        
  {
    int i;
    int fmax = 0; // Will need to ensure alpha dim is this big
    int D = docs_arg.size();
    ds->D = D;

    // If f-labels were not specified, just set all to zero
    if (!Rf_isNull(f_arg) && Rf_length(f_arg) > 1)
      {
        ds->f.assign(D,0);
      }
    // Otherwise convert and check
    else
      {
        if(D != sizeof(f_arg)) 
          {
            // ERROR
            //PyErr_SetString(PyExc_RuntimeError,
            //                "f and docs have different lengths");
            stop("f and docs have different lengths");
			return BAD;            
          }
        for(i = 0; i < D; i++)
          {            
            ds->f.push_back(f_arg[i]);
            // If PyInt_AsLong fails, it returns -1
            // (also should not be getting neg f-values anyway)
            if(ds->f[i] < 0)
              {
                // ERROR
                //PyErr_SetString(PyExc_RuntimeError,
                //                "Non-numeric or negative f-value");
				        stop("Non-numeric or negative f-value");		
                return BAD;
              }
            else if(ds->f[i] > fmax)
              {
                fmax = ds->f[i];
              }
          }
      }
      
// Get info from parameters
    int W = leafmap.size();
//    Rcout << "Number of words " << W << "\n";
    
    ds->W = W;
    int F = alpha.nrow();
    int T = alpha.ncol();
    
    mp->alpha = alpha;
    mp->T = T;
    
    
	// not sure if 1-> by row or by col
    //mp->alphasum = (PyArrayObject*) PyArray_Sum(alpha,1,PyArray_DOUBLE,NULL);
	  mp->alphasum = sumR(alpha);
	
//    Rcout << "mp-> alphasum " <<  wrap(mp->alphasum[1]) << "\n";
//    Rcout << "fmax " << fmax << "\n";
//    Rcout << "F " << F << "\n";
  
    // Check max F
    if(fmax != (F - 1))
      {
        // ERROR
        //Py_DECREF(mp->alphasum);
        //delete(mp->alphasum);
		//PyErr_SetString(PyExc_RuntimeError,
        //                "Alpha/f dimensionality mismatch");
		    stop("Alpha/f dimensionality mismatch");
        return BAD;
      }

    // Check that all alpha values are non-negative
    //
    //double alphamin = PyFloat_AsDouble(PyArray_Min(alpha,NPY_MAXDIMS,NULL));
    //TODO overload min function
//	  double alphamin = min(minR(alpha));
//    if(alphamin < 0)       
//      {
//        // ERROR
//        //Py_DECREF(mp->alphasum);      
//		    //delete (mp->alphasum);
//        //PyErr_SetString(PyExc_RuntimeError,
//        //                "Negative Alpha value");
//		    stop("Negative Alpha value");
//        return BAD;
//      }

    // Read in and check leafmap (must be a valid permutation)
    //
    int* valid = new int[W];
    for(i = 0; i < W; i++)
      valid[i] = 0;
    mp->leafmap.reserve(W);
    for(i = 0; i < W; i++)
      {
        //leafmap is a numericvector
        //Fixing the leafmap index
        mp->leafmap[i] = leafmap[i]-1;
        
//        Rcout << "mp->leafmap[i] " << mp->leafmap[i] << "\n";
        //if(mp->leafmap[i] < 0 || mp->leafmap[i] > (W-1))
        // the leafmap start at 1 t/m W
        if(mp->leafmap[i] < 0 || mp->leafmap[i] > (W-1))
          {
            // ERROR
            //Py_DECREF(mp->alphasum);      
        		//delete (mp->alphasum);
            //PyErr_SetString(PyExc_RuntimeError,
            //               "Out-of-range value in leafmap");
			      stop("Out-of-range value in leafmap");
            delete[] valid;
            return BAD;
          }
        valid[mp->leafmap[i]] = 1;
      }    
    int sum = 0;
    for(i = 0; i < W; i++)
      sum += valid[i];
      
//    Rcout << "sum " << sum << "\n";  
    if(sum != W)
      {
        // ERROR
        //Py_DECREF(mp->alphasum);      
		    // delete (mp-> alphasum);
        //PyErr_SetString(PyExc_RuntimeError,
        //                "Out-of-range value in leafmap");
		    stop("Out-of-range value in leafmap");
        delete[] valid;
        return BAD;
      }
    delete[] valid;

    // Use dirtree arg to build Dirichlet Tree 
    // data structures for each topic
    // 
    
    // Populate counts struct

	//dirtree is a dtnode
    // Build Dirichlet Tree for each topic
    for(int ti = 0; ti < T; ti++)
      {
        try{
//          if(4 == sizeof(dirtree))
//            {
//              // Unpack tuple contents
//              PyArrayObject* cedges = (PyArrayObject*) 
//                PyTuple_GetItem(dirtree,0);
//              node* cchildren = PyTuple_GetItem(dirtree,1);
//              PyObject* cmaxind = PyTuple_GetItem(dirtree,2);
//              int cleafstart = dirtree[3]);
//        
//              // Build root node of Dirichlet Tree
//              dtnode* newtopic = new dtnode(cedges,cchildren,
//                                            cmaxind,cleafstart);
//              c->topics.push_back(newtopic);
//            }
          
//          else{
//            // ERROR
//            //Py_DECREF(mp->alphasum);      
//        			delete (mp->alphasum);      
//             //PyErr_SetString(PyExc_RuntimeError,
//             //                "Node has wrong number of elements");
//        			stop("Node has wrong number of elements");
//              return BAD;
//          }
            //FIXME I think a copy should be made for each topic

            // Build root node of Dirichlet Tree
//            Rcout << "Build root node of Dirichlet Tree\n";
//            Rcout << "dirtree edges" << (*dirtree).edges << "\n";
//            Rcout << "dirtree ichildren" << (*dirtree).ichildren << "\n";
//            Rcout << "dirtree maxind" << (*dirtree).maxind << "\n";
//            Rcout << "dirtree leafstart" << (*dirtree).leafstart << "\n";
            
//            dtnode* newtopic = new dtnode((*dirtree).edges, (*dirtree).ichildren,
//                                            (*dirtree).maxind, (*dirtree).leafstart);
            Rcout << "cloning dir tree\n";
            dtnode* newtopic = dirtree->clone();
            
            
            c->topics.push_back(newtopic);
        }
        catch (int err)
          {           
            // ERROR - called fcn has set err condition
            //Py_DECREF(mp->alphasum);      
			      //delete (mp->alphasum);
            return BAD;
          }        
      }

//  Rcout << "printing length of c->topics" << (c->topics).size() << "\n";

    // Convert documents from NumericMatrix to vector<int>
    //
    //TODO This part has to be fixed
    
    ds->doclens.resize(D);
    ds->docs.resize(D);
    for(int d = 0; d < D; d++)
      {
        NumericVector doc = docs_arg(d);
        
		    ds->doclens[d] = doc.size();
        vector<int> newdoc;
        newdoc.resize(ds->doclens[d]);
        ds->docs[d] = newdoc;
		    //is this necessary? coerce to vector<int> from NumericVector
        for(i = 0; i < ds->doclens[d]; i++)
          {
            //FIXED the indices for docs_arg[d][i]
            ds->docs[d][i] = doc[i] - 1;            
            if(ds->docs[d][i] < 0 || ds->docs[d][i] > (W - 1))
              {
                // ERROR
                for(int ti=0; ti < mp->T; ti++)
                  {
                    delete c->topics[ti];
                  }          
                //Py_DECREF(mp->alphasum);      
        				//delete (mp->alphasum);
                //PyErr_SetString(PyExc_RuntimeError,
        				//				"Non-numeric or out of range word");
        				stop("Non-numeric or out of range word");
                return BAD;
              }
          }
      }
      
//      Rcout << "populated d->docs \n";  
  

    // Everything went fine, return OK code
    //
    return OK;
}
 