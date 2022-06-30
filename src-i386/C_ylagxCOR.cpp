#include <Rcpp.h>
using namespace Rcpp;

//   about Rcpp
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/

// [[Rcpp::export]]
NumericVector C_ylagxCOR(NumericVector x, NumericVector y) {
  int lenx = x.size();
  int leny = y.size();
  NumericVector ans(leny-lenx+1);
  NumericVector y0(lenx);

  for(int i = 0; i < (leny-lenx+1); ++i) {
    //this does not seem to be slowing it down much...
    for(int k = 0; k < lenx; ++k){
      y0[k] = y[k + i];
    }
    double sumx = 0;
    double sumy = 0;
    double sumxy = 0;
    double sumx2 = 0;
    double sumy2 = 0;
    int n = 0;
    //above might be better as other data types
    for(int j = 0; j < lenx; ++j) {
      // the two if loops that follow appear to be adding
      // about 30% to time...
      if(!NumericVector::is_na(x[j]) && !NumericVector::is_na(y0[j])) {
        n += 1;
        sumx += x[j];
        sumx2 += (x[j]*x[j]);
        sumy += y0[j];
        sumy2 += (y0[j]*y0[j]);
        sumxy += (x[j]*y0[j]);
      }
    }
    if (n>0) {
      ans[i] = (sumxy - ((sumx * sumy)/n)) / sqrt((sumx2-((sumx*sumx)/n))*(sumy2-((sumy*sumy)/n)));
    }
  }
  return ans;
}


//from is_true(any(is_na(x)))
// assume is_false(is_na(x)) says is this single value not missing
// doesn't seem to....
// ended up with !NumericVector::is_na(x) as single value not missing
// noNA() might work?

//if
//if (testExpression) {
// statement(s) inside the body of if
// }
// else {
//  // statement(s) inside the body of else
//}


// this this should generate 1,1,1
// /*** R
// C_ylagxCOR(1:10, 1:12)
// */
