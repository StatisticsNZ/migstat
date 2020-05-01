#include <Rcpp.h>
using namespace Rcpp;

void
person_id_internal(IntegerVector personId,
		   IntegerVector personId_is_na,
		   IntegerVector match1,
		   IntegerVector match2,
		   int first_id);
