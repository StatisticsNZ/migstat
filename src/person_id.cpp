#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
void
person_id_internal(IntegerVector personId,
		   IntegerVector personId_is_na,
		   IntegerVector match1,
		   IntegerVector match2,
		   int first_id) {
  int n = personId.size();
  int new_id = first_id;
  for (int i = 0; i < n; i++) {
    if (personId_is_na[i]) {
      int first_time_seen_key1 = match1[i] == i + 1;
      int first_time_seen_key2 = match2[i] == i + 1;
      if (first_time_seen_key1 && first_time_seen_key2) {
	personId[i] = new_id;
	new_id += 1;
      }
      else if (first_time_seen_key1) {
	personId[i] = personId[match2[i] - 1];
      }
      else {
	personId[i] = personId[match1[i] - 1];
      }
    }
  }
}

