#include <Rcpp.h>
using namespace Rcpp;

void
res_status_after_one_person(IntegerVector res_status_after,
			    int res_status_before_start_one_person,
			    IntegerVector is_arrival,
			    IntegerVector days_in_country,
			    IntegerVector days_obs,
			    int offset_crossing,
			    int n_crossing_one_person,
			    int dur_test,
			    int dur_threshold);

