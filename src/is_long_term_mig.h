#include <Rcpp.h>
using namespace Rcpp;

void
is_long_term_mig_one_person(IntegerVector is_long_term_mig,
			    IntegerVector is_arrival,
			    IntegerVector res_status_before,
			    IntegerVector res_status_after,
			    IntegerVector days_in_country,
			    IntegerVector days_obs,
			    int dur_test,
			    int dur_threshold,
			    int offset_crossing,
			    int n_crossing_one_person);

