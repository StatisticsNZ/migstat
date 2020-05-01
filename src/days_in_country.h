#include <Rcpp.h>
using namespace Rcpp;

void
days_in_country_one_person(IntegerVector days_in_country,
			   IntegerVector date_crossing,
			   IntegerVector is_arrival,
			   int offset_crossing,
			   int n_crossing_one_person,
			   int date_obs_end,
			   int dur_test);

