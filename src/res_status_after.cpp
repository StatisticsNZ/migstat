#include <Rcpp.h>
#include "res_status_after.h"
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector
res_status_after_internal(IntegerVector res_status_initial,
			  IntegerVector is_arrival,
			  IntegerVector days_in_country,
			  IntegerVector days_obs,
			  IntegerVector n_crossing_person,
			  int dur_test,
			  int dur_threshold) {
    int n_crossing = is_arrival.size();
    int n_person = n_crossing_person.size();
    IntegerVector res_status_after(n_crossing);
    int offset_crossing = 0;
    for (int i_person = 0; i_person < n_person; ++i_person) {
	int n_crossing_one_person = n_crossing_person[i_person];
	int res_status_initial_one_person = res_status_initial[offset_crossing];
	res_status_after_one_person(res_status_after,
				    res_status_initial_one_person,
				    is_arrival,
				    days_in_country,
				    days_obs,
				    offset_crossing,
				    n_crossing_one_person,
				    dur_test,
				    dur_threshold);
	offset_crossing += n_crossing_one_person;
    }
    return res_status_after;
}

// We return -1, rather than NA, when we cannot determine residence status.
// We convert the -1 to NA in the R function 'res_status_after'.
void
res_status_after_one_person(IntegerVector res_status_after,
			    int res_status_initial_one_person,
			    IntegerVector is_arrival,
			    IntegerVector days_in_country,
			    IntegerVector days_obs,
			    int offset_crossing,
			    int n_crossing_one_person,
			    int dur_test,
			    int dur_threshold) {
    int res_status_before_now = res_status_initial_one_person;
    int res_status_after_now;
    int i_now_offset;
    int i_now = 0;
    for ( ; (res_status_before_now != -1) && (i_now < n_crossing_one_person); ++i_now) {
	i_now_offset = i_now + offset_crossing;
	int days_obs_now = days_obs[i_now_offset];
	int days_in_country_now = days_in_country[i_now_offset];
	int is_arrival_now = is_arrival[i_now_offset];
	if (res_status_before_now == 0) { // R = 0
	    if (is_arrival_now) {
		if (days_obs_now >= dur_test - dur_threshold) { // O >= W - V
		    if (days_obs_now - days_in_country_now > dur_test - dur_threshold) { // O - H > W - V
			res_status_after_now = 0; // node (c)
		    }
		    else {
			if (days_obs_now >= dur_test) { // O >= W
			    res_status_after_now = 1; // node (d)
			}
			else {
			    if (days_obs_now - days_in_country_now > days_obs_now - dur_threshold) { // O - H > O - V
				res_status_after_now = -1; // node (e)
			    }
			    else {
				res_status_after_now = 1; // node (f)
			    }
			}
		    }
		}
		else {
		    res_status_after_now = -1; // node (b)
		}
	    }
	    else {
		res_status_after_now = 0; // node (a)
	    }
	}
	else { 			// R = 1
	    if (is_arrival_now) {
		res_status_after_now = 1; // node (a)
	    }
	    else {
		if (days_obs_now >= dur_test - dur_threshold) { // O >= W - V
		    if (days_in_country_now > dur_test - dur_threshold) { // H > W - V
			res_status_after_now = 1; // node (c)
		    }
		    else {
			if (days_obs_now >= dur_test) { // O >= W
			    res_status_after_now = 0; // node (d)
			}
			else {
			    if (days_in_country_now > days_obs_now - dur_threshold) { // H > O - V
				res_status_after_now = -1; // node (e)
			    }
			    else {
				res_status_after_now = 0; // node (f)
			    }
			}
		    }
		}
		else {
		    res_status_after_now = -1; // node (b)
		}
	    }
	}
	res_status_after[i_now_offset] = res_status_after_now;
	res_status_before_now = res_status_after_now;
    } // end for ( ; (res_status_before_now != -1) & (i_now < n_crossing_one_person); ++i_now)
    for ( ; i_now < n_crossing_one_person; ++i_now) {
	i_now_offset = i_now + offset_crossing;
	res_status_after[i_now_offset] = -1;
    }
}

