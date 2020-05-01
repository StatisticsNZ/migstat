#include <Rcpp.h>
#include "is_long_term_mig.h"
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector
  is_long_term_mig_internal(IntegerVector is_arrival,
                            IntegerVector res_status_before,
                            IntegerVector res_status_after,
                            IntegerVector days_in_country,
                            IntegerVector days_obs,
                            int dur_test,
                            int dur_threshold,
                            IntegerVector n_crossing_person) {
    int n_crossing = is_arrival.size();
    int n_person = n_crossing_person.size();
    IntegerVector is_long_term_mig(n_crossing);
    int offset_crossing = 0;
    for (int i_person = 0; i_person < n_person; ++i_person) {
      int n_crossing_one_person = n_crossing_person[i_person];
      is_long_term_mig_one_person(is_long_term_mig,
                                  is_arrival,
                                  res_status_before,
                                  res_status_after,
                                  days_in_country,
                                  days_obs,
                                  dur_test,
                                  dur_threshold,
                                  offset_crossing,
                                  n_crossing_one_person);
      offset_crossing += n_crossing_one_person;
    }
    return is_long_term_mig;
  }


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
                              int n_crossing_one_person) {
    int is_arrival_last_known;
    int res_status_before_last_known;
    int days_in_country_last_known;
    int days_obs_last_known;
    int diff_days;
    int diff_test = 2 * dur_threshold - dur_test;
    int cannot_be_long_term;
    for (int i_now = 0; i_now < n_crossing_one_person; ++i_now) {
      int i_now_offset = i_now + offset_crossing;
      int is_arrival_now = is_arrival[i_now_offset];
      int res_status_before_now = res_status_before[i_now_offset];
      int res_status_after_now = res_status_after[i_now_offset];
      int days_in_country_now = days_in_country[i_now_offset];
      int days_obs_now = days_obs[i_now_offset];
      int observed_res_status_before = res_status_before_now != -1;
      int observed_res_status_after = res_status_after_now != -1;
      int observed_both_statuses = observed_res_status_before && observed_res_status_after;
      if (observed_both_statuses)
        is_long_term_mig[i_now_offset] = res_status_before_now != res_status_after_now;
      else {
        if (observed_res_status_before) { // last known res status
          is_long_term_mig[i_now_offset] = -1;
          is_arrival_last_known = is_arrival_now;
          res_status_before_last_known = res_status_before_now;
          days_in_country_last_known = days_in_country_now;
          days_obs_last_known = days_obs_now;
        }
        else { 		//  i_now > 0, since res status before always observed for first crossing
          if (is_arrival_last_known) {
            if (is_arrival_now)
              is_long_term_mig[i_now_offset] = -1;
            else {
                // diff_days is time spent in country between last known
                // partially-classified crossing and current crossing
              diff_days = days_in_country_last_known - days_in_country_now;
              cannot_be_long_term = (res_status_before_last_known == 0) && (diff_days < diff_test);
              is_long_term_mig[i_now_offset] = cannot_be_long_term ? 0 : -1;
            }
          }
          else {
            if (is_arrival_now) {
                // diff_days is time spent out of country between last known
                // partially-classified crossing and current crossing
                diff_days = days_obs_last_known - days_in_country_last_known - days_obs_now + days_in_country_now;
              cannot_be_long_term = (res_status_before_last_known == 1) && (diff_days < diff_test);
              is_long_term_mig[i_now_offset] = cannot_be_long_term ? 0 : -1;
            }
            else
              is_long_term_mig[i_now_offset] = -1;
          }
        }
      }
    }
  }



