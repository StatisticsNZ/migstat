#include <Rcpp.h>
#include "days_in_country.h"
using namespace Rcpp;


// [[Rcpp::export]]
IntegerVector
days_in_country_internal(IntegerVector date_crossing,
			 IntegerVector is_arrival,
			 IntegerVector n_crossing_person,
			 int date_obs_end,
			 int dur_test) {
    int n_crossing = date_crossing.size();
    int n_person = n_crossing_person.size();
    IntegerVector days_in_country(n_crossing);
    int offset_crossing = 0;
    for (int i_person = 0; i_person < n_person; ++i_person) {
	int n_crossing_one_person = n_crossing_person[i_person];
	days_in_country_one_person(days_in_country,
				   date_crossing,
				   is_arrival,
				   offset_crossing,
				   n_crossing_one_person,
				   date_obs_end,
				   dur_test);
	offset_crossing += n_crossing_one_person;
    }
    return days_in_country;
}
    
// In principle could save some calculations by creating vector of spell
// lengths, but savings likely to be small because most histories
// short, and downside is more complexity.}
void
days_in_country_one_person(IntegerVector days_in_country,
			   IntegerVector date_crossing,
			   IntegerVector is_arrival,
			   int offset_crossing,
			   int n_crossing_one_person,
			   int date_obs_end,
			   int dur_test) {
  for (int i_now = 0; i_now < n_crossing_one_person; ++i_now) {
    int i_now_offset = i_now + offset_crossing;
    int date_now = date_crossing[i_now_offset];
    if (date_now >= date_obs_end)
      break;
    int date_test_end = date_now + dur_test;
    int date_end = date_test_end < date_obs_end ? date_test_end : date_obs_end;
    for (int i_spell = i_now; i_spell < n_crossing_one_person; ++i_spell) {
      int i_spell_offset = i_spell + offset_crossing;
      int date_spell_start = date_crossing[i_spell_offset];
      if (date_spell_start >= date_end)
	break;
      int spell_starts_with_arrival = is_arrival[i_spell_offset];
      int is_last_spell = i_spell == n_crossing_one_person - 1;
      int date_spell_end;
      if (is_last_spell) {
	if (spell_starts_with_arrival)
	  date_spell_end = date_end;
	else
	  break;
      }
      else {
	int spell_ends_with_arrival = is_arrival[i_spell_offset + 1];
	if (!spell_starts_with_arrival && spell_ends_with_arrival)
	  continue;
	date_spell_end = date_crossing[i_spell_offset + 1];
	if (spell_starts_with_arrival && spell_ends_with_arrival)
	  date_spell_end = date_spell_start + (date_spell_end - date_spell_start) / 2;
	if (!spell_starts_with_arrival && !spell_ends_with_arrival) {
	  date_spell_start = date_spell_start + (date_spell_end - date_spell_start) / 2;
	  if (date_spell_start >= date_end)
	    break;
	}
	date_spell_end = date_spell_end < date_end ? date_spell_end : date_end;
      }
      days_in_country[i_now_offset] += date_spell_end - date_spell_start;
    }
  }
}

