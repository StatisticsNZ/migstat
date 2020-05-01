
library(dplyr)

border_crossings <- data.frame(name = c("Amy", "Amy", "Amy", "Amy",
                                            "Amy", "Amy",
                                        "Bader", "Bader", "Bader",
                                        "Cai", "Cai", "Cai", "Cai",
                                            "Cai", "Cai", "Cai",
                                        "David",
                                        "Eisha", "Eisha", "Eisha", "Esha",
                                            "Eisha", "Eisha",
                                        "Fatima"),
                               passport_no = c("A101", "A101", "A101", "A101",
                                                   "A101", "A101",
                                               "B102", "B102", "B102",
                                               "C103", "X200", "C103", "C103",
                                                   "C103", "X200", "C103",
                                               "D104",
                                               "E105", "E105", "E105", "E105",
                                                   "E105", "E105",
                                               "F106"),
                               datetime_crossing = c("2009-05-18 12:01:33", "2009-05-25 01:23:54", "2010-05-13 22:57:44", "2010-05-20 05:31:54",
                                                          "2011-11-27 14:22:10", "2011-12-05 16:13:09",
                                                      "2010-08-12 15:20:13", "2010-09-03 17:31:00", "2010-10-01 11:10:49",
                                                      "2009-05-02 07:23:41", "2010-02-15 19:23:04", "2010-02-25 06:31:28", "2010-06-02 15:43:22",
                                                           "2011-01-22 22:43:09", "2011-06-28 11:15:02", "2011-07-12 20:15:03",
                                                      "2010-02-16 18:32:03",
                                                      "2009-10-06 13:09:33", "2009-10-21 21:51:03", "2010-05-28 14:12:32", "2010-08-27 16:06:48",
                                                          "2011-07-13 22:16:54", "2011-08-09 13:41:03",
                                                      "2011-10-02 19:32:01"),
                               direction = c("Arrival", "Departure", "Arrival", "Departure",
                                                 "Arrival", "Departure",
                                             "Arrival", "Departure", "Arrival",
                                             "Arrival", "Departure", "Arrival",
                                                 "Departure", "Arrival", "Departure", "Arrival",
                                             "Departure",
                                             "Departure", "Arrival", "Departure", "Arrival",
                                                 "Departure", "Arrival",
                                             "Arrival"),
                               res_status_default = c(0L, 0L, 0L, 0L, 0L, 0L,
                                                      0L, 1L, 0L,
                                                      1L, 1L, 1L, 1L, 0L, 1L, 1L,
                                                      1L,
                                                      1L, 1L, 1L, 1L, 1L, 1L,
                                                      0L),
                               stringsAsFactors = FALSE) %>%
  arrange(datetime_crossing) %>%
  mutate(crossing_id = row_number()) %>%
  select(crossing_id, everything())


save(border_crossings,
     file = "data/border_crossings.rda")


