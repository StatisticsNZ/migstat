

#' Plot a migration history.
#'
#' Given a sequence of border crossings for a person,
#' draw a diagram describing that person's migration
#' history.
#'
#' Note that, unlike elsewhere in package \code{migstat}, the
#' \code{date_crossing} and \code{is_arrival} arguments for
#' \code{plot_mig_hist} refer to a single individual.
#'
#' If values for \code{date_first} and \code{date_last} are not supplied,
#' then defaults are calculated, based on the length of the travel history.
#'
#' @param date_crossing A vector of dates.
#' @param is_arrival A logical vector, the same length as \code{date_crossing}
#' specifying whether each border crossing is an arrival.
#' @param res_status_before_str Character or numeric vector,
#' the same length as \code{date_crossing}, showing residence
#' status before each crossing. Optional.
#' @param res_status_after_str Character or numeric vector,
#' the same length as \code{date_crossing}, showing residence
#' status after each crossing.
#' @param date_first The start date for the travel history. Optional.
#' @param date_last The end date for the travel history. Optional.
#' @param show_dates Logical. Whether to display the dates of each border
#' crossing.
#' @param show_days Logical. Whether to display the length, in days, of each
#' spell in or out of the country.
#' @param cex 'Character expansion factor'. A number. Larger values
#' lead to larger text. Defaults to 1.
#' @param lwd Line width. A number. Larger values lead to thicker lines.
#' Defaults to 1.
#' 
#' @return Returns \code{NULL}, but as a side effect draws a graph
#' (using R's traditional graphics system).
#'
#' @examples
#' date_crossing <- as.Date(c("2000-01-01", "2000-02-05", "2000-03-01"))
#' is_arrival <- c(FALSE, TRUE, FALSE)
#' plot_mig_hist(date_crossing = date_crossing,
#'               is_arrival = is_arrival)
#' @export
plot_mig_hist <- function(date_crossing, is_arrival,
                          res_status_before_str = NULL, res_status_after_str = NULL,
                          date_first = NULL, date_last = NULL,
                          show_dates = TRUE, show_days = TRUE,
                          cex = 1, lwd = 1) {
    date_crossing <- check_and_tidy_date_crossing(date_crossing)
    check_is_arrival(is_arrival)
    check_same_length(e1 = date_crossing,
                      e2 = is_arrival,
                      name1 = "date_crossing",
                      name2 = "is_arrival")
    check_res_status_str_date_crossing(res_status_before_str = res_status_before_str,
                                       res_status_after_str = res_status_after_str,
                                       date_crossing = date_crossing)
    date_first <- check_and_tidy_date_first_last(date = date_first,
                                                 date_crossing = date_crossing,
                                                 name = "date_first")
    date_last <- check_and_tidy_date_first_last(date = date_last,
                                                date_crossing = date_crossing,
                                                name = "date_last")
    ## need tests for show_dates, show_days
    check_positive_number(number = cex,
                          name = "cex")
    check_positive_number(number = lwd,
                          name = "lwd")
    n <- length(date_crossing)
    if (identical(n, 0L))
        stop(gettextf("'%s' has length %d",
                      "date_crossing", 0L))
    segment_coord_horiz <- segment_coord_horiz(date_crossing = date_crossing,
                                               is_arrival = is_arrival,
                                               date_first = date_first,
                                               date_last = date_last)
    segment_coord_vert <- segment_coord_vert(date_crossing = date_crossing,
                                             is_arrival = is_arrival)
    x <- range(segment_coord_horiz$x0, segment_coord_horiz$x1)
    y <- range(segment_coord_horiz$y0, segment_coord_horiz$y1)
    if (!is.null(res_status_before_str))
        y <- 1.25 * y ## make room for labels
    graphics::plot(x = x,
                   y = y,
                   type = "n",
                   axes = FALSE,
                   xlab = "",
                   ylab = "")
    graphics::segments(x0 = segment_coord_horiz$x0,
                       x1 = segment_coord_horiz$x1,
                       y0 = segment_coord_horiz$y0,
                       y1 = segment_coord_horiz$y1,
                       lwd = lwd)
    graphics::segments(x0 = segment_coord_vert$x0,
                       x1 = segment_coord_vert$x1,
                       y0 = segment_coord_vert$y0,
                       y1 = segment_coord_vert$y1,
                       lwd = lwd)
    graphics::abline(h = 0,
                     lty = "dotted",
                     lwd = lwd)
    graphics::mtext(text = "Out",
                    side = 2,
                    las = 1,
                    line = -1,
                    cex = 0.8 * cex,
                    at = 0.2,
                    col = "grey20")
    graphics::mtext(text = "In",
                    side = 2,
                    las = 1,
                    cex = 0.8 * cex,
                    line = -1,
                    at = -0.2,
                    col = "grey20")
    if (!is.null(res_status_before_str)) {
        ## before
        graphics::text(x = segment_coord_vert$x0,
                       y = segment_coord_vert$y0,
                       labels = res_status_before_str,
                       pos = ifelse(is_arrival, 3, 1),
                       cex = cex)
        ## after
        graphics::text(x = segment_coord_vert$x1,
                       y = segment_coord_vert$y1,
                       labels = res_status_after_str,
                       pos = ifelse(is_arrival, 1, 3),
                       cex = cex)
    }
    if (show_dates) {
        graphics::mtext(text = date_crossing,
                        side = 1,
                        at = date_crossing,
                        cex = cex)
    }
    if (show_days) {
        days <- date_last - date_crossing[n]
        at <- date_crossing[n] + 0.5 * days
        if (n > 1L) {
            days <- c(diff(date_crossing), days)
            at <- c(date_crossing[-n] + 0.5 * diff(date_crossing), at)
        }
        text <- ifelse(days > 0, sprintf("%d days", days), "")
        side <- ifelse(is_arrival, 1, 3)
        graphics::mtext(text = text,
                        side = side,
                        at = at,
                        cex = cex)
    }
    invisible(NULL)
}

check_res_status_plot <- function(res_status) {
    if (!is.numeric(res_status) && !is.character(res_status))
        stop(gettextf("'%s' has class \"%s\"",
                      "res_status", class(res_status)))
    if (!all(res_status[!is.na(res_status)] %in% c(0L, 1L, "?", "")))
        stop(gettextf("'%s' has values other than 0, 1, ?, and <blank>",
                      "name"))
    NULL
}

check_res_status_plot_and_date_crossing <- function(res_status, date_crossing) {
    n_res <- length(res_status)
    n_date <- length(date_crossing)
    if (!identical(n_res, 2L * n_date))
        stop(gettextf("'%s' has length '%d' but '%s' has length '%d'",
                      "res_status", n_res, "date_crossing", n_date))
    NULL
}

segment_coord_horiz <- function(date_crossing, is_arrival, date_first, date_last) {
    n <- length(date_crossing)
    x0 <- c(date_first, date_crossing)
    x1 <- c(date_crossing, date_last)
    y0 <- ifelse(is_arrival, 1, -1)
    y0 <- c(y0, -y0[n])
    y1 <- y0
    list(x0 = x0,
         x1 = x1,
         y0 = y0,
         y1 = y1)
}

segment_coord_vert <- function(date_crossing, is_arrival) {
    n <- length(date_crossing)
    x0 <- date_crossing
    x1 <- date_crossing
    y0 <- rep(-1L + 2L * is_arrival, times = n)
    y1 <- rep(1L - 2L * is_arrival, times = n)
    list(x0 = x0,
         x1 = x1,
         y0 = y0,
         y1 = y1)
}
