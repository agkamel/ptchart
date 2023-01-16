bounce_total <- function(day, log10_freq) {
  bounce_up <- bounce_up(day, log10_freq, raw = FALSE)
  bounce_down <- bounce_down(day, log10_freq, raw = FALSE)

  bounce_up * bounce_down
}
