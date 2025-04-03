## Depends on: tidyverse, cli, glue
test_for_dupes <- function(aah_1, aah_2) {
    cli::cli_progress_step(
    msg = glue(
      "Testing that E1 and E2 data do not contain duplicate subjects..."
    ),
    msg_done = "Tested that E1 and E2 data do not contain duplicate subjects...."
  )
  
  dupes_detected <- FALSE
  
  e1 <- aah_1$subject |> unique()
  e2 <- aah_2$subject |> unique()
  n_dupes <- e1 %in% e2 |> which() |> length()
  
  if (n_dupes > 0) {dupes_detected == TRUE}
  if (dupes_detected) {
    cli:cli_abort(c(
      "Duplicates detected",
      "x" = str_c(
        "{n_dupes} duplicate subjects"
      )))
  }
}