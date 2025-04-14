## Given names of stimuli in left and right field
## (In a tibble with columns "stimulus left" and "stimulus right"),
## create columns indicating target shape, level, and field:
## target (square, rectangle, absent)
## level (global, local)
## field (LVF, RVF)
code_target_level_field <- function(data_raw) {
  global_square_stimuli <- c("stimuli_target_squarediamonds.svg",
                             "stimuli_target_squaretriangles.svg")
  local_square_stimuli <- c("stimuli_target_diamondsquares.svg",
                            "stimuli_target_trianglesquares.svg")
  global_rectangle_stimuli <- c("stimuli_target_rectanglediamonds.svg",
                             "stimuli_target_rectangletriangles.svg")
  local_rectangle_stimuli <- c("stimuli_target_diamondrectangles.svg",
                            "stimuli_target_trianglerectangles.svg")
  
  target_stimuli <- c(
    global_square_stimuli,
    local_square_stimuli,
    global_rectangle_stimuli,
    local_rectangle_stimuli
  )
  absent_stimuli <- c(
    "stimuli_distractor_diamondtriangles.svg",
    "stimuli_distractor_trianglediamonds.svg"
  )
  
  square_stimuli <- c(global_square_stimuli, local_square_stimuli)
  rectangle_stimuli <- c(global_rectangle_stimuli, local_rectangle_stimuli)
  global_stimuli <- c(global_square_stimuli, global_rectangle_stimuli)
  local_stimuli <- c(local_square_stimuli, local_rectangle_stimuli)
  
  data_recoded <- data_raw |>
    mutate(
      target = case_when(
        (stimulus_left %in% square_stimuli)
        | (stimulus_right %in% square_stimuli) ~ "square",
        (stimulus_left %in% rectangle_stimuli)
        | (stimulus_right %in% rectangle_stimuli) ~ "rectangle",
        TRUE ~ "absent"
      ),
      level = case_when(
        (stimulus_left %in% global_stimuli)
        | (stimulus_right %in% global_stimuli) ~ "global",
        (stimulus_left %in% local_stimuli)
        | (stimulus_right %in% local_stimuli) ~ "local",
        TRUE ~ "absent"
      ),
      field = case_when(
        (stimulus_left %in% target_stimuli) ~ "LVF",
        (stimulus_right %in% target_stimuli) ~ "RVF",
        TRUE ~ "absent"
      )
    )
  return(data_recoded)
  
}