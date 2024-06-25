## Functions to style plots
gg_style <- function(g) {
  g_styled <- g +
    theme_minimal(base_size = 10) +
    theme(aspect.ratio = 1 / 1,
          axis.ticks.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = NA),
          ## Set a white background (so pngs aren't transparent)
          panel.background = element_rect(fill = 'white', color = 'white'),
          plot.background = element_rect(fill = "white", color = "white")
    )
  return(g_styled)
}

## Common style function for all cor bin plots
gg_style_cor_bin <- function(g) {
  g_out <- g |>
    gg_style() +
    theme(
      aspect.ratio = 1 / 1,
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
      panel.grid.minor = element_line(color = "gray92", linewidth = .2),
      panel.grid.major.y = element_line(color = "gray92", linewidth = .4),
      panel.grid.major.x = element_line(color = "gray92", linewidth = .2),
      panel.border = element_rect(fill = NA, color = "gray50")
    )
  return(g_out)
}

gg_rt_2_lmer <- function(title,
                         plot_data,
                         plot_colors,
                         handedness_labeller = NULL,
                         direction_labels = NULL,
                         direction_labels_pos = NULL,
                         xlims = NULL,
                         xbreaks = NULL,
                         x_label = "Difference in RT between LVF and RVF (ms)",
                         n_subjects = list(
                           right = NA,
                           left = NA,
                         )) {
  ## LVF blias by level for each group, witih 95%CI
  plot_proc <- plot_data |>
    mutate(handedness = factor(handedness, levels = c("Right", "Left"))) |>
    mutate(level = factor(level, levels = c("Local", "Global")))
  
  ## make default handedness labeller
  if (is.null(handedness_labeller)) {
    # n_subjects <- rt_subject_plot_proc |> group_by(handedness) |> summarize(n = n_distinct(subject))
    handedness_labeller <- c(
      Right = str_c("Right handed \n (n = ", n_subjects$right, ")"),
      Left = str_c("Left handed \n (n = ", n_subjects$left, ")")
    )
  }
  
  ## Prepare data to annotate first facet
  data_facet1 <- plot_data |>
    filter(handedness == "Right")
  
  
  g <- ggplot(
    plot_proc,
    aes(
      y = level,
      x = estimate,
      fill = handedness,
      color = handedness
    )) +
    # geom_line(aes(group = handedness), show.legend = F) +
    geom_vline(xintercept = 0, color = "gray50", linewidth = .5) +
    geom_linerange(
      aes(xmin = upper.CL, xmax = lower.CL),
      color = "gray30",
      position = position_dodge(1),
      linewidth = .5
    ) +
    geom_point(
      color = "black",
      shape = 23,
      show.legend = F,
      size = 4,
      position = position_dodge(1)
    )  +
    facet_wrap(~ handedness, nrow = 2, strip.position = "left",
               labeller = labeller(handedness = handedness_labeller)) +
    {
      if (!is.null(xbreaks))
        scale_x_continuous(
          breaks = seq(-1000, 1000, xbreaks$major),
          minor_breaks = seq(-1000 , 1000, xbreaks$minor)
        )
    } +   
    {
      if (!is.null(xlims))
        coord_cartesian(xlim = c(xlims$lower, xlims$upper))
    } +
    
    {
      if (!is.null(direction_labels) & !is.null(direction_labels_pos))
        geom_text(
          data = data_facet1,
          color = "gray50",
          y = 1.5,
          x = direction_labels_pos$left,
          hjust = "left",
          label = str_c("⟵  \n",  direction_labels$left),
          size = 3
        )
    } +
    {
      if (!is.null(direction_labels) & !is.null(direction_labels_pos))
        geom_text(
          data = data_facet1,
          color = "gray50",
          y = 1.5,
          x = direction_labels_pos$right,
          hjust = "right",
          label = str_c("⟶ \n",  direction_labels$right),
          size = 3
        )
    } +
    scale_fill_manual(values = plot_colors[c(2, 1)]) +
    scale_color_manual(values = plot_colors[c(2, 1)]) +
    labs(title = title,
         y = "",
         x = x_label)
  
  g <- g |>
    gg_style() +
    theme(
      aspect.ratio = 1 / 4,
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
      strip.background = element_rect(fill = "gray99", color = "gray50"),
      strip.placement = "outside",
      panel.grid.minor = element_line(color = "gray92", linewidth = .2),
      panel.grid.major.y = element_line(color = "gray92", linewidth = .4),
      panel.grid.major.x = element_line(color = "gray92", linewidth = .2),
      panel.border = element_rect(fill = NA, color = "gray50"),
      ggh4x.facet.nestline = element_line(color = "gray50")
    )
  
  return(g)
}


gg_ehi_dotarea <- function(ehi_plot_data,
                           plot_color,
                           cutoff = 40,
                           cutoff_label_ypos = 90,
                           group_label_ypos = 170,
                           y_lims = c(0, 200),
                           hjust_cutoff_label_left = "right",
                           hjust_cutoff_label_right = "right") {
  
  counts <- ehi_plot_data |> 
    mutate(hand_var = case_when(
      ehi <= -1*cutoff ~ "Left",
      ehi >= cutoff ~ "Right",
      ehi > -1*cutoff & ehi < cutoff ~ "Mixed")
    ) |>
    group_by(hand_var) |> 
    summarize(n = sum(n))
  
  n_left <- counts |> filter(hand_var == "Left") |> pull(n)
  n_mixed <- counts |> filter(hand_var == "Mixed") |> pull(n)
  n_right <- counts |> filter(hand_var == "Right") |> pull(n)
  
  line_color = plot_color
  
  g <- ggplot(ehi_plot_data, aes(x = ehi, y = n)) +
    geom_vline(xintercept = -1 * cutoff, color = "gray50") +
    geom_vline(xintercept = 1 * cutoff, color = "gray50") +
    geom_area(
      fill = plot_color,
      alpha = .5,
      color = NA,
      linewidth = 0
    ) +
    geom_line(color = line_color, linewidth = .5) +
    geom_point(fill = plot_color,
               shape = 21,
               size = 3) +
    annotate("text", x = -69, y = group_label_ypos,
             label = str_c("Left \n (n = ", n_left, ")"), size = 3.5) +
    annotate("text", x = 0, y = group_label_ypos,
             label = str_c("Mixed \n (n = ", n_mixed, ")"), size = 3.5) +
    annotate("text", x = 69, y = group_label_ypos,
             label = str_c("Right \n (n = ", n_right, ")"), size = 3.5) +
    annotate("text", x = -1 * cutoff - 1, y = cutoff_label_ypos,
             label = str_c("-", cutoff), size = 3, hjust = hjust_cutoff_label_left) +
    annotate("text", x = cutoff - 1, y = cutoff_label_ypos,
             label = str_c("+", cutoff), size = 3, hjust = hjust_cutoff_label_right) +
    scale_x_continuous(
      breaks = seq(-100, 100, 25),
      minor_breaks = seq(-100, 100, 25),
      expand = expansion(mult = c(.025, .025))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    ylim(y_lims[1], y_lims[2]) + 
    labs(x = "Laterality score from 4-item Veale Edinburgh Handedness Inventory (EHI)",
         y = "Number of participants",
         title = "Hand preference distribution")
  
  g <- g + theme_minimal(base_size = 10) +
    theme(
      aspect.ratio = 1 / 2,
      plot.title = element_text(margin = margin(b = 10, unit = "pt"),
                                hjust = 0.5),
      axis.ticks.x = element_blank(),
      axis.title.x = element_text(margin = margin(t = 8, unit = "pt")),
      axis.ticks.y = element_blank(),
      axis.title.y = element_text(margin = margin(r = 5, unit = "pt")),
      panel.grid.major.y = element_line(color = "gray92", linewidth = .4),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(color = "gray92", linewidth = .4),
      panel.grid.minor.x = element_blank(),
    )
  
  return(g)
}




## Continuous plots
gg_rt_2_line <- function(title = "",
                         y_title = "DV",
                         rt_subject_plot,
                         plot_colors,
                         plot_color = "black",
                         group_by_level = FALSE,
                         direction_labels = NULL,
                         direction_labels_pos = NULL,
                         ylims = NULL,
                         ybreaks = NULL) {
  
  ## Calculate mean, sem by quantile.
  summary_data <- rt_subject_plot |>
    group_by(ehi, level) |>
    summarize(
      # for sanity check
      mean_dv = mean(dv),
      median_dv = median(dv),
      n = n(),
      sem_dv = sd(dv) / sqrt(n)
    )
  
  
  #### Function to plot 1-box graph
  ## Prepare data to annotate first facet
  data_facet1 <- summary_data
  
  ## Make plot
  g <- ggplot(data = rt_subject_plot, aes(x = ehi, y = dv)) +
    stat_summary(
      fun.data = mean_se,
      geom = "linerange",
      linetype = 1,
      color = "gray70",
      show.legend = F,
      linewidth = .5
    ) +
    geom_point(
      data = summary_data,
      aes(y = mean_dv, size = n),
      fill = plot_color,
      color = "gray20",
      shape = 21,
      show.legend = F
    ) +
    geom_hline(yintercept = 0,
               color = "gray50",
               linewidth = .5) +
    # scale_fill_manual(values = h_plot_colors) + ## to color handedness groups
    # scale_x_discrete(labels = x_labels) +
    {
      if (!is.null(ybreaks))
        scale_y_continuous(
          breaks = seq(-1000, 1000, ybreaks$major),
          minor_breaks = seq(-1000 , 1000, ybreaks$minor)
        )
    } +
    {
      if (!is.null(ylims))
        coord_cartesian(ylim = c(ylims$lower, ylims$upper))
    } +
    {
      if (!is.null(direction_labels) & !is.null(direction_labels_pos))
        geom_text(
          data = data_facet1,
          color = "gray50",
          x = 0,
          y = direction_labels_pos$up,
          hjust = "center",
          label = str_c("↑ \n",  direction_labels$up) ,
          size = 3
        )
    } +
  {
    if (!is.null(direction_labels) & !is.null(direction_labels_pos))
      geom_text(
        data = data_facet1,
        color = "gray50",
        x = 0,
        y = direction_labels_pos$down,
        hjust = "center",
        label = str_c(direction_labels$down, "\n ↓"),
        size = 3
      )
  } +
  labs(title = title,
       y = y_title,
       x = "Handedness laterality score")
  return(g)
}

gg_rt_1_line <- function(title = "",
                         y_title = "DV",
                         rt_subject_plot,
                         plot_colors,
                         plot_color = "black",
                         group_by_level = FALSE,
                         direction_labels = NULL,
                         direction_labels_pos = NULL,
                         ylims = NULL,
                         ybreaks = NULL) {
  
  ## Calculate mean, sem by quantile.
  summary_data <- rt_subject_plot |>
    group_by(ehi) |>
    summarize(
      # for sanity check
      mean_dv = mean(dv),
      median_dv = median(dv),
      n = n(),
      sem_dv = sd(dv) / sqrt(n)
    )
  
  
  #### Function to plot 1-box graph
  ## Prepare data to annotate first facet
  data_facet1 <- summary_data
  
  ## Make plot
  g <- ggplot(data = rt_subject_plot, aes(x = ehi, y = dv)) +
    stat_summary(
      fun.data = mean_se,
      geom = "linerange",
      linetype = 1,
      color = "gray70",
      show.legend = F,
      linewidth = .5
    ) +
    geom_point(
      data = summary_data,
      aes(y = mean_dv, size = n),
      fill = plot_color,
      color = "gray20",
      shape = 21,
      show.legend = F
    ) +
    geom_hline(yintercept = 0,
               color = "gray50",
               linewidth = .5) +
    {
      if (!is.null(ybreaks))
        scale_y_continuous(
          breaks = seq(-1000, 1000, ybreaks$major),
          minor_breaks = seq(-1000 , 1000, ybreaks$minor)
        )
    } +
    {
      if (!is.null(ylims))
        coord_cartesian(ylim = c(ylims$lower, ylims$upper))
    } +
    {
      if (!is.null(direction_labels) & !is.null(direction_labels_pos))
        geom_text(
          data = data_facet1,
          color = "gray50",
          x = 10,
          y = direction_labels_pos$up,
          hjust = "center",
          label = str_c("↑ \n",  direction_labels$up) ,
          size = 3
        )
    } +

  labs(title = title,
       y = y_title,
       x = "EHI laterality score")
  return(g)
}