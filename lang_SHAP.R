#SHAP values across discharge and access models

##Script timer

start_time <- Sys.time()

##Functions

###SHAP bar plot
shapbar <- function(df, labeltype) {
  shap_filtered <- df %>%
    filter(!is.na(token)) %>%
    dplyr::slice(1:40) %>%
    mutate(token = make.unique(token)) %>%
    mutate(token = str_replace(token, ".1", "(2)"))

  shap_filtered$token <- factor(
    shap_filtered$token,
    levels = shap_filtered %>%
      arrange(total_abs_shap) %>%
      select(token) %>%
      unlist()
  )

  shapbarchart <- ggplot(shap_filtered, aes(x = token, y = total_abs_shap)) +
    geom_col(fill = "#00BFC4") +
    coord_flip() +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    ggtitle(glue(
      "Top 40 tokens by total SHAP value for predicting\n antibiotic on discharge (participant {i+1})"
    )) +
    ylab("Total SHAP value") +
    xlab("Token")

  ggsave(
    filename = glue("{labeltype}dischargeab_shap.png"),
    plot = shapbarchart,
    width = 10,
    height = 10,
    units = "in",
    dpi = 300
  )
}

###SHAP bar chart (Access)
ac_shapbar <- function(df, labeltype) {
  shap_filtered <- df %>%
    filter(!is.na(token)) %>%
    dplyr::slice(1:40) %>%
    mutate(token = make.unique(token)) %>%
    mutate(token = str_replace(token, ".1", "(2)"))

  shap_filtered$token <- factor(
    shap_filtered$token,
    levels = shap_filtered %>%
      arrange(total_abs_shap) %>%
      select(token) %>%
      unlist()
  )

  shapbarchart <- ggplot(shap_filtered, aes(x = token, y = total_abs_shap)) +
    geom_col(fill = "#F8766D") +
    coord_flip() +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    ggtitle(glue(
      "Top 40 tokens by total SHAP value for predicting\n Access antibiotic on discharge (participant {i+1})"
    )) +
    ylab("Total SHAP value") +
    xlab("Token")

  ggsave(
    filename = glue("ac_{labeltype}dischargeab_shap.png"),
    plot = shapbarchart,
    width = 10,
    height = 10,
    units = "in",
    dpi = 300
  )
}

##iterate over questionnaire discharge letters

for (i in 0:5) {
  shapdf <- read_csv(glue("shaptokens_df_{i}.csv"))

  shapbar(shapdf, labeltype = i)

  ac_shapdf <- read_csv(glue("access_shaptokens_df_{i}.csv"))

  ac_shapbar(ac_shapdf, labeltype = i)
}

##Record time taken to run the script

end_time <- Sys.time()
time_taken <- end_time - start_time
time_taken <- as.numeric(time_taken, units = "secs")
time_df1 <- data.frame(matrix(ncol = 2, nrow = 1))
time_df1[1, ] <- c("lang_SHAP.R", time_taken)
colnames(time_df1) <- c("Script", "Time (secs)")
time_df <- read_csv("script_times.csv")
time_df <- rbind(time_df, time_df1)
write_csv(time_df, "script_times.csv")

##Total timings

seconds_to_hms <- function(secs) {
  h <- floor(secs / 3600)
  m <- floor((secs %% 3600) / 60)
  s <- round(secs %% 60, 1)
  glue::glue("{h}h {m}m {s}s")
}

time_df |>
  summarise(`Time (secs)` = sum(as.numeric(`Time (secs)`))) |>
  mutate(Script = "Total") |>
  relocate(Script, .before = `Time (secs)`) |>
  rbind(time_df) |>
  mutate(`Time (secs)` = as.numeric(`Time (secs)`)) |>
  mutate(`Time (hms)` = seconds_to_hms(`Time (secs)`)) |>
  select(-`Time (secs)`) |>
  write_csv("script_times.csv")
