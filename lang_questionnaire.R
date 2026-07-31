#Questionnaire results

##Set seed

set.seed(123)

##Script timer

start_time <- Sys.time()

##Functions

###Iteration over decision thresholds
decision_iterator <- function(df, ref_df, thresholds, model) {
  cumul_cm_sens <- data.frame(matrix(
    nrow = length(ref_df$byClass) + 6,
    ncol = length(thresholds)
  ))
  rownames(cumul_cm_sens) <- c(
    names(ref_df$byClass),
    "Cohen's kappa",
    "True negatives",
    "True positives",
    "False negatives",
    "False positives",
    "Total flagged"
  )

  for (i in seq_along(thresholds)) {
    if (model == "overall") {
      cumul_temp_rx <- df |>
        mutate(bert_inapp = ifelse((1 - prob) >= thresholds[i], 1, 0)) |>
        mutate(bert_inapp = as.factor(bert_inapp), answer = as.factor(answer))
    } else {
      cumul_temp_rx <- df |>
        mutate(bert_inapp = ifelse(prob >= thresholds[i], 1, 0)) |>
        mutate(bert_inapp = as.factor(bert_inapp), answer = as.factor(answer))
    }

    cumul_cm_temp <- confusionMatrix(
      cumul_temp_rx$bert_inapp,
      cumul_temp_rx$answer,
      positive = "1"
    )
    cohen.kappa(as.matrix(cumul_temp_rx[, c("bert_inapp", "answer")]))
    cumul_cm_tempsens <- cumul_cm_temp$byClass |>
      data.frame() |>
      rename(!!as.character(thresholds[i]) := 1)
    ck <- cohen.kappa(as.matrix(cumul_temp_rx[, c(
      "bert_inapp",
      "answer"
    )]))$kappa |>
      data.frame()
    rownames(ck) <- "Cohen's kappa"
    colnames(ck) <- as.character(thresholds[i])
    cumul_tn <- cumul_cm_temp$table[1, 1] |> data.frame()
    rownames(cumul_tn) <- "True negatives"
    colnames(cumul_tn) <- as.character(thresholds[i])
    cumul_tp <- cumul_cm_temp$table[2, 2] |> data.frame()
    rownames(cumul_tp) <- "True positives"
    colnames(cumul_tp) <- as.character(thresholds[i])
    cumul_fn <- cumul_cm_temp$table[1, 2] |> data.frame()
    rownames(cumul_fn) <- "False negatives"
    colnames(cumul_fn) <- as.character(thresholds[i])
    cumul_fp <- cumul_cm_temp$table[2, 1] |> data.frame()
    rownames(cumul_fp) <- "False positives"
    colnames(cumul_fp) <- as.character(thresholds[i])
    total_pos <- cumul_cm_temp$table[2, 1] +
      cumul_cm_temp$table[2, 2] |> data.frame()
    rownames(total_pos) <- "Total flagged"
    colnames(total_pos) <- as.character(thresholds[i])
    cumul_cm_tempsens <- cumul_cm_tempsens |>
      rbind(ck, cumul_tn, cumul_tp, cumul_fn, cumul_fp, total_pos)
    cumul_cm_sens[, i] <- cumul_cm_tempsens[, 1]
  }

  colnames(cumul_cm_sens) <- thresholds

  return(cumul_cm_sens)
}

###Threshold line plot function
threshold_plot <- function(df, model, int_line) {
  cumulplot_df <- df |> t() |> data.frame()
  colnames(cumulplot_df) <- gsub(".", " ", colnames(cumulplot_df), fixed = TRUE)

  cumulplot_df <- cumulplot_df |>
    mutate(Threshold = as.numeric(threshold_list)) |>
    pivot_longer(cols = -Threshold, names_to = "Metric", values_to = "Count") |>
    filter(
      Metric %in%
        c(
          "True positives",
          "False positives",
          "True negatives",
          "False negatives",
          "Total flagged"
        )
    ) |>
    mutate(
      Metric = factor(
        Metric,
        levels = c(
          "Total flagged",
          "True positives",
          "False positives",
          "True negatives",
          "False negatives"
        )
      )
    )

  ###Line plot of flagged positives, true positives, false positives, true negatives and false negatives
  cumulplot <- ggplot(
    cumulplot_df,
    aes(x = Threshold, y = Count, group = Metric, color = Metric)
  ) +
    geom_line() +
    labs(
      x = "Probability threshold for flagging prescription as inappropriate",
      y = "Number of discharge prescriptions",
      title = glue(
        "Agreement of {model} model with clinicians across probability\nthresholds for 150 discharge antibiotic prescriptions"
      )
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    #change color palette
    scale_color_manual(
      values = c(
        "Total flagged" = "#71009e",
        "True positives" = "#009e12",
        "False positives" = "#dfa208",
        "True negatives" = "#56B4E9",
        "False negatives" = "#d50000"
      )
    ) +
    #grey dotted horizontal line at 30
    geom_hline(yintercept = int_line, linetype = "dashed", color = "grey")

  print(cumulplot)

  #save plot
  ggsave(
    glue("questionnaire_{model}_threshold_plot.png"),
    plot = cumulplot,
    width = 10,
    height = 6,
    dpi = 300
  )
  ggsave(
    glue("questionnaire_{model}_threshold_plot.pdf"),
    plot = cumulplot,
    width = 10,
    height = 6,
    dpi = 300
  )
}

###Write performance metrics and confidence intervals together
qu_perf_vec <- function(df) {
  df_vec <- c(
    glue("{round(df[1], 2)}({round(df[2], 2)}-{round(df[3], 2)})"),
    glue("{round(df[4], 2)}({round(df[5], 2)}-{round(df[6], 2)})"),
    glue("{round(df[7], 2)}({round(df[8], 2)}-{round(df[9], 2)})"),
    glue("{round(df[10], 2)}({round(df[11], 2)}-{round(df[12], 2)})"),
    glue("{round(df[13], 2)}({round(df[14], 2)}-{round(df[15], 2)})"),
    glue("{round(df[16], 2)}({round(df[17], 2)}-{round(df[18], 2)})"),
    glue("{round(df[19], 2)}({round(df[20], 2)}-{round(df[21], 2)})"),
    glue("{round(df[22], 2)}({round(df[23], 2)}-{round(df[24], 2)})"),
    glue("{round(df[25], 2)}({round(df[26], 2)}-{round(df[27], 2)})"),
    glue("{round(df[28], 2)}({round(df[29], 2)}-{round(df[30], 2)})"),
    glue("{round(df[31], 2)}({round(df[32], 2)}-{round(df[33], 2)})")
  )

  df_vec
}

##Read-in

bert_preds <- read_csv("bert_preds.csv")
ac_bert_preds <- read_csv("access_bert_preds.csv")

##Questionnaire performance metrics

###Tabulate from questionnaire result files
cumul_rx <- data.frame(matrix(nrow = 0, ncol = 7))
cumul_ac_rx <- data.frame(matrix(nrow = 0, ncol = 7))

for (i in 1:6) {
  r_x <- read_csv(glue("r_{i}.csv")) |>
    rename(text = "question") |>
    mutate(answer = case_when(answer == "Yes" ~ 1, answer == "No" ~ 0))
  df_x <- read_csv(glue("df_{i}.csv")) |>
    mutate(bert_inapp = case_when(pred == 0 ~ 1, pred == 1 ~ 0))
  df_r_x <- r_x |> left_join(df_x, by = "text")
  ac_r_x <- read_csv(glue("ac_r_{i}.csv")) |>
    rename(text = "question") |>
    mutate(answer = case_when(answer == "Yes" ~ 1, answer == "No" ~ 0))
  ac_df_x <- read_csv(glue("ac_df_{i}.csv")) |>
    mutate(bert_inapp = case_when(pred == 1 ~ 1, pred == 0 ~ 0))
  ac_df_r_x <- ac_r_x |> left_join(ac_df_x, by = "text")
  cumul_rx <- cumul_rx %>% rbind(df_r_x) %>% tibble()
  cumul_ac_rx <- cumul_ac_rx %>% rbind(ac_df_r_x) %>% tibble()
}

##Backstop to ensure predictions match final validation dataset
bert_preds_probs <- bert_preds |>
  select(text, prob) |>
  mutate(prob = as.numeric(prob))
ac_bert_preds_probs <- ac_bert_preds |>
  select(text, prob) |>
  mutate(prob = as.numeric(prob))

cumul_rx <- cumul_rx |>
  select(-c(prob, pred, bert_inapp)) |>
  left_join(bert_preds_probs, by = "text")
cumul_ac_rx <- cumul_ac_rx |>
  select(-c(prob, pred, bert_inapp)) |>
  left_join(ac_bert_preds_probs, by = "text")

cumul_rx <- cumul_rx |>
  mutate(pred = ifelse(prob >= 0.5, 1, 0)) |>
  mutate(bert_inapp = case_when(pred == 0 ~ 1, pred == 1 ~ 0))

cumul_ac_rx <- cumul_ac_rx |>
  mutate(pred = ifelse(prob >= 0.5, 1, 0)) |>
  mutate(bert_inapp = case_when(pred == 1 ~ 1, pred == 0 ~ 0))

###Quick check of performance metrics (overall model)
cumul_rx <- cumul_rx |>
  mutate(bert_inapp = as.factor(bert_inapp), answer = as.factor(answer))
cumul_cm <- confusionMatrix(
  cumul_rx$bert_inapp,
  cumul_rx$answer,
  positive = "1"
)
cohen.kappa(as.matrix(cumul_rx[, c("bert_inapp", "answer")]))
print(cumul_cm)

###Quick check of performance metrics (Access model)
cumul_ac_rx <- cumul_ac_rx |>
  mutate(bert_inapp = as.factor(bert_inapp), answer = as.factor(answer))
cumul_ac_cm <- confusionMatrix(
  cumul_ac_rx$bert_inapp,
  cumul_ac_rx$answer,
  positive = "1"
)
cohen.kappa(as.matrix(cumul_ac_rx[, c("bert_inapp", "answer")]))
print(cumul_ac_cm)

##Threshold analysis

###Tabulate
threshold_list <- seq(0, 1, by = 0.05)
cumul_df <- decision_iterator(cumul_rx, cumul_cm, threshold_list, "overall")
cumul_ac_df <- decision_iterator(
  cumul_ac_rx,
  cumul_ac_cm,
  threshold_list,
  "Access"
)

###Line plots
threshold_plot(cumul_df, "overall", 30)
threshold_plot(cumul_ac_df, "Access", 30)

###Save source data
cumul_df |>
  mutate(Metric = rownames(cumul_df)) |>
  relocate(Metric, .before = 1) |>
  write_csv("sourcedata_questionnaire_threshold_df.csv")
cumul_ac_df |>
  mutate(Metric = rownames(cumul_ac_df)) |>
  relocate(Metric, .before = 1) |>
  write_csv("sourcedata_questionnaire_ac_threshold_df.csv")

##Full performance metric table

###Bootstrapping for confidence intervals
cumul_ci_df <- data.frame(matrix(nrow = 1000, ncol = 11))
colnames(cumul_ci_df) <- names(cumul_cm$byClass)
ac_cumul_ci_df <- data.frame(matrix(nrow = 1000, ncol = 11))
colnames(ac_cumul_ci_df) <- names(cumul_ac_cm$byClass)

for (i in 1:1000) {
  samp_cumul_rx <- cumul_rx[
    sample(nrow(cumul_rx), size = nrow(cumul_rx), replace = TRUE),
  ]

  cumul_cm_samp <- confusionMatrix(
    samp_cumul_rx$bert_inapp,
    samp_cumul_rx$answer,
    positive = "1"
  )

  cumul_ci_df[i, ] <- cumul_cm_samp$byClass

  samp_cumul_ac_rx <- cumul_ac_rx[
    sample(nrow(cumul_ac_rx), size = nrow(cumul_ac_rx), replace = TRUE),
  ]

  cumul_ac_cm_samp <- confusionMatrix(
    samp_cumul_ac_rx$bert_inapp,
    samp_cumul_ac_rx$answer,
    positive = "1"
  )

  ac_cumul_ci_df[i, ] <- cumul_ac_cm_samp$byClass
}

sens_df <- cumul_ci_df |>
  summarise(across(
    everything(),
    list(
      mean = mean,
      lower = ~ quantile(., 0.025),
      upper = ~ quantile(., 0.975)
    )
  ))

ac_sens_df <- ac_cumul_ci_df |>
  summarise(across(
    everything(),
    list(
      mean = mean,
      lower = ~ quantile(., 0.025),
      upper = ~ quantile(., 0.975)
    )
  ))

sensdf_vec <- sens_df |> qu_perf_vec()
ac_sensdf_vec <- ac_sens_df |> qu_perf_vec()

full_qu_perf <- data.frame(
  `Metric` = cumul_cm$byClass |> names(),
  `Overall Model (95% CI%)` = sensdf_vec,
  `Access Model (95% CI%)` = ac_sensdf_vec
) |>
  tibble()

colnames(full_qu_perf) <- c(
  "Metric",
  "Overall Model (95% CI%)",
  "Access Model (95% CI%)"
)

ov_cohen <- cohen.kappa(as.matrix(cumul_rx[, c("bert_inapp", "answer")]))
ac_cohen <- cohen.kappa(as.matrix(cumul_ac_rx[, c("bert_inapp", "answer")]))

kappa_cis <- c(
  "Kappa",
  glue(
    "{round(ov_cohen$kappa, 2)}({round(ov_cohen$confid[1], 2)}-{round(ov_cohen$confid[1,3], 2)})"
  ),
  glue(
    "{round(ac_cohen$kappa, 2)}({round(ac_cohen$confid[1], 2)}-{round(ac_cohen$confid[1,3], 2)})"
  )
)

acc_cis <- c(
  "Accuracy",
  glue(
    "{round(cumul_cm$overall[1], 2)}({round(cumul_cm$overall[3], 2)}-{round(cumul_cm$overall[4], 2)})"
  ),
  glue(
    "{round(cumul_ac_cm$overall[1], 2)}({round(cumul_ac_cm$overall[3], 2)}-{round(cumul_ac_cm$overall[4], 2)})"
  )
)

full_qu_perf <- full_qu_perf |> rbind(kappa_cis, acc_cis)

write_csv(full_qu_perf, "full_qu_perf.csv")

##Record time taken to run the script

end_time <- Sys.time()
time_taken <- end_time - start_time
time_taken <- as.numeric(time_taken, units = "secs")
time_df1 <- data.frame(matrix(ncol = 2, nrow = 1))
time_df1[1, ] <- c("lang_questionnaire.R", time_taken)
colnames(time_df1) <- c("Script", "Time (secs)")
time_df <- read_csv("script_times.csv")
time_df <- rbind(time_df, time_df1)
write_csv(time_df, "script_times.csv")
