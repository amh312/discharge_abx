#BERT SENSITIVITY ANALYSES

##Set seed

set.seed(123)

##Script timer

start_time <- Sys.time()

##Read-in
discharge2 <- read_csv("discharge_interim.csv")
chartab <- read_csv("characteristics_table.csv")

##Functions

###Calibration slope only
calslope <- function(actc, predp) {
  #predicted probs and actual class into dataframe
  ur_calib_df <- data.frame(
    pred_probs = predp %>% unlist(),
    act_probs = as.numeric(as.character(actc %>% unlist()))
  ) %>%

    #put predicted probs into 10 bins (trycatch fallback if throws error)
    mutate(
      probs_bin = tryCatch(
        cut(
          pred_probs,
          breaks = unique(quantile(
            pred_probs,
            probs = seq(0.1, 1, by = 0.1),
            na.rm = TRUE
          )),
          labels = FALSE,
          include.lowest = TRUE
        ),
        error = function(e) {
          cut(pred_probs, breaks = 5, labels = FALSE, include.lowest = TRUE)
        }
      )
    ) %>%

    #get means and n samples
    group_by(probs_bin) %>%
    summarise(
      meanpp = mean(pred_probs),
      act_prop = mean(act_probs),
      nsamp = n()
    ) %>%
    ungroup()

  #slope from linear model
  urcalib_model <- lm(ur_calib_df$act_prop ~ ur_calib_df$meanpp)
  coef(urcalib_model)[2]
}

###Fairness test
fairness_test <- function(df, pred_col, label_col, prob_col, n_boot) {
  pred_col <- enquo(pred_col)
  label_col <- enquo(label_col)
  prob_col <- enquo(prob_col)

  perfmets_big <- data.frame(matrix(nrow = 0, ncol = 6))

  colnames(perfmets_big) <- c(
    "lower",
    "upper",
    "value",
    "Characteristic",
    "Group",
    "Metric"
  )

  for (j in 1:length(chars)) {
    categs <- df %>%
      distinct(!!sym(chars[j])) %>%
      arrange(!!sym(chars[j])) %>%
      unlist()

    for (i in 1:length(categs)) {
      print(chars[j])
      print(categs[i])
      this_group <- categs[i]

      d_filtered <- df %>% filter(!!sym(chars[j]) == categs[i])

      perfmets <- data.frame(matrix(nrow = 1000, ncol = 10))

      colnames(perfmets) <- c(
        "Precision",
        "Recall",
        "F1",
        "Specificity",
        "NPV",
        "PPR",
        "Accuracy",
        "AUROC",
        "AUPRC",
        "Calibration"
      )

      for (i in 1:n_boot) {
        samp_perfs <- d_filtered[
          sample(nrow(d_filtered), size = nrow(d_filtered), replace = TRUE),
        ]

        TP <- nrow(samp_perfs %>% filter(!!pred_col == 1 & !!label_col == 1))
        TN <- nrow(samp_perfs %>% filter(!!pred_col == 0 & !!label_col == 0))
        FP <- nrow(samp_perfs %>% filter(!!pred_col == 1 & !!label_col == 0))
        FN <- nrow(samp_perfs %>% filter(!!pred_col == 0 & !!label_col == 1))

        thisroc <- roc(
          samp_perfs %>% select(!!label_col) %>% unlist(),
          samp_perfs %>% select(!!prob_col) %>% unlist(),
          levels = c(0, 1)
        )
        thisprc <- pr.curve(
          scores.class0 = samp_perfs %>%
            filter(!!label_col == 1) %>%
            select(!!prob_col) %>%
            unlist(),
          scores.class1 = samp_perfs %>%
            filter(!!label_col == 0) %>%
            select(!!prob_col) %>%
            unlist(),
          curve = TRUE
        )

        perfmets$Precision[i] <- TP / (TP + FP)
        perfmets$Recall[i] <- TP / (TP + FN)
        perfmets$F1[i] <- ifelse(
          perfmets$Precision[i] == Inf | perfmets$Recall[i] == Inf,
          NA,
          2 *
            ((perfmets$Precision[i] * perfmets$Recall[i]) /
              (perfmets$Precision[i] + perfmets$Recall[i]))
        )
        perfmets$Specificity[i] <- TN / (TN + FP)
        perfmets$NPV[i] <- TN / (TN + FN)
        perfmets$PPR[i] <- ppr <- (TP + FP) / (TP + TN + FP + FN)
        perfmets$Accuracy[i] <- (TP + TN) / (TP + TN + FP + FN)
        perfmets$AUROC[i] <- as.numeric(auc(thisroc))
        perfmets$AUPRC[i] <- thisprc$auc.integral
        perfmets$Calibration[i] <- calslope(
          samp_perfs %>% select(!!label_col),
          samp_perfs %>% select(!!prob_col)
        )
      }

      perf_cis <- t(apply(perfmets, 2, function(x) {
        quantile(x, probs = c(0.025, 0.975), na.rm = T)
      }))

      colnames(perf_cis) <- c("lower", "upper")

      perf_cis <- perf_cis %>% as.data.frame()

      TP <- nrow(d_filtered %>% filter(!!pred_col == 1 & !!label_col == 1))
      TN <- nrow(d_filtered %>% filter(!!pred_col == 0 & !!label_col == 0))
      FP <- nrow(d_filtered %>% filter(!!pred_col == 1 & !!label_col == 0))
      FN <- nrow(d_filtered %>% filter(!!pred_col == 0 & !!label_col == 1))

      thisroc <- roc(
        d_filtered %>% select(!!label_col) %>% unlist(),
        d_filtered %>% select(!!prob_col) %>% unlist(),
        levels = c(0, 1)
      )
      thisprc <- pr.curve(
        scores.class0 = d_filtered %>%
          filter(!!label_col == 1) %>%
          select(!!prob_col) %>%
          unlist(),
        scores.class1 = d_filtered %>%
          filter(!!label_col == 0) %>%
          select(!!prob_col) %>%
          unlist(),
        curve = TRUE
      )
      precision <- TP / (TP + FP)
      recall <- TP / (TP + FN)

      perf_vec <- c(
        precision,
        recall,
        ifelse(
          precision == Inf | recall == Inf,
          NA,
          2 * ((precision * recall) / (precision + recall))
        ),
        TN / (TN + FP),
        TN / (TN + FN),
        (TP + FP) / (TP + TN + FP + FN),
        (TP + TN) / (TP + TN + FP + FN),
        as.numeric(auc(thisroc)),
        thisprc$auc.integral,
        calslope(
          samp_perfs %>%
            select(!!label_col),
          samp_perfs %>%
            select(!!prob_col)
        )
      )

      perf_cis$value <- perf_vec
      perf_cis$Characteristic <- chars[j]
      perf_cis$Group <- this_group
      perf_cis$Metric <- rownames(perf_cis)
      perf_cis <- perf_cis %>% tibble()

      perfmets_big <- perfmets_big %>% rbind(perf_cis)
    }
  }

  perfmets_big
}

###Plotting fairness results
fairness_plotter <- function(df, perfm, model) {
  prec_perfmets <- df %>%
    filter(Metric == perfm) %>%
    mutate(Group = glue("{Characteristic}: {Group}"))

  prec_perfmets
  vec1 <- prec_perfmets %>%
    mutate(Group = as.character(Group)) %>%
    select(Group) %>%
    arrange(desc(Group)) %>%
    unlist()
  vec2 <- vec1[1:18]
  vec3 <- vec1[20:26]
  vec4 <- vec1[19]
  vec1 <- c(vec2, vec3, vec4)

  prec_perfmets$Group <- factor(prec_perfmets$Group, levels = vec1)

  fairplot <- ggplot(
    prec_perfmets,
    aes(x = Group, y = value, col = Characteristic)
  ) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
    coord_flip() +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    ggtitle(glue("{model} model {perfm} in different patient groups")) +
    ylim(min(prec_perfmets$lower - 0.1), max(prec_perfmets$upper + 0.1))

  ggsave(
    filename = glue("{model}_{perfm}_fairness.png"),
    plot = fairplot,
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
  )
  ggsave(
    filename = glue("{model}_{perfm}_fairness.pdf"),
    plot = fairplot,
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
  )
}

###Plotting fairness threshold analysis
threshold_fairplot <- function(df, prot_group, model) {
  disc_plot_df <- df |>
    filter(
      !is.na(value) &
        Group == prot_group &
        Metric != "AUROC" &
        Metric != "AUPRC" &
        Metric != "Calibration"
    )
  charac <- disc_plot_df |> pull(Characteristic)

  disc_plot_df_0.5 <- disc_plot_df |> filter(Threshold == 0.5)

  fairplot1 <- ggplot(
    disc_plot_df,
    aes(x = Metric, y = value, col = Threshold)
  ) +
    geom_point() +
    geom_point(
      data = disc_plot_df_0.5,
      aes(x = Metric, y = value),
      col = "red",
      shape = "square",
      size = 2
    ) +
    coord_flip() +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    ggtitle(glue(
      "{model} model performance in '{charac}'='{prot_group}' group\nacross decision thresholds"
    )) +
    ylim(0, 1)

  ggsave(
    filename = glue("{charac}_{model}_{prot_group}_fairness_threshold.png"),
    plot = fairplot1,
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
  )
  ggsave(
    filename = glue("{charac}_{model}_{prot_group}_fairness_threshold.pdf"),
    plot = fairplot1,
    width = 8,
    height = 5,
    units = "in",
    dpi = 300
  )
}

###AUROC value and ROC curve
roc_maker <- function(actclass, predpr, outc, aurocnam) {
  #get roc curve
  urroc <- roc(actclass %>% unlist(), predpr %>% unlist(), levels = c(0, 1))

  #get roc curve confidence intervals
  urroc_ci <- ci.se(
    urroc,
    specificities = seq(0, 1, by = 0.01),
    boot.n = 1000,
    conf.level = 0.95
  )

  #get auroc
  ur_auroc_value <- auc(urroc)

  #update message
  print(glue("Validation AUROC for {outc} = {round(ur_auroc_value,2)}"))

  #assign to global env
  assign(aurocnam, ur_auroc_value, envir = .GlobalEnv)

  #write sourcedata to csv
  roc_df <- data.frame(
    fpr = 1 - as.numeric(rownames(urroc_ci)),
    tpr = urroc_ci[, 2],
    lower = urroc_ci[, 1],
    upper = urroc_ci[, 3]
  )
  write_csv(roc_df, glue("sourcedata_{aurocnam}.csv"))

  #plot roc curve
  ggroc(urroc, color = "blue3", legacy.axes = TRUE) +

    #titles
    ggtitle(glue("{outc} ROC curve")) +
    labs(x = "False positive rate", y = "True positive rate") +

    #zero effect line
    geom_segment(
      aes(x = 0, y = 0, xend = 1, yend = 1),
      color = "grey",
      linetype = "dashed"
    ) +

    #shaded confidence intervals
    geom_ribbon(
      data = data.frame(
        fpr = 1 - as.numeric(rownames(urroc_ci)),
        lower = urroc_ci[, 1],
        upper = urroc_ci[, 3]
      ),
      aes(x = fpr, ymin = lower, ymax = upper),
      fill = "blue3",
      alpha = 0.2
    ) +

    #auc annotation
    geom_rect(
      aes(xmin = 0.6, xmax = 0.9, ymin = 0.1, ymax = 0.2),
      fill = "white",
      alpha = 0.2,
      color = "grey9"
    ) +
    annotate(
      "text",
      x = 0.75,
      y = 0.15,
      label = glue("AUC: {round(ur_auroc_value,2)}"),
      size = 10,
      color = "grey9"
    ) +

    #theme, text and ticks
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 30),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20)
    )
}

###Calibration curve and slope value
calibmaker <- function(actc, predp, outc, analysis) {
  #predicted probs and actual class into dataframe
  ur_calib_df <- data.frame(
    pred_probs = predp %>% unlist(),
    act_probs = as.numeric(as.character(actc %>% unlist()))
  ) %>%

    #put predicted probs into 10 bins
    mutate(
      probs_bin = cut(
        pred_probs,
        breaks = quantile(pred_probs, probs = seq(0.1, 1, by = 0.1), na.rm = T),
        labels = F
      )
    ) %>%

    #get means and n samples
    group_by(probs_bin) %>%
    summarise(
      meanpp = mean(pred_probs),
      act_prop = mean(act_probs),
      nsamp = n()
    ) %>%
    ungroup()

  #loess smoothed values for actual probabilities
  loesspreds <- predict(
    loess(ur_calib_df$act_prop ~ ur_calib_df$meanpp),
    span = 1,
    se = T
  )
  ur_calib_df$sm_act <- loesspreds$fit

  #smoothing 95% confidence intervals
  ur_calib_df$upperci <- loesspreds$fit + 1.96 * loesspreds$se.fit
  ur_calib_df$lowerci <- loesspreds$fit - 1.96 * loesspreds$se.fit

  #actual means 95% confidence intervals
  ur_calib_df$grupci <- ur_calib_df$act_prop +
    (sqrt(
      (ur_calib_df$act_prop * 1 - ur_calib_df$act_prop) / ur_calib_df$nsamp
    ) *
      1.96)
  ur_calib_df$grloci <- ur_calib_df$act_prop -
    ((sqrt(ur_calib_df$act_prop * 1 - ur_calib_df$act_prop) /
      ur_calib_df$nsamp) *
      1.96)

  #slope from linear model
  urcalib_model <- lm(ur_calib_df$act_prop ~ ur_calib_df$meanpp)
  ur_calslope <- coef(urcalib_model)[2]

  #values for annotation box
  xrange <- range(ur_calib_df$meanpp)
  xrangesize <- xrange[2] - xrange[1]
  yrange <- range(ur_calib_df$act_prop)
  yrangesize <- yrange[2] - yrange[1]
  max_xbox <- xrange[1] + xrangesize * 0.65
  min_xbox <- xrange[1] + xrangesize * 0.95
  x_text <- mean(c(min_xbox, max_xbox))
  min_y <- yrange[1] + yrangesize * 0.01
  max_y <- yrange[1] + yrangesize * 0.12
  y_text <- mean(c(min_y, max_y))

  #save sourcedata
  write_csv(ur_calib_df, glue("sourcedata_{analysis}_calibration_curve.csv"))

  #calibration plot
  ggplot(ur_calib_df, aes(x = meanpp, y = sm_act)) +

    #loess smoothed line
    geom_line(color = "#00BFC4", linetype = "solid") +

    #ideal line
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "grey20"
    ) +

    #loess confidence intervals
    geom_ribbon(
      data = ur_calib_df,
      aes(x = meanpp, ymin = lowerci, ymax = upperci),
      fill = "#00BFC4",
      alpha = 0.2
    ) +

    #actual means
    geom_point(
      data = ur_calib_df,
      aes(x = meanpp, y = act_prop),
      col = "#F8766D",
      size = 3
    ) +

    #confidence intervals of means
    geom_errorbar(
      data = ur_calib_df,
      aes(ymin = grloci, ymax = grupci),
      col = "#F8766D",
      width = 0
    ) +

    #theme and titles
    theme_minimal() +
    labs(
      x = "Mean predicted probability",
      y = "Actual proportion of positives",
      title = glue("{outc}\ncalibration curve")
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 30),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20)
    ) +

    #x and y limits
    ylim(min(ur_calib_df$lowerci), max(ur_calib_df$upperci)) +
    xlim(min(ur_calib_df$meanpp), max(ur_calib_df$meanpp)) +

    #annotation with slope
    geom_rect(
      aes(xmin = min_xbox, xmax = max_xbox, ymin = min_y, ymax = max_y),
      fill = "white",
      alpha = 0.2,
      color = "grey9"
    ) +
    annotate(
      "text",
      x = x_text,
      y = y_text,
      label = glue("Slope: {round(ur_calslope,2)}"),
      size = 10,
      color = "grey9"
    )
}

###Classification report
ur_perf_mets <- function(df, indexrows, bootstr = T) {
  #if bootstrapping
  if (bootstr == T) {
    #subset by selected indices
    ur_act <- df$act_val[indexrows]
    ur_probs <- df$pred_probs[indexrows]
    ur_class <- df$pred_class[indexrows]

    #confusion matrix
    ur_confmat <- confusionMatrix(factor(ur_class), factor(ur_act))

    #accuracy
    acc <- ur_confmat$overall['Accuracy']

    #precision
    prec <- ur_confmat$byClass['Precision']

    #recall
    rec <- ur_confmat$byClass['Recall']

    #f1 score
    f1 <- 2 * (prec * rec) / (prec + rec)

    #auroc
    auroc <- auc(roc(ur_act, ur_probs, levels = c(0, 1)))

    #return vector
    c(auroc = auroc, precision = prec, recall = rec, accuracy = acc, f1 = f1)

    #if not bootstrapping
  } else {
    #use whole vectors
    ur_act <- df$act_val
    ur_probs <- df$pred_probs
    ur_class <- df$pred_class

    #confusion matrix
    ur_confmat <- confusionMatrix(factor(ur_class), factor(ur_act))

    #accuracy
    acc <- ur_confmat$overall['Accuracy']

    #precision
    prec <- ur_confmat$byClass['Precision']

    #recall
    rec <- ur_confmat$byClass['Recall']

    #f1 score
    f1 <- 2 * (prec * rec) / (prec + rec)

    #auroc
    auroc <- auc(roc(ur_act, ur_probs, levels = c(0, 1)))

    #return list
    list(
      AUC = auroc,
      Accuracy = acc,
      Precision = prec,
      Recall = rec,
      F1_Score = f1
    )
  }
}

###Calibration slope only
calslope <- function(actc, predp) {
  #predicted probs and actual class into dataframe
  ur_calib_df <- data.frame(
    pred_probs = predp %>% unlist(),
    act_probs = as.numeric(as.character(actc %>% unlist()))
  ) %>%

    #put predicted probs into 10 bins
    mutate(
      probs_bin = cut(
        pred_probs,
        breaks = quantile(pred_probs, probs = seq(0.1, 1, by = 0.1), na.rm = T),
        labels = F
      )
    ) %>%

    #get means and n samples
    group_by(probs_bin) %>%
    summarise(
      meanpp = mean(pred_probs),
      act_prop = mean(act_probs),
      nsamp = n()
    ) %>%
    ungroup()

  #loess smoothed values for actual probabilities
  loesspreds <- predict(
    loess(ur_calib_df$act_prop ~ ur_calib_df$meanpp),
    span = 1,
    se = T
  )
  ur_calib_df$sm_act <- loesspreds$fit

  #smoothing 95% confidence intervals
  ur_calib_df$upperci <- loesspreds$fit + 1.96 * loesspreds$se.fit
  ur_calib_df$lowerci <- loesspreds$fit - 1.96 * loesspreds$se.fit

  #actual means 95% confidence intervals
  ur_calib_df$grupci <- ur_calib_df$act_prop +
    (sqrt(
      (ur_calib_df$act_prop * 1 - ur_calib_df$act_prop) / ur_calib_df$nsamp
    ) *
      1.96)
  ur_calib_df$grloci <- ur_calib_df$act_prop -
    ((sqrt(ur_calib_df$act_prop * 1 - ur_calib_df$act_prop) /
      ur_calib_df$nsamp) *
      1.96)

  #slope from linear model
  urcalib_model <- lm(ur_calib_df$act_prop ~ ur_calib_df$meanpp)
  coef(urcalib_model)[2]
}

##Fairness analysis

###Filter to test datasets
disc_perfs <- discharge2 %>%
  filter(!is.na(pred)) %>%
  select(pred:`Age group`) %>%
  select(-c(ac_pred, ac_prob, ac_label, `Discharge location`))
acc_perfs <- discharge2 %>%
  filter(!is.na(ac_pred)) %>%
  select(ac_pred:`Age group`) %>%
  select(-`Discharge location`)

###Fairness tables
chars <- chartab %>%
  distinct(Characteristic) %>%
  filter(Characteristic != "··") %>%
  mutate(Characteristic = str_remove_all(Characteristic, "\\*")) %>%
  dplyr::slice(1:6) %>%
  unlist()

fairness_perfmets <- disc_perfs %>% fairness_test(pred, label, prob, 1000)
ac_fairness_perfmets <- acc_perfs %>%
  fairness_test(ac_pred, ac_label, ac_prob, 1000)

write_csv(fairness_perfmets, "sourcedata_fairness_perfmets.csv")
write_csv(ac_fairness_perfmets, "sourcedata_ac_fairness_perfmets.csv")

###Fairness plots

metric_list <- fairness_perfmets %>% distinct(Metric) %>% unlist()

for (i in seq_along(metric_list)) {
  fairness_perfmets %>% fairness_plotter(metric_list[i], "Overall")
  ac_fairness_perfmets %>% fairness_plotter(metric_list[i], "Access")
}

##Fairness decision threshold adjustment
thresholds <- seq(0, 1, by = 0.1)
disc_perfs_cum <- data.frame(matrix(nrow = 0, ncol = 7))
disc_perfs_cum_ac <- data.frame(matrix(nrow = 0, ncol = 7))
cum_coln <- c(
  "lower",
  "upper",
  "value",
  "Characteristic",
  "Group",
  "Metric",
  "Threshold"
)
colnames(disc_perfs_cum) <- cum_coln
colnames(disc_perfs_cum_ac) <- cum_coln

for (i in seq_along(thresholds)) {
  disc_perfs_adj <- disc_perfs |>
    mutate(
      pred = case_when(prob >= thresholds[i] ~ 1, prob < thresholds[i] ~ 0)
    ) %>%
    fairness_test(pred, label, prob, 1) |>
    mutate(Threshold = thresholds[i])

  disc_perfs_cum <- disc_perfs_cum |> rbind(disc_perfs_adj)

  disc_perfs_adj_ac <- acc_perfs |>
    mutate(
      ac_pred = case_when(
        ac_prob >= thresholds[i] ~ 1,
        ac_prob < thresholds[i] ~ 0
      )
    ) %>%
    fairness_test(ac_pred, ac_label, ac_prob, 1) |>
    mutate(Threshold = thresholds[i])

  disc_perfs_cum_ac <- disc_perfs_cum_ac |> rbind(disc_perfs_adj_ac)
}

write_csv(disc_perfs_cum, "sourcedata_fairness_perfmets_threshold.csv")
write_csv(disc_perfs_cum_ac, "sourcedata_fairness_perfmets_threshold_ac.csv")

###Plot fairness threshold analysis
duplicate_groups <- c("Marital status", "Race")

disc_perfs_cum_1 <- disc_perfs_cum |>
  filter(!(Characteristic %in% duplicate_groups))
disc_perfs_cum_ac_1 <- disc_perfs_cum_ac |>
  filter(!(Characteristic %in% duplicate_groups))
grouplist_1 <- disc_perfs_cum_1 %>% pull(Group) |> unique()
disc_perfs_cum_2 <- disc_perfs_cum |>
  filter((Characteristic %in% duplicate_groups))
disc_perfs_cum_ac_2 <- disc_perfs_cum_ac |>
  filter((Characteristic %in% duplicate_groups))
grouplist_2 <- disc_perfs_cum_2 %>% pull(Group) |> unique()

for (i in seq_along(grouplist_1)) {
  threshold_fairplot(disc_perfs_cum_1, grouplist_1[i], "Overall")
  threshold_fairplot(disc_perfs_cum_ac_1, grouplist_1[i], "Access")
}

for (i in seq_along(grouplist_2)) {
  threshold_fairplot(disc_perfs_cum_2, grouplist_2[i], "Overall")
  threshold_fairplot(disc_perfs_cum_ac_2, grouplist_2[i], "Access")
}

##Stability analysis

###STABILITY Read-in
perf_df <- read_csv("bert_preds_stab.csv")

###STABILITY ROC
d_roc <- roc_maker(
  perf_df %>% select(label),
  perf_df %>% select(prob),
  "Discharge antibiotic stability",
  "stab_dischargeab_roc"
)

ggsave(
  filename = "dischargeab_roc_stab.png",
  plot = d_roc,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)
ggsave(
  filename = "dischargeab_roc_stab.pdf",
  plot = d_roc,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

###STABILITY Calibration curve
d_calib <- calibmaker(
  perf_df %>% select(label),
  perf_df %>% select(prob),
  "Discharge antibiotic stability",
  "stab_dischargeab"
)

ggsave(
  filename = "dischargeab_calib_stab.png",
  plot = d_calib,
  width = 10,
  height = 10,
  dpi = 300
)
ggsave(
  filename = "dischargeab_calib_stab.pdf",
  plot = d_calib,
  width = 10,
  height = 10,
  dpi = 300
)

###STABILITY Precision-recall curve
prc <- pr.curve(
  scores.class0 = perf_df %>% filter(label == 1) %>% select(prob) %>% unlist(),
  scores.class1 = perf_df %>% filter(label == 0) %>% select(prob) %>% unlist(),
  curve = TRUE
)

auprc <- prc$auc.integral

png("dischargeab_pr_stab.png", width = 6, height = 6, units = "in", res = 300)

plot(
  prc$curve[, 1],
  prc$curve[, 2],
  type = "n",
  xlab = "Recall",
  ylab = "Precision"
)

grid(col = "grey85", lty = 1)

lines(prc$curve[, 1], prc$curve[, 2], col = "blue", lwd = 2)

title(
  main = paste0(
    "Discharge antibiotic stability PR curve\n(AUC = ",
    round(auprc, 2),
    ")"
  )
)

dev.off()

pdf("dischargeab_pr_stab.pdf", width = 6, height = 6)

plot(
  prc$curve[, 1],
  prc$curve[, 2],
  type = "n",
  xlab = "Recall",
  ylab = "Precision"
)

grid(col = "grey85", lty = 1)

lines(prc$curve[, 1], prc$curve[, 2], col = "blue", lwd = 2)

title(
  main = paste0(
    "Discharge antibiotic stability PR curve\n(AUC = ",
    round(auprc, 2),
    ")"
  )
)

dev.off()

prc_df <- data.frame(
  recall = prc$curve[, 1],
  precision = prc$curve[, 2]
)
write_csv(prc_df, "sourcedata_stab_prc.csv")

###STABILITY Other performance characteristics

perfmets <- data.frame(matrix(nrow = 1000, ncol = 10))

colnames(perfmets) <- c(
  "Precision",
  "Recall",
  "F1",
  "Specificity",
  "NPV",
  "PPR",
  "Accuracy",
  "AUROC",
  "AUPRC",
  "Calibration"
)

for (i in 1:1000) {
  samp_perfs <- perf_df[
    sample(nrow(perf_df), size = nrow(perf_df), replace = TRUE),
  ]

  TP <- nrow(samp_perfs %>% filter(pred == 1 & label == 1))
  TN <- nrow(samp_perfs %>% filter(pred == 0 & label == 0))
  FP <- nrow(samp_perfs %>% filter(pred == 1 & label == 0))
  FN <- nrow(samp_perfs %>% filter(pred == 0 & label == 1))

  thisroc <- roc(samp_perfs$label, samp_perfs$prob, levels = c(0, 1))
  thisprc <- pr.curve(
    scores.class0 = samp_perfs %>%
      filter(label == 1) %>%
      select(prob) %>%
      unlist(),
    scores.class1 = samp_perfs %>%
      filter(label == 0) %>%
      select(prob) %>%
      unlist(),
    curve = TRUE
  )

  perfmets$Precision[i] <- TP / (TP + FP)
  perfmets$Recall[i] <- TP / (TP + FN)
  perfmets$F1[i] <- 2 *
    ((perfmets$Precision[i] * perfmets$Recall[i]) /
      (perfmets$Precision[i] + perfmets$Recall[i]))
  perfmets$Specificity[i] <- TN / (TN + FP)
  perfmets$NPV[i] <- TN / (TN + FN)
  perfmets$PPR[i] <- ppr <- (TP + FP) / (TP + TN + FP + FN)
  perfmets$Accuracy[i] <- (TP + TN) / (TP + TN + FP + FN)
  perfmets$AUROC[i] <- as.numeric(auc(thisroc))
  perfmets$AUPRC[i] <- thisprc$auc.integral
  perfmets$Calibration[i] <- calslope(
    samp_perfs %>% select(label),
    samp_perfs %>% select(prob)
  )
}

perf_cis <- t(apply(perfmets, 2, function(x) {
  quantile(x, probs = c(0.025, 0.975), na.rm = T)
}))

colnames(perf_cis) <- c("lower", "upper")

perf_cis <- perf_cis %>% as.data.frame()

TP <- nrow(perf_df %>% filter(pred == 1 & label == 1))
TN <- nrow(perf_df %>% filter(pred == 0 & label == 0))
FP <- nrow(perf_df %>% filter(pred == 1 & label == 0))
FN <- nrow(perf_df %>% filter(pred == 0 & label == 1))

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

thisroc <- roc(perf_df$label, perf_df$prob, levels = c(0, 1))
thisprc <- pr.curve(
  scores.class0 = perf_df %>% filter(label == 1) %>% select(prob) %>% unlist(),
  scores.class1 = perf_df %>% filter(label == 0) %>% select(prob) %>% unlist(),
  curve = TRUE
)

perf_vec <- c(
  precision,
  recall,
  2 * ((precision * recall) / (precision + recall)),
  TN / (TN + FP),
  TN / (TN + FN),
  (TP + FP) / (TP + TN + FP + FN),
  (TP + TN) / (TP + TN + FP + FN),
  as.numeric(auc(thisroc)),
  thisprc$auc.integral,
  calslope(
    samp_perfs %>%
      select(label),
    samp_perfs %>%
      select(prob)
  )
)

perf_cis$value <- perf_vec
perf_cis$Metric <- rownames(perf_cis)
perf_cis <- perf_cis %>%
  mutate(
    `Overall antibiotic model (95% CI)` = glue(
      "{sprintf('%.2f', round(value,2))} ({sprintf('%.2f', round(lower,2))}-{sprintf('%.2f', round(upper,2))})"
    )
  ) %>%
  select(-c(lower, upper, value)) %>%
  tibble()

write_csv(perf_cis, "performance_metrics_stab.csv")

##Time sensitivity analysis

###TIMESENS Read-in
perf_df <- read_csv("bert_preds_timesens.csv")

###TIMESENS ROC
d_roc <- roc_maker(
  perf_df %>% select(label),
  perf_df %>% select(prob),
  "Discharge antibiotic time sensitivity",
  "timesens_dischargeab_roc"
)

ggsave(
  filename = "dischargeab_roc_timesens.png",
  plot = d_roc,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)
ggsave(
  filename = "dischargeab_roc_timesens.pdf",
  plot = d_roc,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

###TIMESENS Calibration curve
d_calib <- calibmaker(
  perf_df %>% select(label),
  perf_df %>% select(prob),
  "Discharge antibiotic time sensitivity",
  "timesens_dischargeab"
)

ggsave(
  filename = "dischargeab_calib_timesens.png",
  plot = d_calib,
  width = 10,
  height = 10,
  dpi = 300
)
ggsave(
  filename = "dischargeab_calib_timesens.pdf",
  plot = d_calib,
  width = 10,
  height = 10,
  dpi = 300
)

###TIMESENS Precision-recall curve
prc <- pr.curve(
  scores.class0 = perf_df %>% filter(label == 1) %>% select(prob) %>% unlist(),
  scores.class1 = perf_df %>% filter(label == 0) %>% select(prob) %>% unlist(),
  curve = TRUE
)

auprc <- prc$auc.integral

png(
  "dischargeab_pr_timesens.png",
  width = 6,
  height = 6,
  units = "in",
  res = 300
)

plot(
  prc$curve[, 1],
  prc$curve[, 2],
  type = "n",
  xlab = "Recall",
  ylab = "Precision"
)

grid(col = "grey85", lty = 1)

lines(prc$curve[, 1], prc$curve[, 2], col = "blue", lwd = 2)

title(
  main = paste0(
    "Discharge antibiotic time sensitivity PR curve\n(AUC = ",
    round(auprc, 2),
    ")"
  )
)

dev.off()

pdf(
  "dischargeab_pr_timesens.pdf",
  width = 6,
  height = 6
)

plot(
  prc$curve[, 1],
  prc$curve[, 2],
  type = "n",
  xlab = "Recall",
  ylab = "Precision"
)

grid(col = "grey85", lty = 1)

lines(prc$curve[, 1], prc$curve[, 2], col = "blue", lwd = 2)

title(
  main = paste0(
    "Discharge antibiotic time sensitivity PR curve\n(AUC = ",
    round(auprc, 2),
    ")"
  )
)

dev.off()

prc_df <- data.frame(
  recall = prc$curve[, 1],
  precision = prc$curve[, 2]
)
write_csv(prc_df, "sourcedata_timesens_prc.csv")

###TIMESENS Other performance characteristics

perfmets <- data.frame(matrix(nrow = 1000, ncol = 10))

colnames(perfmets) <- c(
  "Precision",
  "Recall",
  "F1",
  "Specificity",
  "NPV",
  "PPR",
  "Accuracy",
  "AUROC",
  "AUPRC",
  "Calibration"
)

for (i in 1:1000) {
  samp_perfs <- perf_df[
    sample(nrow(perf_df), size = nrow(perf_df), replace = TRUE),
  ]

  TP <- nrow(samp_perfs %>% filter(pred == 1 & label == 1))
  TN <- nrow(samp_perfs %>% filter(pred == 0 & label == 0))
  FP <- nrow(samp_perfs %>% filter(pred == 1 & label == 0))
  FN <- nrow(samp_perfs %>% filter(pred == 0 & label == 1))

  thisroc <- roc(samp_perfs$label, samp_perfs$prob, levels = c(0, 1))
  thisprc <- pr.curve(
    scores.class0 = samp_perfs %>%
      filter(label == 1) %>%
      select(prob) %>%
      unlist(),
    scores.class1 = samp_perfs %>%
      filter(label == 0) %>%
      select(prob) %>%
      unlist(),
    curve = TRUE
  )

  perfmets$Precision[i] <- TP / (TP + FP)
  perfmets$Recall[i] <- TP / (TP + FN)
  perfmets$F1[i] <- 2 *
    ((perfmets$Precision[i] * perfmets$Recall[i]) /
      (perfmets$Precision[i] + perfmets$Recall[i]))
  perfmets$Specificity[i] <- TN / (TN + FP)
  perfmets$NPV[i] <- TN / (TN + FN)
  perfmets$PPR[i] <- ppr <- (TP + FP) / (TP + TN + FP + FN)
  perfmets$Accuracy[i] <- (TP + TN) / (TP + TN + FP + FN)
  perfmets$AUROC[i] <- as.numeric(auc(thisroc))
  perfmets$AUPRC[i] <- thisprc$auc.integral
  perfmets$Calibration[i] <- calslope(
    samp_perfs %>% select(label),
    samp_perfs %>% select(prob)
  )
}

perf_cis <- t(apply(perfmets, 2, function(x) {
  quantile(x, probs = c(0.025, 0.975), na.rm = T)
}))

colnames(perf_cis) <- c("lower", "upper")

perf_cis <- perf_cis %>% as.data.frame()

TP <- nrow(perf_df %>% filter(pred == 1 & label == 1))
TN <- nrow(perf_df %>% filter(pred == 0 & label == 0))
FP <- nrow(perf_df %>% filter(pred == 1 & label == 0))
FN <- nrow(perf_df %>% filter(pred == 0 & label == 1))

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

thisroc <- roc(perf_df$label, perf_df$prob, levels = c(0, 1))
thisprc <- pr.curve(
  scores.class0 = perf_df %>% filter(label == 1) %>% select(prob) %>% unlist(),
  scores.class1 = perf_df %>% filter(label == 0) %>% select(prob) %>% unlist(),
  curve = TRUE
)

perf_vec <- c(
  precision,
  recall,
  2 * ((precision * recall) / (precision + recall)),
  TN / (TN + FP),
  TN / (TN + FN),
  (TP + FP) / (TP + TN + FP + FN),
  (TP + TN) / (TP + TN + FP + FN),
  as.numeric(auc(thisroc)),
  thisprc$auc.integral,
  calslope(
    samp_perfs %>%
      select(label),
    samp_perfs %>%
      select(prob)
  )
)

perf_cis$value <- perf_vec
perf_cis$Metric <- rownames(perf_cis)
perf_cis <- perf_cis %>%
  mutate(
    `Overall antibiotic model (95% CI)` = glue(
      "{sprintf('%.2f', round(value,2))} ({sprintf('%.2f', round(lower,2))}-{sprintf('%.2f', round(upper,2))})"
    )
  ) %>%
  select(-c(lower, upper, value)) %>%
  tibble()

write_csv(perf_cis, "performance_metrics_timesens.csv")

##Access Stability analysis

###ACCESS STABILITY Read-in
perf_df <- read_csv("bert_preds_stab_ac.csv")

###ACCESS STABILITY ROC
d_roc <- roc_maker(
  perf_df %>% select(label),
  perf_df %>% select(prob),
  "Access antibiotic stability",
  "stab_accessab_roc"
)

ggsave(
  filename = "dischargeab_roc_stab_ac.png",
  plot = d_roc,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)
ggsave(
  filename = "dischargeab_roc_stab_ac.pdf",
  plot = d_roc,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

###ACCESS STABILITY Calibration curve
d_calib <- calibmaker(
  perf_df %>% select(label),
  perf_df %>% select(prob),
  "Access antibiotic stability",
  "stab_accessab"
)

ggsave(
  filename = "dischargeab_calib_stab_ac.png",
  plot = d_calib,
  width = 10,
  height = 10,
  dpi = 300
)
ggsave(
  filename = "dischargeab_calib_stab_ac.pdf",
  plot = d_calib,
  width = 10,
  height = 10,
  dpi = 300
)

###ACCESS STABILITY Precision-recall curve
prc <- pr.curve(
  scores.class0 = perf_df %>% filter(label == 1) %>% select(prob) %>% unlist(),
  scores.class1 = perf_df %>% filter(label == 0) %>% select(prob) %>% unlist(),
  curve = TRUE
)

auprc <- prc$auc.integral

png(
  "dischargeab_pr_stab_ac.png",
  width = 6,
  height = 6,
  units = "in",
  res = 300
)

plot(
  prc$curve[, 1],
  prc$curve[, 2],
  type = "n",
  xlab = "Recall",
  ylab = "Precision"
)

grid(col = "grey85", lty = 1)

lines(prc$curve[, 1], prc$curve[, 2], col = "blue", lwd = 2)

title(
  main = paste0(
    "Access antibiotic stability PR curve\n(AUC = ",
    round(auprc, 2),
    ")"
  )
)

dev.off()

pdf(
  "dischargeab_pr_stab_ac.pdf",
  width = 6,
  height = 6
)

plot(
  prc$curve[, 1],
  prc$curve[, 2],
  type = "n",
  xlab = "Recall",
  ylab = "Precision"
)

grid(col = "grey85", lty = 1)

lines(prc$curve[, 1], prc$curve[, 2], col = "blue", lwd = 2)

title(
  main = paste0(
    "Access antibiotic stability PR curve\n(AUC = ",
    round(auprc, 2),
    ")"
  )
)

dev.off()

prc_df <- data.frame(
  recall = prc$curve[, 1],
  precision = prc$curve[, 2]
)
write_csv(prc_df, "sourcedata_stab_ac_prc.csv")

###ACCESS STABILITY Other performance characteristics

perfmets <- data.frame(matrix(nrow = 1000, ncol = 10))

colnames(perfmets) <- c(
  "Precision",
  "Recall",
  "F1",
  "Specificity",
  "NPV",
  "PPR",
  "Accuracy",
  "AUROC",
  "AUPRC",
  "Calibration"
)

for (i in 1:1000) {
  samp_perfs <- perf_df[
    sample(nrow(perf_df), size = nrow(perf_df), replace = TRUE),
  ]

  TP <- nrow(samp_perfs %>% filter(pred == 1 & label == 1))
  TN <- nrow(samp_perfs %>% filter(pred == 0 & label == 0))
  FP <- nrow(samp_perfs %>% filter(pred == 1 & label == 0))
  FN <- nrow(samp_perfs %>% filter(pred == 0 & label == 1))

  thisroc <- roc(samp_perfs$label, samp_perfs$prob, levels = c(0, 1))
  thisprc <- pr.curve(
    scores.class0 = samp_perfs %>%
      filter(label == 1) %>%
      select(prob) %>%
      unlist(),
    scores.class1 = samp_perfs %>%
      filter(label == 0) %>%
      select(prob) %>%
      unlist(),
    curve = TRUE
  )

  perfmets$Precision[i] <- TP / (TP + FP)
  perfmets$Recall[i] <- TP / (TP + FN)
  perfmets$F1[i] <- 2 *
    ((perfmets$Precision[i] * perfmets$Recall[i]) /
      (perfmets$Precision[i] + perfmets$Recall[i]))
  perfmets$Specificity[i] <- TN / (TN + FP)
  perfmets$NPV[i] <- TN / (TN + FN)
  perfmets$PPR[i] <- ppr <- (TP + FP) / (TP + TN + FP + FN)
  perfmets$Accuracy[i] <- (TP + TN) / (TP + TN + FP + FN)
  perfmets$AUROC[i] <- as.numeric(auc(thisroc))
  perfmets$AUPRC[i] <- thisprc$auc.integral
  perfmets$Calibration[i] <- calslope(
    samp_perfs %>% select(label),
    samp_perfs %>% select(prob)
  )
}

perf_cis <- t(apply(perfmets, 2, function(x) {
  quantile(x, probs = c(0.025, 0.975), na.rm = T)
}))

colnames(perf_cis) <- c("lower", "upper")

perf_cis <- perf_cis %>% as.data.frame()

TP <- nrow(perf_df %>% filter(pred == 1 & label == 1))
TN <- nrow(perf_df %>% filter(pred == 0 & label == 0))
FP <- nrow(perf_df %>% filter(pred == 1 & label == 0))
FN <- nrow(perf_df %>% filter(pred == 0 & label == 1))

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

thisroc <- roc(perf_df$label, perf_df$prob, levels = c(0, 1))
thisprc <- pr.curve(
  scores.class0 = perf_df %>% filter(label == 1) %>% select(prob) %>% unlist(),
  scores.class1 = perf_df %>% filter(label == 0) %>% select(prob) %>% unlist(),
  curve = TRUE
)

perf_vec <- c(
  precision,
  recall,
  2 * ((precision * recall) / (precision + recall)),
  TN / (TN + FP),
  TN / (TN + FN),
  (TP + FP) / (TP + TN + FP + FN),
  (TP + TN) / (TP + TN + FP + FN),
  as.numeric(auc(thisroc)),
  thisprc$auc.integral,
  calslope(
    samp_perfs %>%
      select(label),
    samp_perfs %>%
      select(prob)
  )
)

perf_cis$value <- perf_vec
perf_cis$Metric <- rownames(perf_cis)
perf_cis <- perf_cis %>%
  mutate(
    `Overall antibiotic model (95% CI)` = glue(
      "{sprintf('%.2f', round(value,2))} ({sprintf('%.2f', round(lower,2))}-{sprintf('%.2f', round(upper,2))})"
    )
  ) %>%
  select(-c(lower, upper, value)) %>%
  tibble()

write_csv(perf_cis, "performance_metrics_stab_ac.csv")

perf_cis_stab <- read_csv("performance_metrics_stab.csv")
perf_cis_stab_ac <- read_csv("performance_metrics_stab_ac.csv") |>
  rename(
    `Access antibiotic model (95% CI)` = "Overall antibiotic model (95% CI)"
  ) |>
  select(`Access antibiotic model (95% CI)`)
perf_cis_stab <- perf_cis_stab |> cbind(perf_cis_stab_ac) |> tibble()
write_csv(perf_cis_stab, "performance_metrics_stab_both.csv")

##Time sensitivity analysis

###ACCESS TIMESENS Read-in
perf_df <- read_csv("bert_preds_timesens_ac.csv")

###ACCESS TIMESENS ROC
d_roc <- roc_maker(
  perf_df %>% select(label),
  perf_df %>% select(prob),
  "Access antibiotic time sensitivity",
  "timesens_accessab_roc"
)

ggsave(
  filename = "dischargeab_roc_timesens_ac.png",
  plot = d_roc,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)
ggsave(
  filename = "dischargeab_roc_timesens_ac.pdf",
  plot = d_roc,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

###ACCESS TIMESENS Calibration curve
d_calib <- calibmaker(
  perf_df %>% select(label),
  perf_df %>% select(prob),
  "Access antibiotic time sensitivity",
  "timesens_accessab"
)

ggsave(
  filename = "dischargeab_calib_timesens_ac.png",
  plot = d_calib,
  width = 10,
  height = 10,
  dpi = 300
)
ggsave(
  filename = "dischargeab_calib_timesens_ac.pdf",
  plot = d_calib,
  width = 10,
  height = 10,
  dpi = 300
)

###ACCESS TIMESENS Precision-recall curve
prc <- pr.curve(
  scores.class0 = perf_df %>% filter(label == 1) %>% select(prob) %>% unlist(),
  scores.class1 = perf_df %>% filter(label == 0) %>% select(prob) %>% unlist(),
  curve = TRUE
)

auprc <- prc$auc.integral

png(
  "dischargeab_pr_timesens_ac.png",
  width = 6,
  height = 6,
  units = "in",
  res = 300
)

plot(
  prc$curve[, 1],
  prc$curve[, 2],
  type = "n",
  xlab = "Recall",
  ylab = "Precision"
)

grid(col = "grey85", lty = 1)

lines(prc$curve[, 1], prc$curve[, 2], col = "blue", lwd = 2)

title(
  main = paste0(
    "Access antibiotic time sensitivity PR curve\n(AUC = ",
    round(auprc, 2),
    ")"
  )
)

dev.off()

pdf(
  "dischargeab_pr_timesens_ac.pdf",
  width = 6,
  height = 6
)

plot(
  prc$curve[, 1],
  prc$curve[, 2],
  type = "n",
  xlab = "Recall",
  ylab = "Precision"
)

grid(col = "grey85", lty = 1)

lines(prc$curve[, 1], prc$curve[, 2], col = "blue", lwd = 2)

title(
  main = paste0(
    "Access antibiotic time sensitivity PR curve\n(AUC = ",
    round(auprc, 2),
    ")"
  )
)

dev.off()

prc_df <- data.frame(
  recall = prc$curve[, 1],
  precision = prc$curve[, 2]
)
write_csv(prc_df, "sourcedata_timesens_ac_prc.csv")

###ACCESS TIMESENS Other performance characteristics

perfmets <- data.frame(matrix(nrow = 1000, ncol = 10))

colnames(perfmets) <- c(
  "Precision",
  "Recall",
  "F1",
  "Specificity",
  "NPV",
  "PPR",
  "Accuracy",
  "AUROC",
  "AUPRC",
  "Calibration"
)

for (i in 1:1000) {
  samp_perfs <- perf_df[
    sample(nrow(perf_df), size = nrow(perf_df), replace = TRUE),
  ]

  TP <- nrow(samp_perfs %>% filter(pred == 1 & label == 1))
  TN <- nrow(samp_perfs %>% filter(pred == 0 & label == 0))
  FP <- nrow(samp_perfs %>% filter(pred == 1 & label == 0))
  FN <- nrow(samp_perfs %>% filter(pred == 0 & label == 1))

  thisroc <- roc(samp_perfs$label, samp_perfs$prob, levels = c(0, 1))
  thisprc <- pr.curve(
    scores.class0 = samp_perfs %>%
      filter(label == 1) %>%
      select(prob) %>%
      unlist(),
    scores.class1 = samp_perfs %>%
      filter(label == 0) %>%
      select(prob) %>%
      unlist(),
    curve = TRUE
  )

  perfmets$Precision[i] <- TP / (TP + FP)
  perfmets$Recall[i] <- TP / (TP + FN)
  perfmets$F1[i] <- 2 *
    ((perfmets$Precision[i] * perfmets$Recall[i]) /
      (perfmets$Precision[i] + perfmets$Recall[i]))
  perfmets$Specificity[i] <- TN / (TN + FP)
  perfmets$NPV[i] <- TN / (TN + FN)
  perfmets$PPR[i] <- ppr <- (TP + FP) / (TP + TN + FP + FN)
  perfmets$Accuracy[i] <- (TP + TN) / (TP + TN + FP + FN)
  perfmets$AUROC[i] <- as.numeric(auc(thisroc))
  perfmets$AUPRC[i] <- thisprc$auc.integral
  perfmets$Calibration[i] <- calslope(
    samp_perfs %>% select(label),
    samp_perfs %>% select(prob)
  )
}

perf_cis <- t(apply(perfmets, 2, function(x) {
  quantile(x, probs = c(0.025, 0.975), na.rm = T)
}))

colnames(perf_cis) <- c("lower", "upper")

perf_cis <- perf_cis %>% as.data.frame()

TP <- nrow(perf_df %>% filter(pred == 1 & label == 1))
TN <- nrow(perf_df %>% filter(pred == 0 & label == 0))
FP <- nrow(perf_df %>% filter(pred == 1 & label == 0))
FN <- nrow(perf_df %>% filter(pred == 0 & label == 1))

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

thisroc <- roc(perf_df$label, perf_df$prob, levels = c(0, 1))
thisprc <- pr.curve(
  scores.class0 = perf_df %>% filter(label == 1) %>% select(prob) %>% unlist(),
  scores.class1 = perf_df %>% filter(label == 0) %>% select(prob) %>% unlist(),
  curve = TRUE
)

perf_vec <- c(
  precision,
  recall,
  2 * ((precision * recall) / (precision + recall)),
  TN / (TN + FP),
  TN / (TN + FN),
  (TP + FP) / (TP + TN + FP + FN),
  (TP + TN) / (TP + TN + FP + FN),
  as.numeric(auc(thisroc)),
  thisprc$auc.integral,
  calslope(
    samp_perfs %>%
      select(label),
    samp_perfs %>%
      select(prob)
  )
)

perf_cis$value <- perf_vec
perf_cis$Metric <- rownames(perf_cis)
perf_cis <- perf_cis %>%
  mutate(
    `Overall antibiotic model (95% CI)` = glue(
      "{sprintf('%.2f', round(value,2))} ({sprintf('%.2f', round(lower,2))}-{sprintf('%.2f', round(upper,2))})"
    )
  ) %>%
  select(-c(lower, upper, value)) %>%
  tibble()

write_csv(perf_cis, "performance_metrics_timesens_ac.csv")

perf_cis_timesens <- read_csv("performance_metrics_timesens.csv")
perf_cis_timesens_ac <- read_csv("performance_metrics_timesens_ac.csv") |>
  rename(
    `Access antibiotic model (95% CI)` = "Overall antibiotic model (95% CI)"
  ) |>
  select(`Access antibiotic model (95% CI)`)
perf_cis_timesens <- perf_cis_timesens |>
  cbind(perf_cis_timesens_ac) |>
  tibble()
write_csv(perf_cis_timesens, "performance_metrics_timesens_both.csv")

##Record time taken to run the script

end_time <- Sys.time()
time_taken <- end_time - start_time
time_taken <- as.numeric(time_taken, units = "secs")
time_df1 <- data.frame(matrix(ncol = 2, nrow = 1))
time_df1[1, ] <- c("lang_sensitivity.R", time_taken)
colnames(time_df1) <- c("Script", "Time (secs)")
time_df <- read_csv("script_times.csv")
time_df <- rbind(time_df, time_df1)
write_csv(time_df, "script_times.csv")
