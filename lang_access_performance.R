#MODEL PERFORMANCE (ACCESS PREDICTION)

##Script timer

start_time <- Sys.time()

##Functions

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
calibmaker <- function(actc, predp, outc) {
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

###Stratified df sampling
posdf_sampler <- function(df, n_sample, seedy) {
  set.seed(seedy)

  sample_counts <- df %>%
    count(pred) %>%
    mutate(n_sample_group = round(n / sum(n) * n_sample))

  df <- df %>%
    group_by(pred) %>%
    group_modify(
      ~ slice_sample(
        .x,
        n = sample_counts$n_sample_group[sample_counts$pred == .y$pred]
      )
    ) %>%
    ungroup()

  df[sample(nrow(df)), ]
}

###Question maker
questionmaker_2 <- function(df) {
  df %>%
    select(text) %>%
    rename("question" = text) %>%
    mutate(
      choice1 = "Yes",
      choice2 = "No"
    )
}

##Read-in
perf_df <- read_csv("access_bert_preds.csv")
perf_ci2 <- read_csv("performance_metrics.csv")

##Performance curves

###ROC
d_roc <- roc_maker(
  perf_df %>% select(label),
  perf_df %>% select(prob),
  "Access model",
  "dischargeab_roc"
)

ggsave(
  filename = "accessab_roc.png",
  plot = d_roc,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

###Calibration curve
d_calib <- calibmaker(
  perf_df %>% select(label),
  perf_df %>% select(prob),
  "Access model"
)

ggsave(
  filename = "accessab_calib.png",
  plot = d_calib,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300
)

###Precision-recall curve
prc <- pr.curve(
  scores.class0 = perf_df %>% filter(label == 1) %>% select(prob) %>% unlist(),
  scores.class1 = perf_df %>% filter(label == 0) %>% select(prob) %>% unlist(),
  curve = TRUE
)

auprc <- prc$auc.integral

png("accessab_pr.png", width = 6, height = 6, units = "in", res = 300)

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
    "Access model PR curve\n(AUC = ",
    round(auprc, 3),
    ")"
  )
)

dev.off()

##Other performance characteristics

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
  TP / (TP + FP),
  TP / (TP + FN),
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
    `Access antibiotic model (95% CI)` = glue(
      "{round(value,3)}({round(lower,3)}-{round(upper,3)})"
    )
  ) %>%
  select(-c(lower, upper, value)) %>%
  tibble()

perf_cis <- perf_ci2 %>% left_join(perf_cis, by = "Metric")

write_csv(perf_cis, "access_performance_metrics.csv")

##Write discharge summary questions CSV

pos_df <- perf_df %>% filter(label == 0)

sampleno <- 25

###1
df_1 <- pos_df %>% posdf_sampler(sampleno, 1)
write_csv(
  df_1,
  "ac_df_1.csv"
)
q_1 <- df_1 %>% questionmaker_2()
write_csv(
  q_1,
  "ac_q_1.csv"
)

###2
df_2 <- pos_df %>% posdf_sampler(sampleno, 2)
write_csv(
  df_2,
  "ac_df_2.csv"
)
q_2 <- df_2 %>% questionmaker_2()
write_csv(
  q_2,
  "ac_q_2.csv"
)

###3
df_3 <- pos_df %>% posdf_sampler(sampleno, 3)
write_csv(
  df_3,
  "ac_df_3.csv"
)
q_3 <- df_3 %>% questionmaker_2()
write_csv(
  q_3,
  "ac_q_3.csv"
)

###4
df_4 <- pos_df %>% posdf_sampler(sampleno, 4)
write_csv(
  df_4,
  "ac_df_4.csv"
)
q_4 <- df_4 %>% questionmaker_2()
write_csv(
  q_4,
  "ac_q_4.csv"
)

###5
df_5 <- pos_df %>% posdf_sampler(sampleno, 5)
write_csv(
  df_5,
  "ac_df_5.csv"
)
q_5 <- df_5 %>% questionmaker_2()
write_csv(
  q_5,
  "ac_q_5.csv"
)

###6
df_6 <- pos_df %>% posdf_sampler(sampleno, 6)
write_csv(
  df_6,
  "ac_df_6.csv"
)
q_6 <- df_6 %>% questionmaker_2()
write_csv(
  q_6,
  "ac_q_6.csv"
)

##Record time taken to run the script
end_time <- Sys.time()
time_taken <- end_time - start_time
time_taken <- as.numeric(time_taken, units = "secs")
time_df1 <- data.frame(matrix(ncol = 2, nrow = 1))
time_df1[1, ] <- c("lang_access_performance.R", time_taken)
colnames(time_df1) <- c("Script", "Time (secs)")
time_df <- read_csv("script_times.csv")
time_df <- rbind(time_df, time_df1)
write_csv(time_df, "script_times.csv")
