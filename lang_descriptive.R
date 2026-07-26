#DESCRIPTIVE DATA

##Script timer

start_time <- Sys.time()

##Functions

###Isolate text of interest
select_text <- function(
  df,
  target_col,
  newcol_name,
  start_trim,
  end_trim,
  rem_string
) {
  target_col <- enquo(target_col)
  newcol_name <- enquo(newcol_name)

  df <- df %>%
    filter(grepl(start_trim, !!target_col)) %>%
    mutate(
      !!newcol_name := sub(glue(".*?({start_trim}.*)"), "\\1", !!target_col)
    )

  df <- df %>%
    mutate(!!newcol_name := str_replace(!!newcol_name, end_trim, ""))

  df %>%
    mutate(!!newcol_name := str_replace_all(!!newcol_name, rem_string, ""))
}

###Compile characteristics table
char_table <- function(df, col_name, prop) {
  char_count <- function(df, charac, patient = TRUE) {
    if (patient) {
      df %>%
        distinct(subject_id, .keep_all = T) %>%
        count(!!sym(charac)) %>%
        arrange(!!sym(charac)) %>%
        mutate(Characteristic = charac) %>%
        rename(no = "n") %>%
        relocate(Characteristic, .before = charac) %>%
        rename(Group = charac) %>%
        mutate(`n(%)` = glue("{no}({round((no/sum(no)*100),1)})")) %>%
        select(-no)
    } else {
      df %>%
        count(!!sym(charac)) %>%
        arrange(desc(!!sym(charac))) %>%
        mutate(Characteristic = glue("{charac}*")) %>%
        rename(no = "n") %>%
        relocate(Characteristic, .before = charac) %>%
        rename(Group = charac) %>%
        mutate(`n(%)` = glue("{no}({round((no/sum(no)*100),1)})")) %>%
        select(-no)
    }
  }

  df <- df %>%
    mutate(
      ab_on_disc = case_when(ab_on_disc == 0 ~ "No", TRUE ~ "Yes"),
      ac_only = case_when(
        Access == 1 & Watch == 0 & Reserve == 0 ~ "Yes",
        TRUE ~ "No"
      ),
      Access = case_when(Access == 0 ~ "No", TRUE ~ "Yes"),
      Watch = case_when(Watch == 0 ~ "No", TRUE ~ "Yes"),
      Reserve = case_when(Reserve == 0 ~ "No", TRUE ~ "Yes")
    ) %>%
    rename(
      `Discharged on ≥1 antibiotic` = "ab_on_disc",
      `Discharged only on Access antibiotic(s)` = "ac_only",
      `Discharged on ≥1 Access antibiotic` = "Access",
      `Discharged on ≥1 Watch antibiotic` = "Watch",
      `Discharged on ≥1 Reserve antibiotic` = "Reserve"
    )

  df %>%
    char_count("Gender") %>%
    rbind(
      char_count(df, "Age group"),
      char_count(df, "Race"),
      char_count(df, "English spoken"),
      char_count(df, "Marital status"),
      char_count(df, "Insurance", F),
      char_count(df, "Admission period", F),
      char_count(df, "Discharge location", F),
      char_count(df, "Discharged on ≥1 antibiotic", F),
      char_count(df, "Discharged on ≥1 Access antibiotic", F),
      char_count(df, "Discharged on ≥1 Watch antibiotic", F),
      char_count(df, "Discharged on ≥1 Reserve antibiotic", F),
      char_count(df, "Discharged only on Access antibiotic(s)", F),
      data.frame(
        Characteristic = "Total",
        Group = "Patients",
        this = glue("{nrow(df %>% distinct(subject_id,.keep_all=T))}({prop})")
      ) %>%
        rename(
          `n(%)` = "this"
        ),
      data.frame(
        Characteristic = "",
        Group = "Discharges",
        this = glue("{nrow(df)}({prop})")
      ) %>%
        rename(
          `n(%)` = "this"
        )
    ) %>%
    rename(!!sym(col_name) := "n(%)")
}

###Align numbers of time sensitivity and stability analyses
df_aligner <- function(df, ref_df, propor) {
  df %>% slice_sample(n = round(nrow(ref_df) * propor), replace = FALSE)
}

###Isolate discharge meds
discharge_meds <- function(df, new_col) {
  new_col <- enquo(new_col)

  df <- df %>%
    mutate(
      !!new_col := case_when(
        !grepl("Discharge Medications:", text) ~ "None",
        TRUE ~ sub(
          "(?s).*Discharge Medications:(.*?)\\n(Discharge Diagnosis|Discharge Disposition|Extended Care|Discharge Condition|Followup).*",
          "\\1",
          text,
          perl = TRUE
        )
      )
    )

  df %>%
    mutate(
      !!new_col := case_when(
        grepl("Discharge Medications:", !!new_col) ~ sub(
          "(?s).*Discharge Medications:(.*?)$",
          "\\1",
          !!new_col,
          perl = TRUE
        ),
        TRUE ~ !!new_col
      )
    )
}

##Read-in
discharge2 <- read_csv("discharge_interim.csv")
bert_aware <- read_csv("bert_awarelist.csv")
perf_df <- read_csv("bert_preds.csv")
acperf_df <- read_csv("access_bert_preds.csv")
hadm <- read_csv("admissions.csv")
pt <- read_csv("patients.csv")
pt_orig <- read_csv("pt_orig.csv")
pt_orig_key <- read_csv("pt_orig_key.csv")
pt_orig_key2 <- read_csv("pt_orig_key2.csv")
pt_access_only <- read_csv("pt_access_only.csv")
stab_orig <- read_csv("stab_orig.csv")
stab_access <- read_csv("stab_access.csv")
train_ref <- read_csv("train_ref.csv") |> rename(pt_text = "text")
ac_train_ref <- read_csv("ac_train_ref.csv") |> rename(pt_text = "text")
train_removed <- read_csv("train_removed.csv") |> rename(pt_text = "text")
ac_train_removed <- read_csv("ac_train_removed.csv") |> rename(pt_text = "text")

ab_counts <- discharge2 |>
  anti_join(train_removed, by = "pt_text") |>
  summarise(across(AMK:TMP, ~ sum(.x == 1, na.rm = TRUE)))
ab_names <- ab_name(colnames(ab_counts))
ab_counts <- ab_counts %>%
  t() %>%
  as.data.frame() %>%
  tibble() %>%
  rename(n = 1)

ab_counts$Antimicrobial <- ab_names

awarekey <- bert_aware %>%
  select(name, Category) %>%
  rename(Antimicrobial = "name", AWaRe = "Category") %>%
  mutate(
    Antimicrobial = case_when(
      grepl("Sulfa", Antimicrobial) ~ "Trimethoprim/sulfamethoxazole",
      grepl("clavu", Antimicrobial) ~ "Amoxicillin/clavulanic acid",
      TRUE ~ Antimicrobial
    )
  )
ab_counts <- ab_counts %>% left_join(awarekey)
ab_counts <- ab_counts %>%
  mutate(
    AWaRe = case_when(
      grepl("clav", Antimicrobial) ~ "Access",
      grepl("sulfa", Antimicrobial) ~ "Access",
      TRUE ~ AWaRe
    )
  )

ab_counts <- ab_counts |>
  mutate(`%` = round((n / sum(n)) * 100, 1))

ab_counts |> arrange(desc(`%`))

ab_counts$Antimicrobial <- factor(
  ab_counts$Antimicrobial,
  levels = ab_counts %>%
    arrange(n) %>%
    select(Antimicrobial) %>%
    unlist()
)

discabs_plot <- ggplot(
  ab_counts,
  aes(x = Antimicrobial, y = `%`, fill = AWaRe)
) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "Access" = "darkgreen",
      "Watch" = "darkorange",
      "Reserve" = "darkred"
    )
  ) +
  coord_flip() +
  ggtitle("Antibiotics that patients were discharged on") +
  ylab("Percentage of discharge antibiotics") +
  xlab("Antibiotic") +
  theme_minimal()

ggsave(
  filename = "dischargeab_plot.png",
  plot = discabs_plot,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

ggsave(
  filename = "dischargeab_plot.pdf",
  plot = discabs_plot,
  width = 8,
  height = 8,
  units = "in",
  dpi = 300
)

write_csv(ab_counts, "sourcedata_ab_counts.csv")

##Population characteristics

###Identify train and test datasets
perf_df <- perf_df %>% rename(pt_text = "text")
acperf_df <- acperf_df %>%
  rename(
    pt_text = "text",
    ac_pred = "pred",
    ac_prob = "prob",
    ac_label = "label"
  )
discharge2 <- discharge2 %>%
  left_join(perf_df, by = "pt_text") %>%
  left_join(acperf_df, by = "pt_text")

###Update discharge_interim
write_csv(discharge2, "discharge_interim.csv")

###Joining characteristics
hadm_key <- hadm %>%
  select(
    hadm_id,
    discharge_location,
    insurance,
    language,
    marital_status,
    race
  ) %>%
  rename(
    `Discharge location` = "discharge_location",
    Insurance = "insurance",
    `English spoken` = "language",
    `Marital status` = "marital_status",
    Race = "race"
  ) %>%
  mutate(
    Race = case_when(
      grepl("WHITE", Race) ~ "White",
      grepl("BLACK", Race) ~ "Black",
      grepl("HISPANIC", Race) ~ "Hispanic",
      grepl("ASIAN", Race) ~ "Asian",
      TRUE ~ "Other"
    )
  ) %>%
  mutate(
    `English spoken` = case_when(
      grepl("ENGLISH", `English spoken`) ~ "Yes",
      TRUE ~ "Unknown"
    )
  ) %>%
  mutate(`Marital status` = str_to_title(tolower(`Marital status`))) %>%
  mutate(
    `Marital status` = case_when(
      is.na(`Marital status`) ~ "Unknown",
      TRUE ~ `Marital status`
    )
  ) %>%
  mutate(`Discharge location` = str_to_title(tolower(`Discharge location`))) %>%
  mutate(
    `Discharge location` = case_when(
      is.na(`Discharge location`) ~ "Unknown",
      TRUE ~ `Discharge location`
    )
  )


pt_key <- pt %>%
  select(subject_id, gender, anchor_age, anchor_year_group) %>%
  mutate(
    anchor_age = glue("{floor(anchor_age/10)*10}-{(floor(anchor_age/10)*10)+9}")
  ) %>%
  mutate(
    anchor_age = case_when(
      grepl("19", anchor_age) ~ "≤19",
      grepl("90", anchor_age) ~ "≥90",
      TRUE ~ anchor_age
    )
  ) %>%
  rename(
    Gender = "gender",
    `Age group` = "anchor_age",
    `Admission period` = "anchor_year_group"
  )

discharge2 <- discharge2 %>%
  left_join(hadm_key, by = "hadm_id") %>%
  left_join(pt_key, by = "subject_id")

###Update discharge_interim
write_csv(discharge2, "discharge_interim.csv")

###Characteristic counts
chartab <- char_table(
  discharge2 %>%
    anti_join(train_removed, by = "pt_text") |>
    filter(is.na(label)),
  "Overall prediction training n(%)",
  "100"
) %>%
  left_join(
    char_table(
      discharge2 %>% filter(!is.na(label)),
      "Overall prediction testing n(%)",
      "100"
    ),
    by = c("Characteristic", "Group")
  ) %>%
  left_join(
    char_table(
      discharge2 %>%
        filter(ab_on_disc == 1) %>%
        anti_join(ac_train_removed, by = "pt_text") %>%
        filter(is.na(ac_label)),
      "Access prediction training n(%)",
      "100"
    ),
    by = c("Characteristic", "Group")
  ) %>%
  left_join(
    char_table(
      discharge2 %>% filter(ab_on_disc == 1) %>% filter(!is.na(ac_label)),
      "Access prediction testing n(%)",
      "100"
    ),
    by = c("Characteristic", "Group")
  ) %>%
  mutate(
    Characteristic = case_when(
      lag(Characteristic) == Characteristic ~ "··",
      TRUE ~ Characteristic
    )
  )

chartab[is.na(chartab)] <- "0(0)"
chartab <- chartab |>
  mutate(
    Characteristic = case_when(
      Characteristic == "" ~ "··",
      TRUE ~ Characteristic
    )
  )

write_csv(chartab, "characteristics_table.csv")

##Time sensitivity analysis data

key_2010 <- discharge2 %>%
  filter(grepl("2010", `Admission period`)) %>%
  select(pt_text)
key_2019 <- discharge2 %>%
  filter(grepl("2019", `Admission period`)) %>%
  select(pt_text)

pt_2010 <- pt_orig %>%
  semi_join(
    key_2010,
    by = "pt_text"
  )
pt_2019 <- pt_orig %>%
  semi_join(
    key_2019,
    by = "pt_text"
  )

ac_2010 <- pt_access_only %>%
  semi_join(
    key_2010,
    by = "pt_text"
  )
ac_2019 <- pt_access_only %>%
  semi_join(
    key_2019,
    by = "pt_text"
  )

datekeymaker <- function(ref_df, df) {
  ref_df %>%
    semi_join(
      df,
      by = "pt_text"
    )
}
pt_2010_key <- pt_orig_key |> datekeymaker(pt_2010)
pt_2019_key <- pt_orig_key |> datekeymaker(pt_2019)
ac_2010_key <- pt_orig_key |> datekeymaker(ac_2010)
ac_2019_key <- pt_orig_key |> datekeymaker(ac_2019)
pt_timekey <- pt_2010_key |> rbind(pt_2019_key) |> tibble()
ac_timekey <- ac_2010_key |> rbind(ac_2019_key) |> tibble()

ac_2010 <- ac_2010 |> rename(Access = "access_only")
ac_2019 <- ac_2019 |> rename(Access = "access_only")

write_csv(pt_2010, "pt_2010.csv")
write_csv(pt_2019, "pt_2019.csv")
write_csv(ac_2010, "ac_2010.csv")
write_csv(ac_2019, "ac_2019.csv")
write_csv(pt_2010_key, "pt_2010_key.csv")
write_csv(pt_2019_key, "pt_2019_key.csv")
write_csv(ac_2010_key, "ac_2010_key.csv")
write_csv(ac_2019_key, "ac_2019_key.csv")
write_csv(pt_timekey, "pt_timekey.csv")
write_csv(ac_timekey, "ac_timekey.csv")

##Record time taken to run the script

end_time <- Sys.time()
time_taken <- end_time - start_time
time_taken <- as.numeric(time_taken, units = "secs")
time_df1 <- data.frame(matrix(ncol = 2, nrow = 1))
time_df1[1, ] <- c("lang_descriptive.R", time_taken)
colnames(time_df1) <- c("Script", "Time (secs)")
time_df <- read_csv("script_times.csv")
time_df <- rbind(time_df, time_df1)
write_csv(time_df, "script_times.csv")
