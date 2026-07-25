#DISCHARGE TEXT CLEANING

##Initialise script timer

time_df <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(time_df) <- c("Script", "Time (secs)")
start_time <- Sys.time()

##Functions

###Quick check of the first n rows of a dataframe
eyeball <- function(df, target_col, n_rows) {
  target_col <- enquo(target_col)

  df %>% dplyr::slice(1:n_rows) %>% select(!!target_col) %>% view()
}

###Quick check of antibiotic dataframe to manually curate
ab_eyeball <- function(df, ab_group) {
  df %>%
    filter(group == ab_group) %>%
    select(name, atc_group1, atc_group2) %>%
    view()
}

###Filter down to named antimicrobials within each group
ab_filter <- function(df, ab_group, ablist) {
  df2 <- df %>%
    filter(
      group == ab_group
    )

  df <- df %>%
    filter(
      group != ab_group
    )

  df2 <- df2 %>%
    filter(
      grepl(ablist, name)
    )

  tibble(rbind(df, df2))
}

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

###Remove generic discharge proformas
remove_generics <- function(df, text_col, min_length) {
  text_col <- enquo(text_col)

  generics <- df %>%
    count(!!text_col) %>%
    filter(
      n >= min_length
    ) %>%
    select(!!text_col) %>%
    unlist()

  df %>%
    filter(!(!!text_col %in% generics))
}

###Isolate discharge meds
discharge_meds <- function(df, new_col) {
  new_col <- enquo(new_col)

  df <- df %>%
    mutate(
      !!new_col := case_when(
        !grepl("Discharge Medications:", text) ~ "None",
        TRUE ~ sub(
          ".*Discharge Medications:\n1.(.*?)\nDischarge Diagnosis:.*",
          "\\1",
          text
        )
      )
    )

  df <- df %>%
    mutate(
      !!new_col := case_when(
        grepl("Discharge Medications:", !!new_col) ~ sub(
          ".*Discharge Medications:(.*?)\nDischarge Disposition:.*",
          "\\1",
          !!new_col
        ),
        TRUE ~ !!new_col
      )
    )

  df %>%
    mutate(!!new_col := sub("Discharge Disposition.*$", "", !!new_col))
}

##Read-in

discharge <- read_csv("discharge.csv")
aware <- read_csv("aware_classification.csv")

##Discharge letter filtering and text preprocessing

###Filter to patient discharge instructions
discharge2 <- discharge %>%
  select_text(
    text,
    pt_text,
    "Discharge Instructions:",
    "Followup Instructions:",
    "\n"
  )

###Remove prefix
discharge2 <- discharge2 %>%
  mutate(
    pt_text = str_replace(pt_text, "Discharge Instructions:", "")
  )

###Interim save
write_csv(discharge2, "discharge_interim.csv")

###Remove any repeated proformas
discharge2 <- discharge2 %>%
  remove_generics(
    pt_text,
    2
  )

##Discharge medications filtering and preprocessing

###Isolate discharge meds
discharge2 <- discharge2 %>%
  discharge_meds(disc_meds)

###Load in antimicrobial list from AMR package
abdf <- AMR::antimicrobials

###Manually curate reference df to common antibiotics of interest
abdf <- abdf %>%
  filter(
    !grepl(
      "(tuberculosis|lepra|antimycotic|antifungal|topical)",
      atc_group1,
      ignore.case = T
    )
  ) %>%
  filter(!grepl("rifaximin", name, ignore.case = T)) %>%
  filter(!grepl("(antifungal|mycobac)", group, ignore.case = T)) %>%
  filter(!is.na(group)) %>%
  filter(!is.na(atc_group1))

abdf %>% count(group) %>% arrange(desc(n))

abdf %>% ab_eyeball("Beta-lactams/penicillins")

abdf <- abdf %>%
  ab_filter(
    "Beta-lactams/penicillins",
    "Amoxicillin|Amoxicillin/clavulanic acid|Ampicillin|Ampicillin/sulbactam|Benzylpenicillin|Dicloxacillin|Flucloxacillin|Pivmecillinam|Oxacillin|Phenoxymethylpenicillin|Piperacillin/tazobactam|Temocillin"
  )

abdf %>% count(group) %>% arrange(desc(n))

abdf %>% ab_eyeball("Trimethoprims")

abdf <- abdf %>%
  ab_filter("Trimethoprims", "Trimethoprim|Trimethoprim/sulfamethoxazole")

abdf <- abdf %>%
  mutate(
    name = case_when(
      name == "Trimethoprim/sulfamethoxazole" ~
        "Sulfameth/Trimethoprim",
      TRUE ~ name
    )
  )

abdf %>% count(group) %>% arrange(desc(n))

abdf <- abdf %>%
  ab_filter("Fluoroquinolones", "^Ciprofloxacin$|^Levofloxacin$|Moxifloxacin")

abdf %>% count(group) %>% arrange(desc(n))

abdf %>% ab_eyeball("Cephalosporins (3rd gen.)")

abdf <- abdf %>%
  ab_filter(
    "Cephalosporins (3rd gen.)",
    "Cefixime|Cefnidir|Cefotaxime|Cefpodoxime|^Ceftriaxone$|^Ceftazidime$|Ceftizoxime"
  )

abdf %>% count(group) %>% arrange(desc(n))

abdf %>% ab_eyeball("Other antibacterials")

abdf <- abdf %>%
  ab_filter(
    "Other antibacterials",
    "Daptomycin|Fusidic acid|Metronidazole|Nitrofurantoin"
  )

abdf %>% count(group) %>% arrange(desc(n))

abdf %>% ab_eyeball("Macrolides/lincosamides")

abdf <- abdf %>%
  ab_filter(
    "Macrolides/lincosamides",
    "Azithromycin|Clarithromycin|Clindamycin|Erythromycin|Quinupristin/dalfopristin|Telithromycin"
  )

abdf %>% count(group) %>% arrange(desc(n))

abdf %>% ab_eyeball("Tetracyclines")

abdf <- abdf %>%
  ab_filter(
    "Tetracyclines",
    "Doxycycline|Eravacycline|Lymecycline|Minocycline|Oxytetracycline|Tetracycline|Tigecycline"
  )

abdf %>% count(group) %>% arrange(desc(n))

abdf %>% ab_eyeball("Aminoglycosides")

abdf <- abdf %>%
  ab_filter("Aminoglycosides", "Amikacin|Gentamicin|Tobramycin|Kanamycin")

abdf %>% count(group) %>% arrange(desc(n))

abdf %>% ab_eyeball("Cephalosporins (1st gen.)")

abdf <- abdf %>%
  ab_filter("Cephalosporins (1st gen.)", "Cefalexin|Cefazolin|Cefadroxil")

abdf %>% count(group) %>% arrange(desc(n))

abdf %>% ab_eyeball("Cephalosporins (2nd gen.)")

abdf <- abdf %>%
  ab_filter(
    "Cephalosporins (2nd gen.)",
    "Cefaclor|Cefaclor|Cefprozil|Cefoxitin|Cefotetan"
  )

abdf %>% count(group) %>% arrange(desc(n))

abdf %>% ab_eyeball("Quinolones")

abdf <- abdf %>% filter(group != "Quinolones")

abdf %>% count(group) %>% arrange(desc(n))

abdf %>% ab_eyeball("Carbapenems")

abdf %>% ab_eyeball("Glycopeptides")

abdf %>% ab_eyeball("Cephalosporins (4th gen.)")

abdf <- abdf %>% ab_filter("Cephalosporins (4th gen.)", "^Cefepime$|Cefpirome")

abdf %>% count(group) %>% arrange(desc(n))

abdf %>% ab_eyeball("Cephalosporins (5th gen.)")

abdf %>% ab_eyeball("Oxazolidinones")

abdf %>% ab_eyeball("Phenicols")

abdf <- abdf %>% ab_filter("Phenicols", "Chloramphenicol")

abdf %>% count(group) %>% arrange(desc(n))

abdf %>% ab_eyeball("Polymyxins")

abdf %>% ab_eyeball("Monobactams")

abdf %>% ab_eyeball("Phosphonics")

nrow(abdf)

abdf <- abdf %>% arrange(group, atc_group1, atc_group2, name)

abdf <- abdf %>% mutate(name = case_when(ab == "AMC" ~ "clavulan", TRUE ~ name))

abdf <- abdf %>%
  mutate(name = case_when(ab == "SXT" ~ "sulfameth", TRUE ~ name))

###Convert antimicrobial df to list
abvec <- unlist(abdf$name)
short_abvec <- unlist(abdf$ab)

###Add outcome measure for each antimicrobial agent
for (i in seq_along(abvec)) {
  discharge2 <- discharge2 %>%
    mutate(
      !!sym(short_abvec[i]) := case_when(
        str_detect(disc_meds, regex(abvec[i], ignore_case = T)) ~ 1,
        TRUE ~ 0
      )
    )
}

###Remove AMX-AMC and TMP-SXT duplicates
discharge2 <- discharge2 %>%
  mutate(
    AMX = case_when(AMC == 1 ~ 0, TRUE ~ AMX),
    TMP = case_when(SXT == 1 ~ 0, TRUE ~ TMP)
  )

###Remove antimicrobial columns for which there were 0 prescriptions
for (i in seq_along(short_abvec)) {
  abcount <- sum(discharge2 %>% select(as.character(short_abvec[i])))

  if (abcount == 0) {
    discharge2 <- discharge2 %>% select(-as.character(short_abvec[i]))
  }
}

###Add outcome measure for any antimicrobial on discharge
discharge2 <- discharge2 %>%
  mutate(
    ab_on_disc = if_any(AMK:TMP, ~ .x == 1) * 1
  )

###Add outcome measure for AWaRe class
awarekey <- aware %>%
  select(Antibiotic, Category) %>%
  rename(ab = "Antibiotic") %>%
  mutate(
    ab = as.ab(ab)
  )
abdf <- abdf %>% left_join(awarekey)
abdf <- abdf %>%
  mutate(
    Category = case_when(
      ab == "SXT" ~ "Access",
      TRUE ~ Category
    )
  )
abdf <- abdf %>% distinct(name, .keep_all = T)
abdf <- abdf %>%
  mutate(
    Category = case_when(
      grepl("Cefpod", name) ~ "Watch",
      grepl("Imipen", name) ~ "Watch",
      TRUE ~ Category
    )
  )
abdf %>% view()
write_csv(abdf, "bert_awarelist.csv")

access <- abdf %>% filter(Category == "Access") %>% select(ab) %>% unlist()
watch <- abdf %>% filter(Category == "Watch") %>% select(ab) %>% unlist()
reserve <- abdf %>% filter(Category == "Reserve") %>% select(ab) %>% unlist()

discharge2 <- discharge2 %>%
  mutate(
    Access = if_any(any_of(access), ~ .x == 1) * 1
  ) %>%
  mutate(
    Watch = if_any(any_of(watch), ~ .x == 1) * 1
  ) %>%
  mutate(
    Reserve = if_any(any_of(reserve), ~ .x == 1) * 1
  )

###Interim save
write_csv(discharge2, "discharge_interim.csv")

##Removal of undetected duplicates
discharge2 <- read_csv("discharge_interim.csv")
discharge2 <- discharge2 %>% distinct(pt_text, .keep_all = T)

###Interim save
write_csv(discharge2, "discharge_interim.csv")

##Preprocessing and writing for DistilBERT main analysis and stability analysis

###Antimicrobial on discharge (overall model)
pt_orig <- discharge2 %>% select(pt_text, ab_on_disc)
pt_orig_key <- discharge2 %>% select(subject_id, pt_text)
pt_orig_key2 <- discharge2 |> select(note_id, pt_text)
write_csv(pt_orig, "pt_orig.csv")
write_csv(pt_orig_key, "pt_orig_key.csv")
write_csv(pt_orig_key2, "pt_orig_key2.csv")

###Access Antimicrobial
pt_access <- discharge2 %>% filter(ab_on_disc == 1) %>% select(pt_text, Access)
pt_access_key <- discharge2 %>%
  filter(ab_on_disc == 1) %>%
  select(subject_id, pt_text)
write_csv(pt_access, "pt_access.csv")
write_csv(pt_access_key, "pt_access_key.csv")

###Watch Antimicrobial
pt_watch <- discharge2 %>% filter(ab_on_disc == 1) %>% select(pt_text, Watch)
pt_watch_key <- discharge2 %>%
  filter(ab_on_disc == 1) %>%
  select(subject_id, pt_text)
write_csv(pt_watch, "pt_watch.csv")
write_csv(pt_watch_key, "pt_watch_key.csv")

###Reserve Antimicrobial
pt_reserve <- discharge2 %>%
  filter(ab_on_disc == 1) %>%
  select(pt_text, Reserve)
pt_reserve_key <- discharge2 %>%
  filter(ab_on_disc == 1) %>%
  select(subject_id, pt_text)
write_csv(pt_reserve, "pt_reserve.csv")
write_csv(pt_reserve_key, "pt_reserve_key.csv")

###Access only (Access model)
pt_access_only <- discharge2 %>%
  mutate(
    access_only = case_when(
      Access == 1 & Watch == 0 & Reserve == 0 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(ab_on_disc == 1) %>%
  select(pt_text, access_only)
pt_access_only_key <- discharge2 %>%
  mutate(
    access_only = case_when(
      Access == 1 & Watch == 0 & Reserve == 0 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(ab_on_disc == 1) %>%
  select(subject_id, pt_text)
write_csv(pt_access_only, "pt_access_only.csv")
write_csv(pt_access_only_key, "pt_access_only_key.csv")

##Stability analysis data

###Seed for random sampling
set.seed(123)

###Overall model
sampling_index <- createDataPartition(
  pt_orig$ab_on_disc,
  p = 0.02,
  list = FALSE
)
pt_stab <- pt_orig[sampling_index, ]
pt_stab_key <- pt_orig_key[sampling_index, ]
write_csv(pt_stab, "stab_orig.csv")
write_csv(pt_stab_key, "stab_orig_key.csv")

###Access model
ac_index <- createDataPartition(
  pt_access_only$access_only,
  p = 0.07,
  list = FALSE
)
ac_stab <- pt_access_only[ac_index, ]
ac_stab_key <- pt_access_only_key[ac_index, ]
ac_stab <- ac_stab |> rename(Access = "access_only")
write_csv(ac_stab, "stab_access.csv")
write_csv(ac_stab_key, "stab_access_key.csv")

##Record time taken to run the script

end_time <- Sys.time()
time_taken <- end_time - start_time
time_taken <- as.numeric(time_taken, units = "secs")
time_df[1, ] <- c("lang_disc_cleaning.R", time_taken)
write_csv(time_df, "script_times.csv")
