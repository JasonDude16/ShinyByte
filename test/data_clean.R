library(dplyr)
library(forcats)

# set paths
coinc_path <- "~/Desktop/coincidence-manuscript/data/coincidence/"
dems_path <- "~/Desktop/coincidence-manuscript/data/BASE_redcap_report.csv"
cog_path <- "~/Desktop/coincidence-manuscript/data/MIR_z-scores_final.xlsx"
csf_path <- "~/Desktop/coincidence-manuscript/data/BASE_CSF.csv"
ss_path <- "~/Desktop/coincidence-manuscript/data/BASE_sleepstats_hypno_artrej.csv"

# main coincidence dataset
coinc_files <- list.files(coinc_path, full.names = T, pattern = ".csv")
df_coinc <- purrr::map_dfr(coinc_files, readr::read_csv, show_col_types = F)

# demographics, cognitive measures, csf, sleep stats
df_dems <- readr::read_csv(dems_path, show_col_types = F)
df_cog <- suppressMessages(readxl::read_excel(cog_path))
df_csf <- read.csv(csf_path)
df_ss <- read.csv(ss_path)

# rename for merging
df_dems <- df_dems %>% mutate(id = as.factor(paste0("BASE_", study_id)))
df_cog <- df_cog %>% mutate(id = as.factor(paste0("BASE_", ID)))
df_csf <- df_csf %>% mutate(id = as.factor(paste0("BASE_", subj))) %>% select(id, Status)
df_ss <- df_ss %>% mutate(id = as.factor(subj)) %>% select(-subj)

# merge with csf
df_coinc <- left_join(df_coinc, df_csf)

# relabel and drop any sleep stages that have values other than the ones below 
df_coinc_relab <- df_coinc %>% 
  mutate(
    sleep_stage = case_when(
      sleep_stage == 10 ~ "Wake",
      sleep_stage == 1 ~ "NREM1",
      sleep_stage == 2 ~ "NREM2",
      sleep_stage == 3 ~ "NREM3",
      sleep_stage == 5 ~ "REM"
    )
  ) %>% 
  filter(!is.na(sleep_stage))

# find first sleep stage, relabel stages/artifact
df_coinc_relab <- df_coinc_relab %>% 
  group_by(id) %>% 
  mutate(
    index = 1:n(),
    first_sleep_index = first(which(sleep_stage != "Wake")),
    last_sleep_index = last(which(sleep_stage != "Wake")),
    sleep_stage = case_when(
      sleep_stage == "Wake" & index < first_sleep_index[1] ~ "Wake_pre_sleep",
      sleep_stage == "Wake" & between(index, first_sleep_index[1], last_sleep_index[1]) ~ "Wake_during_sleep",
      sleep_stage == "Wake" & index > last_sleep_index[1] ~ "Wake_post_sleep",
      TRUE ~ sleep_stage
    ),
    artifact = case_when(
      is.na(artifact) & stringr::str_detect(sleep_stage, "Wake") ~ "not clean wake",
      artifact == "W" & stringr::str_detect(sleep_stage, "Wake") ~ "clean wake",
      artifact == "A" ~ "artifact",
      is.na(artifact) ~ "not artifact"
    )
  ) %>% 
  ungroup()

# remove artifact, sleep transition epochs
df_coinc_relab <- df_coinc_relab %>% 
  filter(
    sleep_transition == 0,
    !is.na(artifact),
    artifact != "artifact"
  )

# convert to long format to make analyses easier 
df_coinc_epoch <- df_coinc_relab %>% 
  select(
    id,
    Status,
    epoch,
    artifact,
    sleep_stage,
    first_sleep_index,
    last_sleep_index,
    starts_with("coinc_prop")
  ) %>%
  tidyr::pivot_longer(
    cols = colnames(select(., starts_with("coinc_prop"))),
    names_to = "chan",
    names_prefix = "coinc_prop_",
    values_to = "coinc"
  )

# remove missing coincidence and coincidence == 0 (considered artifact)
df_coinc_epoch <- df_coinc_epoch %>% 
  filter(
    !is.na(coinc),
    coinc != 0
  )

# clean chan and convert vars to factor
ss_order <- c(
  "Wake_pre_sleep",
  "Wake_during_sleep",
  "Wake_post_sleep",
  "NREM1",
  "NREM2",
  "NREM3",
  "REM"
)
df_coinc_epoch <- df_coinc_epoch %>% 
  mutate(
    id = as.factor(id),
    artifact = as.factor(artifact),
    sleep_stage = factor(sleep_stage, levels = ss_order),
    chan = as.factor(stringr::str_remove_all(chan, "_M[1,2]{1}")),
    Status = as.factor(Status)
  )

# aggregate to subj x sleep stage x chan for subject level analyses
df_coinc_agg <- df_coinc_epoch %>% 
  filter(sleep_stage %in% c("Wake_pre_sleep", "Wake_post_sleep") == F) %>% 
  group_by(id, sleep_stage, chan) %>% 
  summarise(
    chan = chan[1],
    n_epochs = n(),
    coinc = mean(coinc, na.rm = TRUE)
  ) %>% 
  ungroup()

# need to aggregate wake portion separately, then merge
df_wake_presleep_clean <- df_coinc_epoch %>% 
  filter(
    sleep_stage == "Wake_pre_sleep",
    artifact == "clean wake"
  ) %>%
  group_by(id, chan) %>% 
  summarise(
    n_epochs = n(), 
    sleep_stage = sleep_stage[1],
    coinc = mean(coinc, na.rm = T)
  ) %>% 
  filter(n_epochs > 3)

df_coinc_agg <- rbind(df_coinc_agg, df_wake_presleep_clean)

df_coinc_agg <- df_coinc_agg %>% 
  mutate(sleep_stage = droplevels(sleep_stage)) %>% 
  arrange(id, sleep_stage)

# merge with other data 
# left join because we want all subjs to have a coincidence value 
df_coinc_stage_chan <- purrr::reduce(list(df_coinc_agg, df_dems, df_cog, df_csf, df_ss), left_join, by = "id")

# let's reduce the size of the dataset by selecting variables of interest 
df_coinc_stage_chan <- df_coinc_stage_chan %>% 
  select(
    id, 
    Status,
    cdr_global,
    sleep_stage,
    chan, 
    n_epochs, 
    coinc,
    age,
    sex,
    edu,
    psg_ahi3,
    psg_ahi4,
    MOCATOTS_ZSCORE,
    CRAFTDVR_ZSCORE,
    CRAFTVRS_ZSCORE,
    TRAILA_ZSCORE,
    TRAILB_ZSCORE,
    wpv1_test1_score,
    wpv1_test2_score,
    TIB,
    WASO,
    TST,
    SOL,
    SE,
    SFI
  )

# now we'll clean this dataset
df_coinc_stage_chan <- df_coinc_stage_chan %>% 
  mutate(
    id = droplevels(id),
    Status = as.factor(Status),
    cdr_global = as.factor(cdr_global),
    chan = fct_relevel(chan, c("F3_F4", "C3_C4", "O1_O2")),
    sex = as.factor(case_when(sex == 1 ~ "Male", sex == 2 ~ "Female")),
    wpv1_test2_score = suppressWarnings(as.numeric(wpv1_test2_score)),
    MOCATOTS_ZSCORE = as.numeric(MOCATOTS_ZSCORE),
    CRAFTDVR_ZSCORE = as.numeric(CRAFTDVR_ZSCORE),
    CRAFTVRS_ZSCORE = as.numeric(CRAFTVRS_ZSCORE),
    TRAILA_ZSCORE = as.numeric(TRAILA_ZSCORE),
    TRAILB_ZSCORE = as.numeric(TRAILB_ZSCORE),
    Group = case_when(
      cdr_global == 0 & Status == 0 ~ "CDR0_AD-",
      cdr_global == 0 & Status == 1 ~ "CDR0_AD+",
      cdr_global == 0 & is.na(Status) ~ "CDR0_NA",
      cdr_global == 0.5 & Status == 0 ~ "CDR0.5_AD-",
      cdr_global == 0.5 & Status == 1 ~ "CDR0.5_AD+",
      cdr_global == 0.5 & is.na(Status) ~ "CDR0.5_NA",
      cdr_global == 1 & Status == 0 ~ "CDR1_AD-",
      cdr_global == 1 & Status == 1 ~ "CDR1_AD+",
      cdr_global == 1 & is.na(Status) ~ "CDR1_NA"
    ),
    Group = as.factor(Group)
  )

# subset to data of interest
df_coinc_stage_chan_sub <- df_coinc_stage_chan %>% 
  filter(
    sleep_stage %in% c("NREM2", "NREM3", "REM"),
    Group %in% c("CDR0_AD-", "CDR0_AD+"),
    MOCATOTS_ZSCORE > -1.5,
    TRAILA_ZSCORE > -2
  ) %>% 
  mutate(
    sleep_stage = droplevels(sleep_stage),
    Group = droplevels(Group)
  )

# clean up 
rm(
  df_cog,
  df_coinc,
  df_coinc_agg,
  df_coinc_relab,
  df_dems,
  df_csf,
  coinc_files,
  cog_path,
  coinc_path,
  csf_path,
  dems_path,
  ss_path,
  df_ss,
  df_wake_presleep_clean,
  ss_order
)
