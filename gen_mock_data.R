library(dplyr)

set.seed(123)

# ---- parameters ----
n_patients <- 1000
max_obs <- 5   # max records per patient

# ---- patient-level data ----
patients <- data.frame(
  Reference_Key = sprintf("%08d", 1:n_patients),
  ini_age = round(rnorm(n_patients, 73, 8), 1),
  cci = sample(0:3, n_patients, replace = TRUE, prob = c(0.6,0.25,0.1,0.05)),
  base_res = round(rlnorm(n_patients, log(10), 1), 1)
)

# ---- expand into longitudinal format ----
state_main_mock <- patients %>%
  rowwise() %>%
  do({
    
    n <- sample(2:max_obs, 1)  # number of records per patient
    times <- sort(runif(n, 0, 10))
    
    data.frame(
      Reference_Key = .$Reference_Key,
      ini_age = .$ini_age,
      time = times,
      #age = .$ini_age + times,
      cci = .$cci,
      base_res = .$base_res
    )
  }) %>%
  ungroup()

# ---- generate states ----
state_levels <- c("HNPC", "Localised", "Metastatic", "Death")
    
state_main_mock <- state_main_mock %>%
  group_by(Reference_Key) %>%
  mutate(
    state = cumsum(runif(n()) < 0.3) + 1,
    state = factor(pmin(state, length(state_levels)),
                   levels = 1:length(state_levels),
                   labels = state_levels)
  ) %>%
  ungroup()

# ---- dates ----
start_date <- as.Date("2010-01-01")

state_main_mock <- state_main_mock %>%
  group_by(Reference_Key) %>%
  mutate(
    inidate = start_date + sample(0:1000, 1),
    date = inidate + round(time*365),
    diag_date = inidate,
    base_lab_date = inidate - 30,
    Death_date = ifelse(any(state == "Death"),
                        as.character(max(date[state=="Death"])),
                        NA)
  ) %>%
  ungroup()

# ---- factor variables ----
state_main_mock <- state_main_mock %>%
  mutate(
    ini_age_grp = cut(ini_age,
                      breaks = c(0,65,70,75,80,Inf),
                      labels = c("<=65","66~70","71~75","76~80",">80")),
    
    cci_grp = factor(cci,
                     levels = 0:3,
                     labels = c("0","1","2",">=3")),
    
    base_res_grp = cut(base_res,
                       breaks = c(0,20,58,Inf),
                       labels = c("<=20","20.1~58",">58"))
  )

saveRDS(state_main_mock, "state_main_mock.rds")
