# last edited: 2.26.29
# last editor: cjm

# prerequisites ----
#libraries
library('lubridate')
library ('ipumsr')
library('vtable')
library('sf')
library('tidyverse')

# wd 
setwd("C:/Users/casem/Desktop/immigration/immigration_enforcement")

# import and clean cps data ------

#* import metro area data ----
# import jan 2010 to current (jan 2026)
ddi <- read_ipums_ddi("../data/cps_00029.xml")
data <- read_ipums_micro(ddi)

# subset to msp metro only 
# leaving broad for future analysis on the rural areas that are getting slammed rn
data_msp <- subset(data, METFIPS == 33460)

# Date variable creation -----
# cleaning and new variable creation
data_msp$year_mon <- as.Date(paste0(data_msp$YEAR, "-", data_msp$MONTH, "-", "01"))

# check the number of observations over time
# is not consistent, but is 900 - 1200
print(count(data_msp, YEAR, MONTH), n = 210)

#* race ethnicity variable creation -----
data_msp <- data_msp %>%
  mutate(
    white_non_hisp = (RACE == 100 & HISPAN == 0),
    bipoc = !white_non_hisp
  )               

#* labor variable creation -----
data_msp <- data_msp %>%
  mutate(
    # survey response to employment suppliment
    responded = as.integer(EMPSTAT != 0),
    
    # employment status
    emp_at_work = as.integer(EMPSTAT == 10),
    emp_not_at_work = as.integer(EMPSTAT == 12),
    employed = as.integer(EMPSTAT %in% c(10, 12)),
    
    # hours 
    hrs_last_wk = if_else(
      AHRSWORKT %in% c(999),
      NA_real_,
      as.numeric(AHRSWORKT)
    ),
    
    # usual hours
    usual_hrs = if_else(
      UHRSWORKT %in% c(997, 999),
      NA_real_,
      as.numeric(UHRSWORKT)
    ),
    
    # percent of usual hours worked
    pct_usual_hrs = if_else(
      !is.na(hrs_last_wk) & !is.na(usual_hrs) & usual_hrs > 0,
      100 * hrs_last_wk / usual_hrs,
      NA_real_
    ),
    
    # share absent 
    absent = case_when(
      ABSENT == 0 ~ 0L,        # NIU = not absent (for employed workers)
      ABSENT == 1 ~ 0L,        # explicitly not absent
      ABSENT %in% c(2, 3) ~ 1L, # absent
      TRUE ~ NA_integer_
    )
  )

# subset data into groups -------
# all workers 
employed <- subset(data_msp, employed ==1)
# white non hisp and bipoc (mutually exclusive)
white_non_hisp <- subset(data_msp, white_non_hisp == 1)
bipoc <- subset(data_msp, bipoc ==1)
# white non hisp employed
white_non_hisp_employed <- subset(employed, white_non_hisp == 1)
bipoc_employed <- subset(employed, bipoc ==1)

# simple summary statistics for whole sample -----
#* define entire sample summary statistics function over entire sample ----
# weight should be individual weights but weighted by month so each month equally represented
weighted_average_full_sample <- function(data) {
  
  # monthly weighted mean x month
  monthly <- data %>%
    group_by(METFIPS, year_mon) %>%
    summarise(
      n_obs = n(),
      survey_response_prob = weighted.mean(responded, WTFINL, na.rm = TRUE),
      share_emp_at_work = weighted.mean(emp_at_work, WTFINL, na.rm = TRUE),
      share_emp_not_at_work = weighted.mean(emp_not_at_work, WTFINL, na.rm = TRUE),
      mean_hrs_last_wk = weighted.mean(ifelse(employed == 1, hrs_last_wk, NA), WTFINL, na.rm = TRUE),
      mean_usual_hrs = weighted.mean(ifelse(employed == 1, usual_hrs, NA), WTFINL, na.rm = TRUE),
      mean_pct_usual_hrs = weighted.mean(ifelse(employed == 1, pct_usual_hrs, NA), WTFINL, na.rm = TRUE),
      share_absent = weighted.mean(ifelse(employed == 1, absent, NA), WTFINL, na.rm = TRUE),
      .groups = "drop"
    )
  
  data_msp %>% count(ABSENT)
  
  data_msp %>% filter(employed == 1) %>% count(ABSENT)  
  
  # mean across all months
  monthly %>%
    summarise(
      n_obs = sum(n_obs),
      n_months = n_distinct(year_mon),
      survey_response_prob = mean(survey_response_prob, na.rm = TRUE),
      share_emp_at_work = mean(share_emp_at_work, na.rm = TRUE),
      share_emp_not_at_work = mean(share_emp_not_at_work, na.rm = TRUE),
      mean_hrs_last_wk = mean(mean_hrs_last_wk, na.rm = TRUE),
      mean_usual_hrs = mean(mean_usual_hrs, na.rm = TRUE),
      mean_pct_usual_hrs = mean(mean_pct_usual_hrs, na.rm = TRUE),
      share_absent = mean(share_absent, na.rm = TRUE)
    )
}

#*  run function for each group and bind into df -----
results <- bind_rows(
  weighted_average_full_sample(data_msp) %>% mutate(group = "All"),
  weighted_average_full_sample(employed) %>% mutate(group = "All - employed"),
  weighted_average_full_sample(white_non_hisp) %>% mutate(group = "White Non-Hispanic"),
  weighted_average_full_sample(bipoc) %>% mutate(group = "BIPOC"),
  weighted_average_full_sample(bipoc_employed) %>% mutate(group = "BIPOC - employed"),
  weighted_average_full_sample(white_non_hisp_employed) %>% mutate(group = "White Non-Hispanic - employed")
)

# pivot to wide
results_wide <- results %>%
  select(-n_months) %>%
  pivot_longer(-group, names_to = "variable") %>%
  pivot_wider(names_from = group, values_from = value) %>%
  mutate(
    variable = recode(variable,
                      "n_obs"                = "N",
                      "survey_response_prob"  = "Survey response probability",
                      "share_emp_at_work"     = "Share employed at work",
                      "share_emp_not_at_work" = "Share employed not at work",
                      "mean_hrs_last_wk"      = "Mean hours worked last week",
                      "mean_usual_hrs"        = "Mean usual weekly hours",
                      "mean_pct_usual_hrs"    = "Mean percent of usual hours worked",
                      "share_absent"          = "Share absent from work"
    ),
    across(where(is.numeric), ~ round(.x, 3))
  )

#* make table ----
results_wide %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    col.names = c("", "All", "All - Employed", "White Non-Hispanic", "BIPOC", "BIPOC - Employed", "White Non-Hispanic - Employed"),
    caption = "National Labor Market Summary by Race/Ethnicity Group",
    label = "tab:national_summary",
    escape = FALSE
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 10
  ) %>%
  save_kable("weighted_average_full_sample.tex")

# monthly summary statistics by group -----
#*  define worker summary statistic function over entire sample ----
weighted_average_by_month <- function(data) {
  data %>%
    group_by(METFIPS, year_mon) %>%
    summarise(
      n_obs = n(),
      survey_response_prob = weighted.mean(responded, WTFINL, na.rm = TRUE),
      share_emp_at_work = weighted.mean(emp_at_work, WTFINL, na.rm = TRUE),
      share_emp_not_at_work = weighted.mean(emp_not_at_work, WTFINL, na.rm = TRUE),
      mean_hrs_last_wk = weighted.mean(ifelse(employed == 1, hrs_last_wk,   NA), WTFINL, na.rm = TRUE),
      mean_usual_hrs = weighted.mean(ifelse(employed == 1, usual_hrs, NA), WTFINL, na.rm = TRUE),
      mean_pct_usual_hrs = weighted.mean(ifelse(employed == 1, pct_usual_hrs, NA), WTFINL, na.rm = TRUE),
      share_absent = weighted.mean(ifelse(employed == 1, absent, NA), WTFINL, na.rm = TRUE),
      se_survey_response_prob = sd(responded, na.rm = TRUE) / sqrt(sum(!is.na(responded))),
      se_share_emp_at_work = sd(emp_at_work, na.rm = TRUE) / sqrt(sum(!is.na(emp_at_work))),
      se_share_emp_not_at_work = sd(emp_not_at_work, na.rm = TRUE) / sqrt(sum(!is.na(emp_not_at_work))),
      se_mean_hrs_last_wk = sd(ifelse(employed == 1, hrs_last_wk,   NA), na.rm = TRUE) / sqrt(sum(employed == 1 & !is.na(hrs_last_wk))),
      se_mean_usual_hrs = sd(ifelse(employed == 1, usual_hrs,     NA), na.rm = TRUE) / sqrt(sum(employed == 1 & !is.na(usual_hrs))),
      se_mean_pct_usual_hrs = sd(ifelse(employed == 1, pct_usual_hrs, NA), na.rm = TRUE) / sqrt(sum(employed == 1 & !is.na(pct_usual_hrs))),
      se_share_absent = sd(ifelse(employed == 1, absent, NA), na.rm = TRUE) / sqrt(sum(employed == 1 & !is.na(absent))),
      .groups = "drop"
    )
}

#* run and label -----
weighted_monthly_all <- weighted_average_by_month(data_msp)  
weighted_monthly_employed <- weighted_average_by_month(employed)      
weighted_monthly_white_non_hisp <- weighted_average_by_month(white_non_hisp) 
weighted_monthly_bipoc <- weighted_average_by_month(bipoc)  
weighted_monthly_white_non_hisp_employed <- weighted_average_by_month(white_non_hisp_employed) 
weighted_monthly_bipoc_employed <- weighted_average_by_month(bipoc_employed)  

#* make tables -----
col_labels <- c(
  "n_obs"                = "N",
  "survey_response_prob" = "Survey response probability",
  "share_emp_at_work"    = "Share employed at work",
  "share_emp_not_at_work"= "Share employed not at work",
  "mean_hrs_last_wk"     = "Mean hours worked last week",
  "mean_usual_hrs"       = "Mean usual weekly hours",
  "mean_pct_usual_hrs"   = "Mean \\% of usual hours worked",
  "share_absent"         = "Share absent from work"
)

# full data table
tbl <- weighted_monthly_all %>%
  select(-starts_with("se_")) %>%
  rename(Month = year_mon, N = n_obs) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

names(tbl)[-1] <- col_labels[names(tbl)[-1]]

tbl %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Monthly Labor Market Summary — All Workers",
    label = "tab:monthly_employed",
    escape = FALSE
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  save_kable("monthly_summary_all_data.tex")

# full employed table 
tbl <- weighted_monthly_all %>%
  select(-starts_with("se_")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  rename(Month = year_mon)
  names(tbl)[-1] <- col_labels[names(tbl)[-1]]
  
  tbl %>%
    kbl(
      format = "latex",
      booktabs = TRUE,
      caption = "Monthly Labor Market Summary — All Workers",
      label = "tab:monthly_employed",
      escape = FALSE
    ) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    save_kable("monthly_summary_employed.tex")
  
# white non hisp table
  tbl <- weighted_monthly_white_non_hisp %>%
    select(-starts_with("se_")) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    rename(Month = year_mon)
  
  names(tbl)[-1] <- col_labels[names(tbl)[-1]]
  
  tbl %>%
    kbl(
      format = "latex",
      booktabs = TRUE,
      caption = "Monthly Labor Market Summary — White Non Hispanic Workers",
      label = "tab:monthly_employed",
      escape = FALSE
    ) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    save_kable("monthly_summary_white_non_hsip.tex")
  
# white non hisp employed
  tbl <- weighted_monthly_white_non_hisp_employed %>%
    select(-starts_with("se_")) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    rename(Month = year_mon)
  
  names(tbl)[-1] <- col_labels[names(tbl)[-1]]
  
  tbl %>%
    kbl(
      format = "latex",
      booktabs = TRUE,
      caption = "Monthly Labor Market Summary — White Non Hispanic Workers",
      label = "tab:monthly_employed",
      escape = FALSE
    ) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    save_kable("monthly_summary_white_non_hisp_employed.tex")
  
  
# BIPOC workers table
  tbl <- weighted_monthly_bipoc %>%
    select(-starts_with("se_")) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    rename(Month = year_mon)
  
  names(tbl)[-1] <- col_labels[names(tbl)[-1]]
  
  tbl %>%
    kbl(
      format = "latex",
      booktabs = TRUE,
      caption = "Monthly Labor Market Summary — BIPOC Workers",
      label = "tab:monthly_bipoc",
      escape = FALSE
    ) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    save_kable("monthly_summary_bipoc.tex")
  
# bipoc workers
  tbl <- weighted_monthly_bipoc_employed %>%
    select(-starts_with("se_")) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    rename(Month = year_mon)
  
  names(tbl)[-1] <- col_labels[names(tbl)[-1]]
  
  tbl %>%
    kbl(
      format = "latex",
      booktabs = TRUE,
      caption = "Monthly Labor Market Summary — BIPOC Workers",
      label = "tab:monthly_bipoc",
      escape = FALSE
    ) %>%
    kable_styling(latex_options = c("hold_position", "scale_down")) %>%
    save_kable("monthly_summary_bipoc_employed.tex")
  
# make graphs ------

#* full employed sample ----
# usual hours worked 
#adding confidence intervals
  weighted_monthly_employed <- weighted_monthly_employed %>%
    mutate(
      year_mon = as.Date(paste0(year_mon, "-01")),
      ci_low = mean_pct_usual_hrs -1.96 * se_mean_pct_usual_hrs,
      ci_high = mean_pct_usual_hrs +1.96 * se_mean_pct_usual_hrs
    ) 
  
# graph
  ggplot(weighted_monthly_employed %>% filter(year_mon >= "2015-01-01"), aes(x = year_mon, y = mean_pct_usual_hrs)) +
    geom_line() +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 10, alpha = 0.5) +
    geom_vline(xintercept = as.Date("2025-12-01"), linetype = "dashed", color = "red") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "12 months") +
    labs(
      x = NULL,
      y = "% of usual hours worked",
      title = "Mean Percent of Usual Hours Worked by Month emnployed"
    ) +
    theme_minimal() 
  ggsave("usual_hours_employed.png", width = 10, height = 6, dpi = 300)
  
# absent
  ggplot(weighted_monthly_all %>% filter(year_mon >= "2015-01-01"), 
         aes(x = year_mon, y = share_absent)) +
    geom_line() +
    geom_ribbon(aes(ymin = share_absent - 1.96 * se_share_absent,
                    ymax = share_absent + 1.96 * se_share_absent), 
                alpha = 0.2) +
    labs(
      title = "Share Absent from Work Over Time all",
      x = "Month",
      y = "Share Absent"
    ) +
    theme_minimal()
  
  ggsave("share_absent_all.png", width = 10, height = 6, dpi = 300)
  
#* white non hisp sample ----
# usual hours worked
# CI 
  weighted_monthly_white_non_hisp <- weighted_monthly_white_non_hisp_employed %>%
    mutate(
      year_mon = as.Date(paste0(year_mon, "-01")),
      ci_low = mean_pct_usual_hrs -1.96 * se_mean_pct_usual_hrs,
      ci_high = mean_pct_usual_hrs +1.96 * se_mean_pct_usual_hrs
    )
  
  ggplot(weighted_monthly_white_non_hisp %>% filter(year_mon >= "2015-01-01"), aes(x = year_mon, y = mean_pct_usual_hrs)) +
    geom_line() +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 10, alpha = 0.5) +
    geom_vline(xintercept = as.Date("2025-12-01"), linetype = "dashed", color = "red") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "12 months") +
    labs(
      x = NULL,
      y  = "% of usual hours worked",
      title = "Mean Percent of Usual Hours Worked by Month WHITE employed"
    ) +
    theme_minimal() 
  ggsave("usual_hours_white_employed", width = 10, height = 6, dpi = 300)
  
# share absent
  ggplot(weighted_monthly_white_non_hisp_employed  %>% filter(year_mon >= "2015-01-01"), aes(x = year_mon, y = mean_pct_usual_hrs)) +
    geom_line() +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 10, alpha = 0.5) +
    geom_vline(xintercept = as.Date("2025-12-01"), linetype = "dashed", color = "red") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "12 months") +
    geom_vline(xintercept = as.Date("2025-12-01"), linetype = "dashed", color = "red") +
    labs(
      x = NULL,
      y = "% of usual hours worked",
      title = "Mean Percent of Usual Hours Worked by Month white non hispanic employed"
    ) +
    theme_minimal() 
  ggsave("share_absent_white_employed.png", width = 10, height = 6, dpi = 300)
  
#* BIPOC ----
# usual hours worked
  # CI
  weighted_monthly_bipoc <- weighted_monthly_bipoc_employed %>%
    mutate(
      year_mon = as.Date(paste0(year_mon, "-01")),
      ci_low = mean_pct_usual_hrs -1.96 * se_mean_pct_usual_hrs,
      ci_high = mean_pct_usual_hrs +1.96 * se_mean_pct_usual_hrs
    )

# hours worked 
  ggplot(weighted_monthly_bipoc  %>% filter(year_mon >= "2015-01-01"), aes(x = year_mon, y = mean_pct_usual_hrs)) +
    geom_line() +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 10, alpha = 0.5) +
    geom_vline(xintercept = as.Date("2025-12-01"), linetype = "dashed", color = "red") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "12 months") +
    geom_vline(xintercept = as.Date("2025-12-01"), linetype = "dashed", color = "red") +
    labs(
      x = NULL,
      y = "% of usual hours worked",
      title = "Mean Percent of Usual Hours Worked by Month BIPOC employed"
    ) +
    theme_minimal() 
  ggsave("usual_hours_black_hisp_employed", width = 10, height = 6, dpi = 300)
  
  ggplot(weighted_monthly_bipoc_employed  %>% filter(year_mon >= "2015-01-01"), aes(x = year_mon, y = mean_pct_usual_hrs)) +
    geom_line() +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 10, alpha = 0.5) +
    geom_vline(xintercept = as.Date("2025-12-01"), linetype = "dashed", color = "red") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "12 months") +
    geom_vline(xintercept = as.Date("2025-12-01"), linetype = "dashed", color = "red") +
    labs(
      x = NULL,
      y = "% of usual hours worked",
      title = "Mean Percent of Usual Hours Worked by Month black employed"
    ) +
    theme_minimal() 
  ggsave("share_absent_black_hisp_employed.png", width = 10, height = 6, dpi = 300)
  