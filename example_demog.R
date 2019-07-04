library(tidyverse)
library(gt)

gt_tbl <- iris %>%
  group_by(Species) %>%
  mutate(i = row_number()) %>%
  gather(groupname, value, -i, -Species) %>%
  spread(Species, value) %>%
  select(-i) %>%
  group_by(groupname) %>%
  {bind_rows(
    summarize_all(., ~list(sum(!is.na(.)))) %>%
      mutate(rowname = "n"),
    summarize_all(., ~list(list(
      mean = mean(., na.rm = TRUE),
      sd = sd(., na.rm = TRUE)))) %>%
      mutate(rowname = "Mean (SD)"),
    summarize_all(., ~list(median(., na.rm = TRUE))) %>%
      mutate(rowname = "Median"),
    summarize_all(., ~list(list(
      min = min(., na.rm = TRUE),
      max = max(., na.rm = TRUE)))) %>%
      mutate(rowname = "Min - Max")
  )} %>%
  select(groupname, rowname, everything()) %>%
  gt(groupname_col = "groupname", rowname_col = "rowname") %>%
  fmt_sprintf("%.f")(columns = names(.), rows = one_of("n", "Median")) %>%
  fmt_sprintf("%.2f (%.2f)")(columns = names(.), rows = matches("Mean \\(SD\\)")) %>%
  fmt_sprintf("%.1f - %.1f")(columns = names(.), rows = matches("Min - Max"))

library(random.cdisc.data)
adsl <- radsl(50)

names(adsl)
adsl %>%
  group_by(ARMCD) %>%
  summarize_at("AGE",
               mean = mean,
               min = min,
               max = max)