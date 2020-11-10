# Process AC:NH data



Here, we process the AC:NH survey and telemetry files.

We used the following R packages:


```r
library(pacman)
p_load(
  readxl,
  knitr,
  here,
  lubridate,
  scales,
  janitor,
  tidyverse
)
```

## Raw data

This script expects the raw data files to be in `data-raw/noa/`. The raw telemetry and survey files are in a zip compressed file on OSF: <https://osf.io/cjd6z/>. Below, we provide code that downloads and unpacks these files.

The raw survey data was edited to exclude test sessions, any data from individuals who did not consent / below 18, and some unnecessary variables, before uploading to OSF. The code to do that is shown below (but cannot be executed without our formr credentials)


```r
# Dont download if already downloaded
if (!file.exists(here("data-raw/noa/formr-raw.rds"))) {
  # Connect to formr
  formr::formr_connect()
  # These are the survey names on formr
  surveys <- c("gaming_welcome", paste0("gaming_block", 2:5))
  # Download data into a list of data frames
  ac <- map(surveys, ~formr::formr_results(.) %>% as_tibble)
  saveRDS(ac, here("data-raw/noa/formr-raw.rds"))
} else {ac <- readRDS(here("data-raw/noa/formr-raw.rds"))}

# Take out all intermediate time variables
ac[[1]] <- select(ac[[1]], -c(modified:expired))
ac[2:4] <- map(ac[2:4], ~select(., -c(created:expired)))
ac[[5]] <- select(ac[[5]], -c(created, modified, expired))

# Transform to one data frame with one row per participant
ac <- ac %>% reduce(left_join)

# Limit data to survey window to exclude our test sessions
ac <- ac %>% 
  filter(created >= ymd("2020-10-27")) %>% 
  filter(created <= ymd("2020-10-27") + days(7))
range(ac$created)

# Keep only surveys whose code exists and consents check out
ac <- filter(
  ac, !is.na(code), of_age==1, consent_data==1, consent==1
)

# Take out unnecessary variables
ac <- ac %>% 
  select(
    -c(session, browser, last_outside_referrer, of_age:consent)
  )
write_rds(ac, here("data-raw/noa/formr.rds"))
```

The file produced by the above code chunk was uploaded to OSF in a zip compressed archive with the telemetry file.

Start by downloading that file and unpacking it to the target directory by running the code below:


```r
file_dest <- here("data-raw/noa/noa.zip")
# Download & extract file only if you haven't yet
if (!file.exists(file_dest)) {
  download.file("https://osf.io/fev95/download", file_dest)
}
if (!file.exists(here("data-raw/noa/formr.rds"))) {
  unzip(file_dest, exdir = here("data-raw/noa/"))
  
}
```

Next, we load the survey data file.


```r
ac <- readRDS(here("data-raw/noa/formr.rds"))
```

## Process raw files

### Clean survey data

Here, we clean the survey data.
Specifically, we

- Harmonize names so that they are the same as in the other data set (i.e., from PvZ)
- Create duration variable for game time
- Give some sensible variable names
- Assign proper variable types


```r
# Harmonize some names to PvZ names
ac <- ac %>% 
  rename(
    gender = sex,
    gender_other = sex_other,
    player_id = code
    )

# Duration of survey
ac <- ac %>% 
  mutate(
    survey_duration = ended-created
  )
# Create variables for straightliners by checking if variance within a block of questions is zero
ac$straightliner_spane <- apply(
  select(ac, starts_with("spane_") & !starts_with("spane_acnh")), 
  1, sd, na.rm = TRUE
)
ac$straightliner_spane <- ac$straightliner_spane==0
ac$straightliner_motivations <- apply(
  select(
    ac, 
    starts_with("autonomy_"), 
    starts_with("competence_"), 
    starts_with("related_"), 
    starts_with("enjoymen_"), 
    starts_with("extrinsic_")
  ), 
  1, sd, na.rm = TRUE
)
ac$straightliner_motivations <- ac$straightliner_motivations==0

# These are needed as factors
ac <- ac %>% 
  mutate(across(c(gender, played), as_factor))

# Reverse scored items
ac <- ac %>% 
  mutate(
    across(
      c(
        related_not_close,
        enjoyment_attention,
        enjoymen_boring
      ),
      ~ 8 - .x
    )
  )
```

Next, let's create mean indices for the scales like we did before.
SPANE has positive affect, negative affect, and an affect balance score (subtract negative from positive).

```r
# Need to rename SPANE item so it doesnt become confused with scale score name
ac <- rename(
  ac, 
  spane_positiveItem = spane_positive,
  spane_negativeItem = spane_negative
)

# General SPANE
ac <- ac %>% 
  mutate(
    spane_positive = rowMeans(
      select(
        .,
        spane_positiveItem,
        spane_good,
        spane_pleasant,
        spane_happy,
        spane_joyful,
        spane_contented
      ),
      na.rm = TRUE
    ),
    spane_negative = rowMeans(
      select(
        .,
        spane_negativeItem,
        spane_bad,
        spane_unpleasant,
        spane_sad,
        spane_afraid,
        spane_angry
      ),
      na.rm = TRUE
    ),
    spane_balance = spane_positive - spane_negative
  )

# Motivations
ac <- ac %>% 
  mutate(
    autonomy = rowMeans(
      select(., starts_with("autonomy")), na.rm = TRUE
    ),
    competence = rowMeans(
      select(., starts_with("competence")), na.rm = TRUE
    ),
    relatedness = rowMeans(
      select(., starts_with("related")), na.rm = TRUE
    ),
    enjoyment = rowMeans(
      select(., starts_with("enjoymen")), na.rm = TRUE
    ),
    extrinsic = rowMeans(
      select(., starts_with("extrinsic")), na.rm = TRUE
    )
  )

# SPANE because of playing AC:NH
ac <- ac %>% 
  mutate(
    spane_game_positive = rowMeans(
      select(
        .,
        spane_acnh_positive,
        spane_acnh_good,
        spane_acnh_pleasant,
        spane_acnh_happy,
        spane_acnh_joyful,
        spane_acnh_contented
      ),
      na.rm = TRUE
    ),
    spane_game_negative = rowMeans(
      select(
        .,
        spane_acnh_negative,
        spane_acnh_bad,
        spane_acnh_unpleasant,
        spane_acnh_sad,
        spane_acnh_afraid,
        spane_acnh_angry
      ),
      na.rm = TRUE
    ),
    spane_game_balance = spane_game_positive - spane_game_negative
  )

# Hours of estimated play
ac <- ac %>% 
  mutate(
    active_play = active_play_hours + (active_play_minutes / 60)
  )
```

### Checking

First, we check how many rows per player there are.


```r
count(ac, player_id, sort = T)
```

```
## # A tibble: 5,987 x 2
##    player_id              n
##    <chr>              <int>
##  1 ""                    13
##  2 "7dadc6e88bc0b5b8"     3
##  3 "33ec57527b06af7b"     2
##  4 "48bb7bafae2e5c44"     2
##  5 "49f5299432fef738"     2
##  6 "5b9d217b3cbb39fa"     2
##  7 "677ca59a6ed1e5a8"     2
##  8 "7092634f86aa0992"     2
##  9 "842b41d5e20f2922"     2
## 10 "8bd8d7292b3a05b5"     2
## # … with 5,977 more rows
```

There are two kinds of problems:

1. No ID was captured
2. An ID was used more than once

For both cases, connecting to telemetry would be impossible (and wrong connections could be made in latter case), so we drop these cases.


```r
ac <- add_count(ac, player_id) %>% 
  filter(n == 1) %>% 
  select(-n)
```

### Telemetry

This file is in the ZIP archive.


```r
gt <- read_tsv(here("data-raw/noa/telem_data (since Sep 2020).txt"))
```

Column definitions:

- `lc_recorded_at` = Session start date/time
- `nc_recorded_at` = Session end date/time
- `hashed_id` = Hashed account ID
- `product_model` =	Switch model game was played on
- `operation_mode` = Identifies handheld mode, TV mode
- `duration` = Duration of session (seconds)
- `storage_id` = Whether game is played off game card, SD card or internal system memory
- `application_id_hex` = Game's hashed ID

We drop some unnecessary variables


```r
gt <- select(
  gt,
  hashed_id, 
  contains("recorded"),
  duration
)
```

Then rename


```r
names(gt) <- c(
  "player_id", "session_start", "session_end", "Hours"
)
```

And turn duration into hours


```r
gt$Hours <- gt$Hours/60/60
```

Assume that timestamps are US Pacific as this was used to report data collection dates & times.


```r
gt <- gt %>% 
  mutate(
    across(contains("session"), ~mdy_hm(.x, tz = "US/Pacific"))
  )
glimpse(gt)
```

```
## Rows: 191,498
## Columns: 4
## $ player_id     <chr> "be080cb5754884b3", "35a6c680bcf65615", "35a6c680bcf656…
## $ session_start <dttm> 2020-10-15 22:39:00, 2020-09-09 03:38:00, 2020-10-03 1…
## $ session_end   <dttm> 2020-10-15 22:39:00, 2020-09-09 03:38:00, 2020-10-03 1…
## $ Hours         <dbl> 0.67583333, 0.19111111, 0.67111111, 1.77750000, 1.76000…
```

### Clean

We don't need to limit to IDs who took the survey as NOA has already done that--these data only contain folks who filled in the survey.

We do need to limit the data to two weeks preceding the survey, and count session durations within that window. We therefore need to use the session start/end times to find out when the sessions happened.

Some processing is required to do that as the times can have noise due to e.g. players' system times being incorrectly set. Thus, many start times are the same (or even later) than the end time:


```r
gt %>% 
  mutate(
    later_or_same_start = session_start >= session_end
  ) %>% 
  tabyl(later_or_same_start) %>% 
  adorn_pct_formatting()
```

```
##  later_or_same_start      n percent
##                FALSE  13112    6.8%
##                 TRUE 178386   93.2%
```

However, session durations are not based on the device time (session times):


```r
gt %>% 
  mutate(
    duration = as.numeric(session_end - session_start)/60/60
  ) %>% 
  mutate(match = duration/60/60==Hours) %>% 
  tabyl(match) %>% 
  adorn_pct_formatting()
```

```
##  match      n percent
##  FALSE 191494  100.0%
##   TRUE      4    0.0%
```

And end and start times should be within a window. 


```r
tmp <- gt %>% 
  filter(
    session_start < ymd("2020-09-01") |
      session_end < ymd("2020-09-01") |
      session_start > ymd("2020-11-03") |
      session_end > ymd("2020-11-03") 
  ) %>% 
  arrange(session_start) %>% 
  mutate(player_id = fct_inorder(player_id))
# Proportion of these bad dates
percent(nrow(tmp) / nrow(gt), .1)
```

```
## [1] "3.1%"
```

We filter out sessions that aren't in the two weeks preceding each player's survey. Note not all survey respondents have telemetry so the resulting table will be longer


```r
# Get survey times from survey table
gt <- select(ac, player_id, created) %>% 
  left_join(gt) 

# Limit telemetry sessions to appropriate time window
gt <- gt %>%   
  filter(session_start >= (created - days(14))) %>% 
  filter(session_start < created) %>% 
  filter(session_end < created)
```

We then summarize to total hours per person.


```r
gt <- gt %>% 
  group_by(player_id) %>% 
  summarise(
    Hours = sum(Hours), 
    n_sessions = n()
  )
```

### Join survey and telemetry


```r
ac <- left_join(ac, gt)
```

### Checking

People reported if they played AC:NH in the past 14 days. Lets summarise the players, number of players with telemetry, and mean hours, for these two groups


```r
ac %>% 
  group_by(played) %>% 
  summarise(
    Players = n(),
    Missing_Hours = sum(is.na(Hours)),
    Mean_Hours = mean(Hours, na.rm = TRUE)
  )
```

```
## # A tibble: 3 x 4
##   played                                        Players Missing_Hours Mean_Hours
##   <fct>                                           <int>         <int>      <dbl>
## 1 I have played Animal Crossing: New Horizons …    4602          2017      11.7 
## 2 I have **NOT** played Animal Crossing: New H…    1233          1114       2.09
## 3 <NA>                                              140            88       6.75
```

## Exclusions

First save a file with no exclusions.


```r
write_rds(ac, here("data/noa/ac.rds"))
```

### Straightliners

We take out all individuals who straightlined (gave the same response to every item) through SPANE and motivations scales. (If only SPANE items existed, then we didn't exclude.)


```r
ac <- ac %>% 
  mutate(
    straightliner = 
      straightliner_spane & straightliner_motivations
  ) 
ac %>%   
  select(contains("straight")) %>% 
  group_by_all() %>% 
  count
```

```
## # A tibble: 7 x 4
## # Groups:   straightliner_spane, straightliner_motivations, straightliner [7]
##   straightliner_spane straightliner_motivations straightliner     n
##   <lgl>               <lgl>                     <lgl>         <int>
## 1 FALSE               FALSE                     FALSE          4266
## 2 FALSE               TRUE                      FALSE             7
## 3 FALSE               NA                        FALSE          1215
## 4 TRUE                FALSE                     FALSE            20
## 5 TRUE                TRUE                      TRUE              8
## 6 TRUE                NA                        NA               19
## 7 NA                  NA                        NA              440
```

```r
ac %>%   
  tabyl(straightliner) %>% 
  adorn_pct_formatting()
```

```
##  straightliner    n percent valid_percent
##          FALSE 5508   92.2%         99.9%
##           TRUE    8    0.1%          0.1%
##             NA  459    7.7%             -
```

```r
# filter() would also exclude NAs
ac <- filter(ac, !straightliner | is.na(straightliner))
```

### Outliers

Potential outliers. We replace all values that are more than 6SD away from the variable's mean with NAs. As a consequence, individuals are excluded on an analysis-by-analysis case (so if has bad data relevant to that analysis or figure).

This is only done for a subset of variables (relavant to analyses; see below)


```r
ac <- ac %>% 
  # These variables will be affected
  pivot_longer(
    c(
      spane_positiveItem:Hours, 
      -played_with_others, -ended, -survey_duration,
      -contains("straightliner")
    )
  ) %>% 
  group_by(name) %>% 
  mutate(z_value = as.numeric(scale(value))) 
```

These are the numbers of people taken out of each variable (only variables that were affected are shown):


```r
# This is what are taken out
ac %>% 
  summarise(
    Extremes = sum(abs(z_value>=6), na.rm = TRUE),
    Extremes_p = percent(Extremes/n(), accuracy = .01)
  ) %>% 
  filter(Extremes > 0)
```

```
## # A tibble: 5 x 3
##   name                Extremes Extremes_p
##   <chr>                  <int> <chr>     
## 1 active_play               37 0.62%     
## 2 active_play_hours         37 0.62%     
## 3 Hours                     12 0.20%     
## 4 spane_acnh_afraid         25 0.42%     
## 5 spane_game_negative        4 0.07%
```

Code to do it:


```r
ac <- ac %>%
  mutate(value = ifelse(abs(z_value >= 6), NA, value)) %>% 
  select(-z_value) %>% 
  pivot_wider(names_from = "name", values_from = "value") %>% 
  ungroup()
```

## Save files


```r
write_rds(ac, here("data/noa/ac-excluded.rds"))
```

## Session info

```r
sessionInfo()
```

```
## R version 4.0.3 (2020-10-10)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 20.04.1 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3
## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/liblapack.so.3
## 
## locale:
##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] forcats_0.5.0   stringr_1.4.0   dplyr_1.0.2     purrr_0.3.4    
##  [5] readr_1.4.0     tidyr_1.1.2     tibble_3.0.4    ggplot2_3.3.2  
##  [9] tidyverse_1.3.0 janitor_2.0.1   scales_1.1.1    lubridate_1.7.9
## [13] here_0.1        knitr_1.30      readxl_1.3.1    pacman_0.5.1   
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_1.1.0 xfun_0.19        haven_2.3.1      snakecase_0.11.0
##  [5] colorspace_1.4-1 vctrs_0.3.4      generics_0.1.0   htmltools_0.5.0 
##  [9] yaml_2.2.1       utf8_1.1.4       rlang_0.4.8      pillar_1.4.6    
## [13] withr_2.3.0      glue_1.4.2       DBI_1.1.0        dbplyr_2.0.0    
## [17] modelr_0.1.8     lifecycle_0.2.0  munsell_0.5.0    gtable_0.3.0    
## [21] cellranger_1.1.0 rvest_0.3.6      evaluate_0.14    parallel_4.0.3  
## [25] fansi_0.4.1      broom_0.7.2      Rcpp_1.0.5       backports_1.2.0 
## [29] jsonlite_1.7.1   fs_1.5.0         hms_0.5.3        digest_0.6.27   
## [33] stringi_1.5.3    bookdown_0.21    rprojroot_1.3-2  grid_4.0.3      
## [37] cli_2.1.0        tools_4.0.3      magrittr_1.5     crayon_1.3.4    
## [41] pkgconfig_2.0.3  ellipsis_0.3.1   xml2_1.3.2       reprex_0.3.0    
## [45] assertthat_0.2.1 rmarkdown_2.5.2  httr_1.4.2       rstudioapi_0.11 
## [49] R6_2.5.0         compiler_4.0.3
```

