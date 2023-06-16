Activity 3 - MLR
================

``` r
# Get sheet names
lake_sheets <- excel_sheets("~/STA631/Activities/activity03-mlr/data/buoy-2011-2020.xlsx")  

# Read all sheets to list
list_all <- lapply(lake_sheets, function(x) {       
  as.data.frame(read_excel("~/STA631/Activities/activity03-mlr/data/buoy-2011-2020.xlsx", 
                           sheet = x))})

# New names for sheets that follow R naming conventions
new_names <- c("meteor", "h20_tempf", "h20_tempc", "dissolved_02", 
               "depth", "chl_phyco", "notes")

# Rename list elements in list_all with new_names
names(list_all) <- new_names

# Extract list elements to Global Environment
list2env(list_all, envir = .GlobalEnv)
```

    ## <environment: R_GlobalEnv>

``` r
# Change names

chl_phyco_names <- c("dates", "chlor", "phyco")
names(chl_phyco) <- chl_phyco_names

depth_names <- c("dates", "depth_2m", "depth_5m", "depth_8m", "depth_11m")
names(depth) <- depth_names

dissolved_02_names <- c("dates", "sat_2m", "conc_2m", "sat_5m", "conc_5m",
  "sat_8m", "conc_8m", "sat_11m", "conc_11m")
names(dissolved_02) <- dissolved_02_names

h20_tempf_names <- c("dates", "tnode_1.7m", "temp_2m", "tnode_4m", "temp_5m", 
  "tnode_6m", "tnode_8m", "tnode_10m", "temp_11m")
names(h20_tempf) <- h20_tempf_names

meteor_names <- c("dates", "air_temp_f", "humidity", "barom", "wind_speed",
  "max_wind_speed", "wind_direct", "rain_intensity", "tot_rain",  "annual_rain_accum", "inter_rain")
names(meteor) <- meteor_names
```

``` r
lake_data <- dissolved_02 %>% inner_join(h20_tempf, by = "dates")

lake_data <- lake_data %>% inner_join(chl_phyco, by = "dates")
```

1.  Is this an observational study or an experiment?

**Answer:** This study is observational.

2.  Describe the distribution of your response variable. Is the
    distribution skewed? Are there any other interesting/odd features
    (outliers, multiple peaks, etc.)? Is this what you expected to see?
    Why, or why not?

**Answer:** The distribution of my response variable `concentration` is
skewed to the left. Noteworthy is that there appears to be a slight
u-shape in that left tail. There is no temp reading at 8 meters in this
dataset so I did not include the corresponding 8m concentration
readings, so that could have something to do with it.

3.  Excluding your response variable, select two other numeric variables
    (hint: look for <dbl> or <int> designations) and describe their
    relationship with each other using an appropriate visualization.

**Answer:** Are we choosing chlor or phyco? I have no idea what is going
on in these scatterplots lol.

``` r
lake_data <- lake_data %>% 
  pivot_longer(c(conc_2m, conc_5m, conc_11m), names_to = "conc_meters", 
               values_to = "concentration") %>% 
  pivot_longer(c(temp_2m, temp_5m, temp_11m), names_to = "temp_meters", 
               values_to = "temp_f") %>% 
  select(dates, conc_meters, concentration, temp_meters, temp_f, chlor, phyco)
```

``` r
ggplot(lake_data) +
  geom_histogram(aes(x = concentration))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 72327 rows containing non-finite values (`stat_bin()`).

![](activity03_files/figure-gfm/explore%20variables-1.png)<!-- -->

``` r
ggplot(lake_data) +
  geom_point(aes(x = temp_f, y = phyco))
```

    ## Warning: Removed 108468 rows containing missing values (`geom_point()`).

![](activity03_files/figure-gfm/explore%20variables-2.png)<!-- -->

``` r
ggplot(lake_data) +
  geom_point(aes(x = temp_f, y = chlor))
```

    ## Warning: Removed 128394 rows containing missing values (`geom_point()`).

![](activity03_files/figure-gfm/explore%20variables-3.png)<!-- -->

``` r
lake_data %>% 
  select(concentration, temp_f, phyco) %>% 
  ggpairs()
```

    ## Warning: Removed 72327 rows containing non-finite values (`stat_density()`).

    ## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
    ## Removed 73452 rows containing missing values

    ## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
    ## Removed 176532 rows containing missing values

    ## Warning: Removed 73452 rows containing missing values (`geom_point()`).

    ## Warning: Removed 2391 rows containing non-finite values (`stat_density()`).

    ## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
    ## Removed 108468 rows containing missing values

    ## Warning: Removed 176532 rows containing missing values (`geom_point()`).

    ## Warning: Removed 108468 rows containing missing values (`geom_point()`).

    ## Warning: Removed 107127 rows containing non-finite values (`stat_density()`).

![](activity03_files/figure-gfm/pairwise%20relationships-1.png)<!-- -->

4.  For each pair of variables, how would you describe the relationship
    graphically? Do any of the relationships look linear? Are there any
    interesting/odd features (outliers, non-linear patterns, etc.)?

**Answer:** There appears to be a weak relationship between
concentration and temperature. There may be a weak relationship between
temperature and could be

5.  For each pair of variables, how would you describe the relationship
    numerically?

6.  Are your two explanatory variables collinear (correlated)?
    Essentially, this means that adding more than one of these variables
    to the model would not add much value to the model. We will talk
    more on this issue in Activity 4 (other considerations in regression
    models).

``` r
lm_spec <- linear_reg() %>%
set_mode("regression") %>%
set_engine("lm")

lm_spec
```

    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

``` r
mlr_mod <- lm_spec %>% 
fit(concentration ~ temp_f + phyco, data = lake_data)

tidy(mlr_mod)
```

    ## # A tibble: 3 × 5
    ##   term           estimate   std.error statistic  p.value
    ##   <chr>             <dbl>       <dbl>     <dbl>    <dbl>
    ## 1 (Intercept) 16.1        0.0178          901.  0       
    ## 2 temp_f      -0.128      0.000282       -454.  0       
    ## 3 phyco       -0.00000989 0.000000692     -14.3 3.03e-46
