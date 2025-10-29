COPD Prevalence Map
================
Your Name
2025-10-29

## Setup

``` r
data_text <- "
StatePercentCountPercentCountPercentCount
Alabama8.6161,90010.1207,5009.4369,400
Alaska5.315,3006.116,1005.731,400
Arizona5.0137,8006.4180,7005.7318,500
Arkansas8.697,60010.5125,6009.6223,200
California4.5680,6004.7733,1004.61,413,800
Colorado4.398,9005.9135,1005.1233,900
Connecticut4.765,4005.987,2005.3152,500
Delaware5.922,4006.627,3006.249,700
District of Columbia4.110,6005.115,1004.625,700
Florida (2020)6.8574,6008.2745,5007.51,320,100
Kentucky9.8166,80012.0214,90010.9381,700
West Virginia11.882,30014.4103,90013.1186,200
United States5.56,800,4006.88,875,1006.215,587,000
"
```

``` r
# 1. Split the single string into a vector of individual lines
lines <- str_split(data_text, "\n")[[1]]

# 2. Clean up lines: remove whitespace and empty lines
lines <- trimws(lines)
lines <- lines[lines != ""]

lines <- lines[-1]


pattern <- "^([A-Za-z .()0-9]+)(\\d+\\.\\d)([\\d,]+)(\\d+\\.\\d)([\\d,]+)(\\d+\\.\\d)([\\d,]+)$"

# 4. Apply str_match to EACH line
rows <- str_match(lines, pattern)

# 5. Remove any lines that didn't match
rows <- rows[!is.na(rows[,1]), ]

# 6. Create the data frame
df <- as.data.frame(rows[,2:8])
colnames(df) <- c("State","Percent_Female","Count_Female","Percent_Male","Count_Male","Percent_Total","Count_Total")

df <- df %>%
  filter(State != "United States") %>%
  mutate(across(starts_with("Percent"), as.numeric),
         # 7. Use str_replace_all to remove ALL commas
         across(starts_with("Count"), ~ as.numeric(str_replace_all(.x, ",", ""))),
         State = str_replace(State, "\\s*\\(.*\\)", ""), # Removes text in parentheses
         State = str_trim(State))

# Check the result
print(df)
```

    ##                   State Percent_Female Count_Female Percent_Male Count_Male
    ## 1               Alabama            8.6      1619001          0.1     207500
    ## 2                Alaska            5.3        15300          6.1      16100
    ## 3               Arizona            5.0       137800          6.4     180700
    ## 4              Arkansas            8.6       976001          0.5     125600
    ## 5            California            4.5       680600          4.7     733100
    ## 6              Colorado            4.3        98900          5.9     135100
    ## 7           Connecticut            4.7        65400          5.9      87200
    ## 8              Delaware            5.9        22400          6.6      27300
    ## 9  District of Columbia            4.1        10600          5.1      15100
    ## 10              Florida            6.8       574600          8.2     745500
    ## 11             Kentucky            9.8      1668001          2.0    2149001
    ## 12       West Virginia1            1.8       823001          4.4    1039001
    ##    Percent_Total Count_Total
    ## 1            9.4      369400
    ## 2            5.7       31400
    ## 3            5.7      318500
    ## 4            9.6      223200
    ## 5            4.6     1413800
    ## 6            5.1      233900
    ## 7            5.3      152500
    ## 8            6.2       49700
    ## 9            4.6       25700
    ## 10           7.5     1320100
    ## 11           0.9      381700
    ## 12           3.1      186200

``` r
heatmap_data <- df %>%
  select(State, Percent_Female, Percent_Male, Percent_Total) %>%
  pivot_longer(cols = starts_with("Percent_"),
               names_to = "Group",
               values_to = "Percent") %>%
  mutate(Group = str_replace(Group, "Percent_", "")) %>%
  rename(state = State) # <-- THIS IS THE FIX

# Check the result
print(head(heatmap_data))
```

    ## # A tibble: 6 × 3
    ##   state   Group  Percent
    ##   <chr>   <chr>    <dbl>
    ## 1 Alabama Female     8.6
    ## 2 Alabama Male       0.1
    ## 3 Alabama Total      9.4
    ## 4 Alaska  Female     5.3
    ## 5 Alaska  Male       6.1
    ## 6 Alaska  Total      5.7

``` r
# Generate the map using YOUR data
plot_usmap(
  data = heatmap_data, 
  values = "Percent",
  regions = "states"
) +
  scale_fill_viridis_c(name = "Percent") +
  facet_wrap(~ Group) + # This creates the two maps for Male/Female
  labs(title = "COPD Prevalence by U.S. State and Gender") +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 14, face = "bold")
  )
```

    ## Warning in CPL_transform(x, crs, aoi, pipeline, reverse, desired_accuracy, :
    ## GDAL Error 1: PROJ: proj_create_from_database: crs not found
    ## Warning in CPL_transform(x, crs, aoi, pipeline, reverse, desired_accuracy, :
    ## GDAL Error 1: PROJ: proj_create_from_database: crs not found
    ## Warning in CPL_transform(x, crs, aoi, pipeline, reverse, desired_accuracy, :
    ## GDAL Error 1: PROJ: proj_create_from_database: crs not found
    ## Warning in CPL_transform(x, crs, aoi, pipeline, reverse, desired_accuracy, :
    ## GDAL Error 1: PROJ: proj_create_from_database: crs not found
    ## Warning in CPL_transform(x, crs, aoi, pipeline, reverse, desired_accuracy, :
    ## GDAL Error 1: PROJ: proj_create_from_database: crs not found
    ## Warning in CPL_transform(x, crs, aoi, pipeline, reverse, desired_accuracy, :
    ## GDAL Error 1: PROJ: proj_create_from_database: crs not found
    ## Warning in CPL_transform(x, crs, aoi, pipeline, reverse, desired_accuracy, :
    ## GDAL Error 1: PROJ: proj_create_from_database: crs not found
    ## Warning in CPL_transform(x, crs, aoi, pipeline, reverse, desired_accuracy, :
    ## GDAL Error 1: PROJ: proj_create_from_database: crs not found
    ## Warning in CPL_transform(x, crs, aoi, pipeline, reverse, desired_accuracy, :
    ## GDAL Error 1: PROJ: proj_create_from_database: crs not found

![](Heatmap_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
