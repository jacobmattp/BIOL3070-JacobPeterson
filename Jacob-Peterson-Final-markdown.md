Final report: COPD rates compared to average air quality throughout the
year
================
Jacob Peterson
11/20/2025

- [ABSTRACT](#abstract)
- [INTRO](#intro)
- [METHODS](#methods)
- [DISCUSSION](#discussion)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

Chronic Obstructive Pulmonary Disease and air quality have been studied
extensively to deduce an interconnection between the two. Our goal is to
see this linkage in action, asking the question: Does decreased air
quality lead to an increase in COPD across the United States? To reach
our goal, we looked at the air quality data provided by the World
Population Review and COPD rates across the US provided by the American
Lung Association. With these two data sets, we used statistical tests,
the Pearson correlation test, and generalized linear models, to look for
an association between the two data sets. Our results revealed a small
relationship that was significant, specifically, 11% of COPD cases
appear to be caused by low air quality inhalation for extended periods
of time. Thus, low air quality exposure can lead to COPD and other
pulmonary diseases, and steps should be taken to improve low air quality
states to improve life expectancy in the United States.

# Intro

Air Quality Index (AQI) is a standardized system used to measure the
quality of the air around us. The AQI is calculated using the four major
air pollutants, as determined by the Clean Air Act, which are
ground-level ozone, particle pollution, carbon monoxide, and sulfur
dioxide. AQI is measured on a scale of 0-500, with 0 being the cleanest
air and 500 being the most hazardous. This categorizes 0 as the highest
air quality and 500 as the lowest air quality. AQI is a useful tool that
has become integrated into our weather apps, making it accessible to the
majority of the population. One of the primary causes of death in urban
areas is chronic obstructive pulmonary disease (COPD). COPD can be
caused by many factors including tobacco use, occupational factors,
infection, and air pollution. This disease progresses slowly and worsens
with prolonged exposure. The damage is usually not reversible and can
lead to pulmonary failure. There are many factors that play into the
prevalence of COPD like region, age, and sex.

STUDY QUESTION & HYPOTHESIS:

With this information in mind, we have posed the following question: “Do
areas of low air quality have higher rates of Chronic Obstructive
Pulmonary Disease (COPD)?” Although there are many factors that
influence COPD, if there is a negative correlation between air quality
and rates of COPD there are many known preventative measures that can be
taken to prevent COPD and improve lung health. These findings can
influence health practices and help those susceptible to COPD avoid risk
factors, such as low air quality. Although there are many factors that
can play into rates of COPD, we hypothesize that this negative
correlation will exist and that areas with lower air quality will have
higher rates of Chronic Obstructive Pulmonary Disease.

# METHODS

We collected data from two databases, World Population Review air
quality database, and The National Lung Association COPD rates. All data
was formatted, normalized, and tested in R. We ran the data through two
statistical tests, a Pearson correlation test, and a generalized linear
model.

This formats our data so R can effectively read and format the states
data First we define define our pattern and format the copd data Second
we create our data frame

``` r
# This chunk was generated with AI
lines <- str_split(data_text, "\n")[[1]]
lines <- trimws(lines)
lines <- lines[lines != ""]
lines <- lines[-1]

# 4. Define the pattern
pattern <- "^([A-Za-z .()0-9]+?)\\s+(\\d+\\.\\d)\\s+([\\d,]+)\\s+(\\d+\\.\\d)\\s+([\\d,]+)\\s+(\\d+\\.\\d)\\s+([\\d,]+)$"
rows <- str_match(lines, pattern)
rows <- rows[!is.na(rows[,1]), ]

# 7. Create the data frame
df <- as.data.frame(rows[,2:8])
colnames(df) <- c("State","Percent_Male","Count_Male","Percent_Female","Count_Female","Percent_Total","Count_Total")
df <- df %>%
  # Filter out rows i don't want to plot
  filter(State != "United States") %>%
  filter(State != "District of Columbia") %>% # <-- NEW FIX: Remove D.C.
  # Convert columns to the right type
  mutate(across(starts_with("Percent"), as.numeric),
         across(starts_with("Count"), ~ as.numeric(str_replace_all(.x, ",", ""))),
         
         State = str_replace(State, "\\s*\\(.*\\)", ""), # Removes (2020)
         State = str_trim(State))                       # Removes whitespace
```

Creation of the COPD gender maps and colors We first join the map data
and create the color scale We then map the data Figure 1. COPD
prevalence is compared between males and females in the continental
United States. Females show a slightly higher average of COPD.

``` r
# This chunk was troubleshooted and enhanced with AI
# 1. map data
states_map <- map_data("state")

# 2. heat map data to join
heatmap_data <- df %>%
  select(State, Percent_Female, Percent_Male) %>%
  pivot_longer(cols = starts_with("Percent_"),
               names_to = "Group",
               values_to = "Percent") %>%
  mutate(Group = str_replace(Group, "Percent_", ""),
         region = tolower(State)) 
map_plot_data <- left_join(states_map, heatmap_data, by = "region", relationship = "many-to-many")

map_plot_data %>%
  filter(!is.na(Group)) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = Percent)) +
  
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(name = "Percent") +
  facet_wrap(~ Group) + 
  labs(title = "COPD Prevalence by U.S. State and Gender") +
  theme_void() + 
  coord_map() + 
  theme(
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 14, face = "bold")
  
  )
```

![](Jacob-Peterson-Final-markdown_files/figure-gfm/copd%20by%20gender%20map%20creation-1.png)<!-- -->

Figure 2. COPD percent difference for females compared to males in
shown, with most states having a higher percentage of COPD prevalence in
females. This code creates the difference between sexes map. We first
differentiate male and female. Second we plot the difference. Lastly the
color scale is created and the map is printed.

``` r
# This chunk was troubleshooted and enhanced with AI
states_map <- map_data("state") 
difference_data <- df %>%
  mutate(
    Percent_Difference = Percent_Female - Percent_Male,
    region = tolower(State) # <-- This creates the join key
  ) %>%
  select(region, Percent_Difference)
map_diff_data <- left_join(states_map, difference_data, by = "region")
ggplot(map_diff_data, aes(x = long, y = lat, group = group, fill = Percent_Difference)) +
  geom_polygon(color = "white", linewidth = 0.1) + # Draw the states
  # 5. Use the DIVERGING color scale
  scale_fill_gradient2(
    name = "Percent Difference\n(Female - Male)",
    low = "blue",      # States where males are higher (negative)
    mid = "white",     # States where rates are equal (zero)
    high = "red",      # States where females are higher (positive)
    midpoint = 0       # We center the scale at zero
  ) +
  
  labs(title = "Difference in COPD Prevalence (Female vs. Male)") +
  theme_void() + # Use a clean theme
  coord_map() +  # Use correct map projection
  theme(
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
```

![](Jacob-Peterson-Final-markdown_files/figure-gfm/difference%20in%20genders%20map-1.png)<!-- -->

# air quality index

Figure 3. Overall AQI is shown for each of the continental United
States, averaged over the year 2025. All states land generally within an
acceptable range for daily intake, with some on the higher side
indicating a lower AQI. This code chunk creates the overall AQI map. We
begin by reformatting the raw data. Second we join the reformatted data
to the mapping package. Lastly we print the map

``` r
# This chunk was troubleshooted and enhanced with AI
states_map <- map_data("state")

aq_data_to_plot <- aq_df %>%
  # Use the correct column names: `state` and `AirQuality_AirQualityIndexViaUSA_num_YearFree`
  # We also rename the long AQI column to `Overall_AQI` to make it easier to use
  select(State = state, Overall_AQI = `AirQuality_AirQualityIndexViaUSA_num_YearFree`) %>% 
  mutate(region = tolower(State)) # <-- This creates the join key

map_plot_data <- left_join(states_map, aq_data_to_plot, by = "region")


map_plot_data <- map_plot_data %>%
  filter(!is.na(Overall_AQI))

ggplot(map_plot_data, aes(x = long, y = lat, group = group, fill = Overall_AQI)) +
  geom_polygon(color = "white", linewidth = 0.1) + # Draw the states
  
  scale_fill_viridis_c(name = "Overall AQI") +
  
  labs(title = "Overall Air Quality Index (AQI) by U.S. State (2025)") +
  theme_void() + # Use a clean theme
  coord_map() +  # Use correct map projection
  theme(
    legend.position = "right",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
```

![](Jacob-Peterson-Final-markdown_files/figure-gfm/AQI%20map-1.png)<!-- -->

This code normalizes and reformats our data. We begin by loading needed
libraries. Then we load in our data and format to recombine them. Lastly
we normalize our data using the SQRT method.

``` r
# This chunk was troubleshooted and enhanced with AI
library(readr)
library(dplyr)
library(tidyr) # For pivoting later
library(ggplot2) # For the scatter plot

# 2. Reload two data frames 
aq_df <- read_csv("air-quality-by-state-2025.csv")
```

    ## Rows: 51 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): state
    ## dbl (4): AirQuality_AirQualityIndexViaUSA_num_YearFree, AirQualityRankViaUSN...
    ## lgl (1): stateFlagCode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# 3. Join the two data frames by state
# We need to make sure the state names match.
# 'df' has "State" (e.g., "Alabama")
# 'aq_df' has "state" (e.g., "Alabama")

combined_df <- inner_join(df, aq_df, by = c("State" = "state"))


aqi_column_name <- "AirQuality_AirQualityIndexViaUSA_num_YearFree"

combined_df <- combined_df %>%
  mutate(
    # A. Normalize COPD rates (Square Root)
    Percent_Total_Norm = sqrt(Percent_Total),
    Percent_Male_Norm = sqrt(Percent_Male),
    Percent_Female_Norm = sqrt(Percent_Female),
    
    # B. Normalize AQI (Reflect + Square Root)
    k = max(!!sym(aqi_column_name)) + 1,
    AQI_Normalized = sqrt(k - !!sym(aqi_column_name))
  )
```

This code plots our SQRT and SQRT reflected data. We first create the
histogram and colors. Second we create the axes Fig. 4. Normalization of
skewed data. We chose the SQRT method of normalization at the
recommendation of Dr. Saarman

``` r
# This chunk was troubleshooted with AI
ggplot(df, aes(x = Percent_Total)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Overall COPD Prevalence by State",
    x = "Overall COPD Rate (%)",
    y = "Number of States"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )
```

![](Jacob-Peterson-Final-markdown_files/figure-gfm/histogram%20creation-1.png)<!-- -->

``` r
ggplot(aq_df, aes(x = `AirQuality_AirQualityIndexViaUSA_num_YearFree`)) +
  # AQI values are clustered, so a smaller binwidth is better
  geom_histogram(binwidth = 2, fill = "lightgreen", color = "black") +
  labs(
    title = "Distribution of Overall AQI by State (2025)",
    # We can set a cleaner label for the x-axis
    x = "Overall Air Quality Index (AQI)",
    y = "Number of States"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )
```

![](Jacob-Peterson-Final-markdown_files/figure-gfm/histogram%20creation-2.png)<!-- -->

## Pearson test and GLM

This code runs our combined data through a pearson correlation test, and
a GLM model validate our data significance

``` r
#AI was not used in this chunk
# Run a Pearson's Correlation Test
total_corr_test <- cor.test(combined_df$AQI_Normalized, combined_df$Percent_Total_Norm)

print(total_corr_test)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  combined_df$AQI_Normalized and combined_df$Percent_Total_Norm
    ## t = -2.4511, df = 48, p-value = 0.01793
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.55989949 -0.06083131
    ## sample estimates:
    ##        cor 
    ## -0.3335316

``` r
# Run the Simple Linear Regression (lm)
total_lm_model <- lm(Percent_Total_Norm ~ AQI_Normalized, data = combined_df)

print(summary(total_lm_model))
```

    ## 
    ## Call:
    ## lm(formula = Percent_Total_Norm ~ AQI_Normalized, data = combined_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.80346 -0.19194 -0.06826  0.19992  0.90824 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     3.02207    0.18689  16.170   <2e-16 ***
    ## AQI_Normalized -0.14496    0.05914  -2.451   0.0179 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3281 on 48 degrees of freedom
    ## Multiple R-squared:  0.1112, Adjusted R-squared:  0.09273 
    ## F-statistic: 6.008 on 1 and 48 DF,  p-value: 0.01793

This code runs the same code as above between sexes

``` r
#AI was not used in this chunk

# Correlation for MALES
male_corr_test <- cor.test(combined_df$AQI_Normalized, combined_df$Percent_Male_Norm)
print(male_corr_test)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  combined_df$AQI_Normalized and combined_df$Percent_Male_Norm
    ## t = -2.0346, df = 48, p-value = 0.04743
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.519386051 -0.003718359
    ## sample estimates:
    ##        cor 
    ## -0.2817744

``` r
# Correlation for FEMALES
female_corr_test <- cor.test(combined_df$AQI_Normalized, combined_df$Percent_Female_Norm)
print(female_corr_test)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  combined_df$AQI_Normalized and combined_df$Percent_Female_Norm
    ## t = -2.6847, df = 48, p-value = 0.009937
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.58121237 -0.09224833
    ## sample estimates:
    ##        cor 
    ## -0.3613184

This code runs a GLM model against each sex to determine if there is
statistical significance between sexes

``` r
#This code was troubleshooted with AI

long_df <- combined_df %>%
  select(State, AQI_Normalized, Percent_Male_Norm, Percent_Female_Norm) %>%
  pivot_longer(
    cols = c("Percent_Male_Norm", "Percent_Female_Norm"),
    names_to = "Gender",
    values_to = "COPD_Norm"
  ) %>%
  mutate(Gender = str_replace(Gender, "Percent_", "")) %>%
  mutate(Gender = str_replace(Gender, "_Norm", ""))
gender_interaction_model <- lm(COPD_Norm ~ AQI_Normalized * Gender, data = long_df)

summary(gender_interaction_model)
```

    ## 
    ## Call:
    ## lm(formula = COPD_Norm ~ AQI_Normalized * Gender, data = long_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.89027 -0.21358 -0.05057  0.17768  0.93263 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                3.22954    0.19065  16.940  < 2e-16 ***
    ## AQI_Normalized            -0.17132    0.06033  -2.840  0.00551 ** 
    ## GenderMale                -0.43652    0.26961  -1.619  0.10872    
    ## AQI_Normalized:GenderMale  0.05610    0.08532   0.657  0.51245    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3347 on 96 degrees of freedom
    ## Multiple R-squared:  0.2218, Adjusted R-squared:  0.1975 
    ## F-statistic: 9.121 on 3 and 96 DF,  p-value: 2.273e-05

This code generates the final trendline plot and plots the data from the
GLM. Fig. 5 The graph visualizes the results from the Pearson
correlation and the GLM. In this plot it is evident that there is a
correlation between AQI and COPD rates

``` r
#This code chunk was troubleshooted and enchanced with AI
ggplot(combined_df, aes(x = AQI_Normalized, y = Percent_Total_Norm)) +
  geom_point(color = "black", alpha = 0.6, size = 2.5) +
  
  # Add the regression line
  geom_smooth(method = "lm", color = "darkblue", fill = "lightblue") +
  
  labs(
    title = "Overall Relationship: Air Quality vs. COPD Rates",
    x = "Normalized AQI Score \n(← Worse Air Quality | Better Air Quality →)",
    y = "Normalized Total COPD Rate"
  ) +
  
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(color = "black")
  )
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Jacob-Peterson-Final-markdown_files/figure-gfm/final%20plot-1.png)<!-- -->

# Discussion

Our primary analysis revealed a strong statistical correlation between
AQI and COPD rates (p=0.0179). Rejecting the null hypothesis and
confirming our hypothesis. This confirms states with worse air average
air quality exibit higher rates of COPD. While the relation ship is
signifigant, the effect size is moderate. Our model found an R^2 of
.1112, meaning AQI contributes to roughly 11.1% of COPD rates. This
suggests that unaccounted, confounding factors (smoking, occupation,
genetics) are likely a larger factor.

We further investigated the impact of AQI on the biological sexes. While
the Pearson correlation test suggested a slightly stronger relationship
for females (p = 0.0099) than for males (p = 0.0474), our generalized
linear model revealed that this difference was not statistically
significant. The interaction term between gender and air quality yielded
a p-value of 0.512, indicating that the slopes of the regression lines
for males and females are statistically indistinguishable. This suggests
that poor air quality is an equal-opportunity risk factor; as air
quality worsens, COPD rates rise at similar trajectories for both sexes.

Limitations:

Firstly, the use of aggregate level state data may not accurately
reflect the average air quality each individual experienced. In Utah for
example, the majority of the bad air quality is trapped within the two
major valley centers, while the population that does not live within the
valleys experience far better air quality. Second, migration likely
plays a large factor. An individual could develop COPD in a state with
poor AQI and migrate to a state with better AQI, thus skewing data.

# CONCLUSION

According to our data, there is a correlation between areas with a low
air quality index number having higher rates of Chronic Obstructive
Pulmonary Disease. Knowing this, we can help inform people in areas of
higher AQI to take the necessary precautions to avoid COPD. This
includes practices such as reducing time outdoors during times of higher
AQI, having proper filters in their households, and limiting use of
machinery that contributes to the higher AQI. Having this knowledge
could be extremely beneficial to lowering the rates of COPD in the US
and could help people get back to the life they deserve.

# REFERENCES

American Lung Association. (2023). COPD prevalence rates and counts by
state and gender. Retrieved November 18, 2025, from
<https://www.lung.org/research/trends-in-lung-disease/copd-trends-brief/data-tables/copd-prevalence-rates-by-state-gender>

Duan, R. R., Hao, K., & Yang, T. (2020). Air pollution and chronic
obstructive pulmonary disease. Chronic Diseases and Translational
Medicine, 6(4), 260–269. <https://doi.org/10.1016/j.cdtm.2020.05.004>

Google. (2025). Gemini (Version Jan 2025) \[Large language model\].
Accessed 2025-11-20.

OpenAI. (2025). ChatGPT (Jan 2025 version) \[Large language model\].
Accessed 2025-11-20.

World Population Review. (2025). Air Quality by State 2025.
<https://worldpopulationreview.com/state-rankings/air-quality-by-state>
