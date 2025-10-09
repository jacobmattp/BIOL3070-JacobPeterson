Warm-up mini-Report: Mosquito Blood Hosts in Salt Lake City, Utah
================
Jacob Peterson
10/8/2025

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [STUDY QUESTION and HYPOTHESIS](#study-question-and-hypothesis)
  - [Questions](#questions)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
- [METHODS](#methods)
  - [Fill in 1st analysis
    e.g. barplots](#fill-in-1st-analysis-eg-barplots)
  - [Fill in 2nd analysis/plot e.g. generalized linear
    model](#fill-in-2nd-analysisplot-eg-generalized-linear-model)
- [DISCUSSION](#discussion)
  - [Interpretation of 1st analysis
    (e.g. barplots)](#interpretation-of-1st-analysis-eg-barplots)
  - [Interpretation of 2nd analysis (e.g. generalized linear
    model)](#interpretation-of-2nd-analysis-eg-generalized-linear-model)
- [REFERENCES](#references)

# ABSTRACT

West Nile Virus infects hundreds of Americans per year, causing death in
extreme cases. Controlling this virus allows us to implement prevention
methods that are easily transferrable to other areas of the world where
the virus is severe. This study examines carriers of West Nile Virus. We
hypothesized that the most common birds will be the most common
carriers. In this case, around Salt Lake City Utah, the house finch
Haemorhous mexicanus is the most common bird. Therefore the finch will
be the most common carrier. Our data supports our hypothesis with the
house finch being the species with the most amount of positive bloodmeal
tests from captured mosquitos.

# BACKGROUND

West Nile virus (WNV) is most commonly spread to people through the bite
of an infected mosquito. Despite the name, hundreds of people are
infected in the USA every year. Pathology includes body aches, fever and
diarrhea. Severe cases can include high fever, headache, neck stiffness,
stupor, disorientation, coma, tremors, convulsions, muscle weakness,
vision loss, numbness, or paralysis. Recovery from severe illness can
take several weeks or months. Some effects may be permanent. Data was
collected from the Saarman lab, and sequenced in the BIOL 3070 class as
demonstration.

``` r
# Manually transcribe duration (mean, lo, hi) from the last table column
duration <- data.frame(
  Bird = c("Canada Goose","Mallard", 
           "American Kestrel","Northern Bobwhite",
           "Japanese Quail","Ring-necked Pheasant",
           "American Coot","Killdeer",
           "Ring-billed Gull","Mourning Dove",
           "Rock Dove","Monk Parakeet",
           "Budgerigar","Great Horned Owl",
           "Northern Flicker","Blue Jay",
           "Black-billed Magpie","American Crow",
           "Fish Crow","American Robin",
           "European Starling","Red-winged Blackbird",
           "Common Grackle","House Finch","House Sparrow"),
  mean = c(4.0,4.0,4.5,4.0,1.3,3.7,4.0,4.5,5.5,3.7,3.2,2.7,1.7,6.0,4.0,
           4.0,5.0,3.8,5.0,4.5,3.2,3.0,3.3,6.0,4.5),
  lo   = c(3,4,4,3,0,3,4,4,4,3,3,1,0,6,3,
           3,5,3,4,4,3,3,3,5,2),
  hi   = c(5,4,5,5,4,4,4,5,7,4,4,4,4,6,5,
           5,5,5,7,5,4,3,4,7,6)
)

# Choose some colors
cols <- c(rainbow(30)[c(10:29,1:5)])  # rainbow colors

# horizontal barplot
par(mar=c(5,12,2,2))  # wider left margin for names
bp <- barplot(duration$mean, horiz=TRUE, names.arg=duration$Bird,
              las=1, col=cols, xlab="Days of detectable viremia", xlim=c(0,7))

# add error bars
arrows(duration$lo, bp, duration$hi, bp,
       angle=90, code=3, length=0.05, col="black", xpd=TRUE)
```

<img src="Warmup-RMD-Jacob-Peterson_files/figure-gfm/viremia-1.png" style="display: block; margin: auto auto auto 0;" />

# STUDY QUESTION and HYPOTHESIS

## Questions

This study seeks to determine which species are the largest risk factor
for spreading WNV.

## Hypothesis

We believe the common house finch, Haemorhous mexicanus, is the most
common carrier for WNV.

## Prediction

We predict that H. mexicanus will carry the largest porportion of
bloodmeals tested positive for WNV

# METHODS

Gravid and CO2 traps were placed across Salt Lake City UT. Samples were
collected and sequenced with PCR DNA samples were processed using the
MINion device. Sequenced DNA were ran through the BLAST database to
identify carriers. Code was created with the help of Dr Saarman and
ChatGPT.

## Fill in 1st analysis e.g. barplots

We compared number of blood meals from each species between two sites.
Sites that had one or more positive WNV and no positive WNV. A grouped
barplot is the best choice because it visually compares the numerical
counts of blood meals across two separate categorical groups: the host
species and the WNV site status. This side by side clustering makes it
immediately clear whether any specific host species is fed upon
significantly more frequently at WNV-Positive sites than at WNV-Negative
sites. The method is superior for this data because it allows for direct
magnitude comparison between the two critical disease environments.

``` r
## import counts_matrix: data.frame with column 'loc_positives' (0/1) and host columns 'host_*'
counts_matrix <- read.csv("./bloodmeal_plusWNV_for_BIOL3070.csv")

## 1) Identify host columns
host_cols <- grep("^host_", names(counts_matrix), value = TRUE)

if (length(host_cols) == 0) {
  stop("No columns matching '^host_' were found in counts_matrix.")
}

## 2) Ensure loc_positives is present and has both levels 0 and 1 where possible
counts_matrix$loc_positives <- factor(counts_matrix$loc_positives, levels = c(0, 1))

## 3) Aggregate host counts by loc_positives
agg <- stats::aggregate(
  counts_matrix[, host_cols, drop = FALSE],
  by = list(loc_positives = counts_matrix$loc_positives),
  FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
)

## make sure both rows exist; if one is missing, add a zero row
need_levels <- setdiff(levels(counts_matrix$loc_positives), as.character(agg$loc_positives))
if (length(need_levels)) {
  zero_row <- as.list(rep(0, length(host_cols)))
  names(zero_row) <- host_cols
  for (lv in need_levels) {
    agg <- rbind(agg, c(lv, zero_row))
  }
  ## restore proper type
  agg$loc_positives <- factor(agg$loc_positives, levels = c("0","1"))
  ## coerce numeric host cols (they may have become character after rbind)
  for (hc in host_cols) agg[[hc]] <- as.numeric(agg[[hc]])
  agg <- agg[order(agg$loc_positives), , drop = FALSE]
}

## 4) Decide species order (overall abundance, descending)
overall <- colSums(agg[, host_cols, drop = FALSE], na.rm = TRUE)
host_order <- names(sort(overall, decreasing = TRUE))
species_labels <- rev(sub("^host_", "", host_order))  # nicer labels

## 5) Build count vectors for each panel in the SAME order
counts0 <- rev(as.numeric(agg[agg$loc_positives == 0, host_order, drop = TRUE]))
counts1 <- rev(as.numeric(agg[agg$loc_positives == 1, host_order, drop = TRUE]))

## 6) Colors: reuse your existing 'cols' if it exists and is long enough; otherwise generate
if (exists("cols") && length(cols) >= length(host_order)) {
  species_colors <- setNames(cols[seq_along(host_order)], species_labels)
} else {
  species_colors <- setNames(rainbow(length(host_order) + 10)[seq_along(host_order)], species_labels)
}

## 7) Shared x-limit for comparability
xmax <- max(c(counts0, counts1), na.rm = TRUE)
xmax <- if (is.finite(xmax)) xmax else 1
xlim_use <- c(0, xmax * 1.08)

## 8) Plot: two horizontal barplots with identical order and colors
op <- par(mfrow = c(1, 2),
          mar = c(4, 12, 3, 2),  # big left margin for species names
          xaxs = "i")           # a bit tighter axis padding

## Panel A: No WNV detected (loc_positives = 0)
barplot(height = counts0,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (-)",
        xlim = xlim_use)

## Panel B: WNV detected (loc_positives = 1)
barplot(height = counts1,
        names.arg = species_labels, 
        cex.names = .5,
        cex.axis = .5,
        col = rev(unname(species_colors[species_labels])),
        horiz = TRUE,
        las = 1,
        xlab = "Bloodmeal counts",
        main = "Locations WNV (+)",
        xlim = xlim_use)
```

![](Warmup-RMD-Jacob-Peterson_files/figure-gfm/first-analysis-1.png)<!-- -->

``` r
par(op)

## Keep the colors mapping for reuse elsewhere
host_species_colors <- species_colors
```

## Fill in 2nd analysis/plot e.g. generalized linear model

While the visual comparison offered by barplots provides an intuitive
initial assessment, moving from visual suggestion to a scientific
conclusion requires formal statistical rigor. To quantify the
hypothesized role of the House Finch in the West Nile Virus (WNV) cycle,
we employed regression analysis to test its predictive power against key
measures of viral activity. The core of this analysis focused on whether
the presence or increasing count of House Finch blood meals could
reliably predict two distinct WNV outcomes: first, the binary
determination of whether a site registered as WNV-positive; and second,
the numeric measure of the overall WNV positivity rate observed at that
site.

This statistical test served the vital function of formally evaluating
the relationship that was qualitatively suggested by the barplots. By
generating objective coefficients and p-values, the analysis allowed us
to rigorously confirm the strength and direction of the association,
thereby transforming the observed differences in feeding frequency into
a statistically validated measure of the House Finch’s contribution to
WNV transmission risk.

``` r
# second-analysis-or-plot, glm with house finch alone against binary +/_
glm1 <- glm(loc_positives ~ host_House_finch,
            data = counts_matrix,
            family = binomial)
summary(glm1)
```

    ## 
    ## Call:
    ## glm(formula = loc_positives ~ host_House_finch, family = binomial, 
    ##     data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)       -0.1709     0.1053  -1.622   0.1047  
    ## host_House_finch   0.3468     0.1586   2.187   0.0287 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 546.67  on 394  degrees of freedom
    ## Residual deviance: 539.69  on 393  degrees of freedom
    ## AIC: 543.69
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
#glm with house-finch alone against positivity rate
glm2 <- glm(loc_rate ~ host_House_finch,
            data = counts_matrix)
summary(glm2)
```

    ## 
    ## Call:
    ## glm(formula = loc_rate ~ host_House_finch, data = counts_matrix)
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.054861   0.006755   8.122 6.07e-15 ***
    ## host_House_finch 0.027479   0.006662   4.125 4.54e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.01689032)
    ## 
    ##     Null deviance: 6.8915  on 392  degrees of freedom
    ## Residual deviance: 6.6041  on 391  degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## AIC: -484.56
    ## 
    ## Number of Fisher Scoring iterations: 2

# DISCUSSION

Our analysis identifies Haemorhous mexicanus as the primary avian
reservoir for West Nile Virus within the Salt Lake City metropolitan
area. This conclusion is strongly supported by multiple data
visualizations; bar plot analysis indicates that House Finches account
for the highest number of WNV cases, a finding corroborated by our
linear regression model. The prevalence of WNV in this species appears
to be directly proportional to its high population density in the study
region. This correlation suggests that WNV infection rates among local
bird species are largely a function of their relative abundance, rather
than a species-specific susceptibility.

## Interpretation of 1st analysis (e.g. barplots)

House finches were both the most common birds and most common carrier of
WNV. This supports our hypothesis.

## Interpretation of 2nd analysis (e.g. generalized linear model)

The generalized linear models quantify the relationship suggested by the
bar plots, confirming that the association between House Finches and
West Nile Virus is statistically significant. Two separate models were
run to test this relationship:

Predicting WNV Presence/Absence (glm1): This first model (a logistic
regression) tested whether the number of House Finch bloodmeals could
predict if a location was WNV-positive or negative. The results show a
positive and statistically significant relationship (coefficient =
0.3468, p-value = 0.0287). This means that as the number of House Finch
bloodmeals increases, the probability of a site testing positive for WNV
also significantly increases.

Predicting WNV Infection Rate (glm2): The second model tested if the
number of House Finch bloodmeals could predict the actual infection rate
(loc_rate) at a site. This model also found a positive and highly
statistically significant relationship (coefficient = 0.027479, p-value
= 4.54e-05). This result indicates that for every additional House Finch
bloodmeal found, the WNV infection rate is predicted to rise by
approximately 0.027. \# CONCLUSION

The evidence from this study strongly supports our initial hypothesis.
The house finch is the most common carrier for West Nile Virus in the
Salt Lake City area. This was demonstrated via a statistical test and
analysis of the data. These findings suggest that the high population
density of House Finches is a key factor driving WNV amplification and
transmission risk in this urban ecosystem.

# REFERENCES

1.  Komar N, Langevin S, Hinten S, Nemeth N, Edwards E, Hettler D, Davis
    B, Bowen R, Bunning M. Experimental infection of North American
    birds with the New York 1999 strain of West Nile virus. Emerg Infect
    Dis. 2003 Mar;9(3):311-22. <https://doi.org/10.3201/eid0903.020628>

2.  ChatGPT. OpenAI, version Jan 2025. Used as a reference for functions
    such as plot() and to correct syntax errors. Accessed 2025-10-09.

3.  3.  <https://www.cdc.gov/west-nile-virus/about/index.html>
