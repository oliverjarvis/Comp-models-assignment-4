Assignment 4
------------

### Step 1: Perform a meta-analysis of pitch variability from previous studies of voice in ASD

### Step 2: Analyse pitch variability in ASD in two new studies for which you have access to all the trials (not just study level estimates)

``` r
data <- read_csv("Ass4_data.csv", col_types = cols(ID = col_character()))

data <- data %>% mutate(
  PitchVariability = scale(Pitch_IQR)
)

hist(data$Pitch_IQR)
```

![](Assignment4_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
hist(data$PitchVariability)
```

![](Assignment4_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
#Looks shifted log normal, but gaussian is pretty close
```

### Step 3: Build a regression model predicting Pitch variability from Diagnosis.

Using uninformed, conservative priors

``` r
NewStudies_f0 <- bf(PitchVariability ~ 1 + Diagnosis + (1|ID))

get_prior(NewStudies_f0, data, family = gaussian())
```

    ##                 prior     class        coef group resp dpar nlpar bound
    ## 1                             b                                        
    ## 2                             b DiagnosisTD                            
    ## 3 student_t(3, 0, 10) Intercept                                        
    ## 4 student_t(3, 0, 10)        sd                                        
    ## 5                            sd                ID                      
    ## 6                            sd   Intercept    ID                      
    ## 7 student_t(3, 0, 10)     sigma

``` r
sd(data$PitchVariability)
```

    ## [1] 1

``` r
NS_prior0 <- c(
  prior(normal(0, .3), class = Intercept),
  prior(normal(0, .2), class = b),
  prior(normal(0, .2), class = sd),
  prior(normal(.5, .3), class = sigma)
)

NS_m0_pc <- brm(
  NewStudies_f0,
  data,
  family = gaussian(),
  prior = NS_prior0,
  sample_prior = "only",
  chains = 2,
  cores = 2,
  seed = seed,
  file="NS_m0_pc_f"
)

pp_check(NS_m0_pc, nsamples=100)
```

![](Assignment4_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
NS_m0 <- brm(
  NewStudies_f0,
  data,
  family = gaussian(),
  prior = NS_prior0,
  sample_prior = T,
  chains = 2,
  cores = 2,
  #seed = seed,
  file="NS_m0_malate_f"
)
pp_check(NS_m0, nsamples=100)
```

![](Assignment4_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
#Because we have seen in the meta analysis, that there is less variance in TD
plot(hypothesis(NS_m0, "DiagnosisTD < 0"))
```

![](Assignment4_files/figure-markdown_github/unnamed-chunk-3-3.png)

``` r
hypothesis(NS_m0, "DiagnosisTD < 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (DiagnosisTD) < 0    -0.13      0.11     -0.3     0.04       8.62       0.9
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
summary(NS_m0)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: PitchVariability ~ 1 + Diagnosis + (1 | ID) 
    ##    Data: data (Number of observations: 1074) 
    ## Samples: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 2000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 149) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.73      0.05     0.64     0.82 1.00      587      979
    ## 
    ## Population-Level Effects: 
    ##             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept       0.29      0.08     0.12     0.45 1.01      383      742
    ## DiagnosisTD    -0.13      0.11    -0.34     0.08 1.00      429      820
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.70      0.02     0.67     0.73 1.00     2540     1591
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
plot(NS_m0)
```

![](Assignment4_files/figure-markdown_github/unnamed-chunk-3-4.png)

``` r
NS_m0 <- add_criterion(NS_m0, criterion = "loo", reloo = T)
```

    ## 4 problematic observation(s) found.
    ## The model will be refit 4 times.

    ## 
    ## Fitting model 1 out of 4 (leaving out observation 125)

    ## 
    ## Fitting model 2 out of 4 (leaving out observation 639)

    ## 
    ## Fitting model 3 out of 4 (leaving out observation 640)

    ## 
    ## Fitting model 4 out of 4 (leaving out observation 770)

    ## Start sampling
    ## Start sampling
    ## Start sampling
    ## Start sampling

    ## Automatically saving the model object in 'NS_m0_malate_f.rds'

### Step 4: Now re-run the model with the meta-analytic prior

``` r
NS_informed_prior0 <- c(
  prior(normal(.0, .3), class = Intercept),
  prior(normal(-0.4528398, 0.2), class = b),
  prior(normal(0, .2), class = sd),
  prior(normal(.32, .3), class = sigma)
)


NS_informed_m0_pc <- brm(
  NewStudies_f0,
  data,
  family = gaussian(),
  prior = NS_informed_prior0,
  sample_prior = "only",
  chains = 2,
  cores = 2,
  #seed = seed,
  file="NS_in_pc_f"
)

pp_check(NS_informed_m0_pc, nsamples = 100)
```

![](Assignment4_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
NS_informed_m0 <- brm(
  NewStudies_f0,
  data,
  family = gaussian(),
  prior = NS_informed_prior0,
  sample_prior = T,
  chains = 2,
  cores = 2,
  #seed=seed,
  file="NS_informed_m0_malte_f"
)

pp_check(NS_informed_m0, nsamples = 100)
```

![](Assignment4_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
plot(hypothesis(NS_informed_m0, "DiagnosisTD < 0"))
```

![](Assignment4_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
hypothesis(NS_informed_m0, "DiagnosisTD < 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (DiagnosisTD) < 0    -0.27      0.11    -0.45    -0.09         99      0.99
    ##   Star
    ## 1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
summary(NS_informed_m0)
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: PitchVariability ~ 1 + Diagnosis + (1 | ID) 
    ##    Data: data (Number of observations: 1074) 
    ## Samples: 2 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 2000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 149) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.73      0.05     0.64     0.82 1.00      572     1034
    ## 
    ## Population-Level Effects: 
    ##             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept       0.36      0.08     0.20     0.52 1.00      502      888
    ## DiagnosisTD    -0.27      0.11    -0.48    -0.06 1.00      614     1016
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.70      0.02     0.67     0.73 1.00     3439     1633
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
NS_informed_m0 <- add_criterion(NS_informed_m0, criterion = "loo", reloo = T)
```

    ## 2 problematic observation(s) found.
    ## The model will be refit 2 times.

    ## 
    ## Fitting model 1 out of 2 (leaving out observation 244)

    ## 
    ## Fitting model 2 out of 2 (leaving out observation 640)

    ## Start sampling
    ## Start sampling

    ## Automatically saving the model object in 'NS_informed_m0_malte_f.rds'

### Step 5: Compare the models

``` r
loo_model_weights(NS_m0, NS_informed_m0)
```

    ## Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.

    ## Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.

    ## Method: stacking
    ## ------
    ##                weight
    ## NS_m0          0.000 
    ## NS_informed_m0 1.000

``` r
plot(hypothesis(NS_m0, "DiagnosisTD < 0"))
```

![](Assignment4_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
plot(hypothesis(NS_informed_m0, "DiagnosisTD < 0"))
```

![](Assignment4_files/figure-markdown_github/unnamed-chunk-5-2.png)
