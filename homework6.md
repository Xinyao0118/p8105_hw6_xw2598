Homework6
================
Xinyao Wu
2018/11/16

``` r
knitr::opts_chunk$set(
  fig.width = 12,
  fig.asp = .6,
  out.width = "100%"
)
```

problem1
--------

**Washington Post**

##### (1) Tidy Data

*disposition: new binary variable*

*delete records:Dallas, TX; Phoenix, AZ; and Kansas City, MO*

*exhibit head 5 lines of dataset*

``` r
Original_data = read.csv("https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv") 
wp_df = 
  Original_data %>% 
  janitor::clean_names() %>% 
  #Create a city_state variable and a binary variable indicating whether the homicide is solved.
  mutate(
    city_state = str_c(city,state, sep = ", "), 
    resolved = as.numeric(disposition == "Closed by arrest")
  ) %>% 
  #Omit cities Dallas, TX; Phoenix, AZ; and Kansas City, MO
  filter(
    city_state != "Dallas, TX" & city_state !="Phoenix, AZ" & city_state !="Kansas City, MO" & city_state !="Tulsa, AL"
  ) %>% 
  mutate(
    victim_race = ifelse(victim_race =="White","white","non_white"),
    victim_race = fct_relevel(victim_race, "white"),
    victim_age = as.numeric(victim_age)
  )
wp_df %>% head(5) 
```

    ##          uid reported_date victim_last victim_first victim_race victim_age
    ## 1 Alb-000001      20100504      GARCIA         JUAN   non_white         79
    ## 2 Alb-000002      20100216     MONTOYA      CAMERON   non_white         12
    ## 3 Alb-000003      20100601 SATTERFIELD      VIVIANA       white         10
    ## 4 Alb-000004      20100101    MENDIOLA       CARLOS   non_white         29
    ## 5 Alb-000005      20100102        MULA       VIVIAN       white         73
    ##   victim_sex        city state      lat       lon           disposition
    ## 1       Male Albuquerque    NM 35.09579 -106.5386 Closed without arrest
    ## 2       Male Albuquerque    NM 35.05681 -106.7153      Closed by arrest
    ## 3     Female Albuquerque    NM 35.08609 -106.6956 Closed without arrest
    ## 4       Male Albuquerque    NM 35.07849 -106.5561      Closed by arrest
    ## 5     Female Albuquerque    NM 35.13036 -106.5810 Closed without arrest
    ##        city_state resolved
    ## 1 Albuquerque, NM        0
    ## 2 Albuquerque, NM        1
    ## 3 Albuquerque, NM        0
    ## 4 Albuquerque, NM        1
    ## 5 Albuquerque, NM        0

##### (2)"Baltimore, MD": use the glm function to fit a logistic regression with resolved vs unresolved as the outcome and victim age, sex and race (as just defined) as predictors.

*1)Save the output of glm as an R object *

*2)apply the broom::tidy to this object*

*3)obtain the estimate and confidence interval of the adjusted odds ratio for solving homicides comparing non-white victims to white victims keeping all other variables fixed.*

``` r
#For the city of Baltimore, MD
baltimore = wp_df %>% 
  filter(city_state =="Baltimore, MD") 
#fit a logistic regression
fit_baltimore = glm(resolved ~ victim_age + victim_sex + victim_race , family = binomial(), data = baltimore)

broom::tidy(fit_baltimore)
## # A tibble: 4 x 5
##   term                 estimate std.error statistic  p.value
##   <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)           1.05      0.227        4.62 3.78e- 6
## 2 victim_age           -0.00374   0.00303     -1.23 2.17e- 1
## 3 victim_sexMale       -0.885     0.136       -6.50 8.08e-11
## 4 victim_racenon_white -0.793     0.174       -4.55 5.33e- 6
#the estimate of the adjusted odds ratio for solving homicides comparing non-white victims to white victims
coef(fit_baltimore)["victim_racenon_white"] %>% exp()
## victim_racenon_white 
##            0.4525206
#the confidence interval of the adjusted odds ratio for solving homicides comparing non-white victims to white victims
confint(fit_baltimore,"victim_racenon_white")%>% exp()
## Waiting for profiling to be done...
##     2.5 %    97.5 % 
## 0.3208675 0.6359593
```

The estimate of the adjusted odds ratio for solving homicides comparing non-white victims to white victims is 0.453, and the 95% CI is (0.321,0.636).

##### (3)Now run glm for each of the cities in your dataset, and extract the adjusted odds ratio (and CI) for solving homicides comparing non-white victims to white victims.

Do this within a “tidy” pipeline, making use of purrr::map, list columns, and unnest as necessary to create a dataframe with estimated ORs and CIs for each city.

``` r
 city_glm =
  wp_df %>% 
  select(resolved, city_state, victim_race,victim_age,victim_sex) %>% 
  group_by(city_state) %>% 
  nest() %>% 
  mutate(
    model = map(data,~glm(resolved ~ victim_age + victim_sex + victim_race ,family = binomial(),data = .)),
    model = map(model, ~broom::tidy(.,conf.int = TRUE))
)  %>% 
   select(-data) %>% 
  unnest() %>% 
  filter(term == "victim_racenon_white")  %>% 
  mutate(OR = exp(estimate),
         CI.low = exp(conf.low),
         CI.high = exp(conf.high)) %>% 
  select(city_state,OR,CI.low ,CI.high ) 

 knitr::kable(head(city_glm,5))
```

| city\_state     |         OR|     CI.low|    CI.high|
|:----------------|----------:|----------:|----------:|
| Albuquerque, NM |  0.6860531|  0.4158651|  1.1237789|
| Atlanta, GA     |  0.7667500|  0.4332108|  1.3204368|
| Baltimore, MD   |  0.4525206|  0.3208675|  0.6359593|
| Baton Rouge, LA |  0.6558545|  0.2991473|  1.3796610|
| Birmingham, AL  |  1.0471153|  0.6194345|  1.7589481|

##### (5)Create a plot that shows the estimated ORs and CIs for each city. Organize cities according to estimated OR, and comment on the plot.

``` r
city_glm %>% 
  mutate(
    city_state = reorder(city_state,desc(OR))
  ) %>% 
ggplot(aes(x = city_state, y = OR))+ geom_line()+
  geom_point()+
  geom_errorbar(aes(x= city_state,ymin = CI.low, ymax = CI.high))+
  labs(
    x = "City, State",
    y = "Adjusted Odd ratio ",
    title = "odd ratio of homocides solving comparing non-white to white victims in United States"
  )+
  theme(
    axis.text.x = element_text(angle = 45 , hjust = 1)
  )
```

    ## geom_path: Each group consists of only one observation. Do you need to
    ## adjust the group aesthetic?

<img src="homework6_files/figure-markdown_github/unnamed-chunk-5-1.png" width="100%" />

Comment:
--------

Most cities who have a higher OR are tending to have a wider OR confidence interval.`Tampa,Fl`,`Durham,NC`,`Birmingham,AL` these three cities have a OR higher than 1, which indicates in these cities, homicides with non-white victims are more likely to be solved. However, since they all have a wide 95% confidence interval, which include point 1, there still exists some probability that homicides with non-white victims are equal or less likely to be solved.

problem2
--------

Load and clean the data for regression analysis (i.e. convert numeric to factor where appropriate, check for missing data, etc.).

``` r
orignal_bw = read.csv("./data/birthweight.csv")
bw_df = orignal_bw %>% 
  janitor::clean_names() %>% 
  #convert numeric to factor where appropriate
  mutate(
    babysex = as.factor(ifelse(babysex==1,"male","female")),
    frace = as.factor(frace),
    malform = as.factor(malform),
    mrace = as.factor(mrace),
  )
#check for missing data
table(is.na(bw_df))
```

    ## 
    ## FALSE 
    ## 86840

Propose a regression model for birthweight. This model may be based on a hypothesized structure for the factors that underly birthweight, on a data-driven model-building process, or a combination of the two. **Describe your modeling process** and show a plot of model residuals against fitted values – use add\_predictions and add\_residuals in making this plot.

``` r
reg = bw_df %>% 
 select(bwt,babysex, bhead, blength) 
#the distribution of baby’s birth weight
ggplot(reg,aes(x = bwt))+geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

<img src="homework6_files/figure-markdown_github/unnamed-chunk-7-1.png" width="100%" />

``` r
#likely normal
#make a linear regression
  fit_self = lm(bwt~babysex + bhead + blength , data = reg)
  summary(fit_self)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength, data = reg)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1138.45  -190.36   -11.59   177.64  2693.95 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6080.305     96.224 -63.189  < 2e-16 ***
    ## babysexmale   -41.086      8.887  -4.623 3.89e-06 ***
    ## bhead         148.175      3.512  42.192  < 2e-16 ***
    ## blength        85.016      2.071  41.044  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 288.5 on 4338 degrees of freedom
    ## Multiple R-squared:  0.6829, Adjusted R-squared:  0.6827 
    ## F-statistic:  3114 on 3 and 4338 DF,  p-value: < 2.2e-16

``` r
 pred_resid = modelr::add_predictions(reg, fit_self) 
 pred_resid$resid = modelr::add_residuals(reg, fit_self) %>% pull(resid)
 
  ggplot(pred_resid,aes(x = pred, y = resid))+
  geom_point()+
    labs(
      title = "predictions vs residuals of the specific linear regression model",
      y = "residual",
      x = "prediction"
    )
```

<img src="homework6_files/figure-markdown_github/unnamed-chunk-7-2.png" width="100%" />

comments
--------

lm(bwt ~ babysex + bhead + blength + mrace + parity)

Compare your model to two others:
---------------------------------

\*cv set

``` r
cv_df =
  crossv_mc(bw_df, 100) %>% 
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble))
```

``` r
cv_df = cv_df %>% 
  mutate(
    non_nest_mod = map(train,~lm(bwt~blength + gaweeks,data = .x)),
    nest_mod = map(train,~lm(bwt~bhead * blength * babysex,data = .x)),
    self_mod = map(train,~lm(bwt~babysex + bhead + blength,data = .x))
  ) %>% 
  mutate(
    rmse_non_nest   = map2_dbl(non_nest_mod, test, ~rmse(model = .x, data = .y)),
    rmse_nest   = map2_dbl(nest_mod, test, ~rmse(model = .x, data = .y)),
    rmse_self   = map2_dbl(self_mod, test, ~rmse(model = .x, data = .y))
  )

cv_df %>% 
  select(starts_with("rmse")) %>% 
  gather(key = "model",value = rmse) %>%
  mutate(
    model = str_replace(model,"rmse_",""),
    model = fct_inorder(model)
         ) %>% 
  ggplot(aes(x = model, y = rmse))+geom_violin()+
  labs(
    title = "Rmse distribution in three different model cross-validations"
  )
```

<img src="homework6_files/figure-markdown_github/unnamed-chunk-9-1.png" width="100%" />

One using length at birth and gestational age as predictors (main effects only) One using head circumference, length, sex, and all interactions (including the three-way interaction) between these

Make this comparison in terms of the cross-validated prediction error; use crossv\_mc and functions in purrr as appropriate.
