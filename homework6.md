Homework6
================
Xinyao Wu
2018/11/16

problem1
--------

**Washington Post**

Create a city\_state variable (e.g. “Baltimore, MD”), and a binary variable indicating whether the homicide is solved. Omit cities Dallas, TX; Phoenix, AZ; and Kansas City, MO – these don’t report victim race. Also omit Tulsa, AL – this is a data entry mistake. Modifiy victim\_race to have categories white and non-white, with white as the reference category. Be sure that victim\_age is numeric.

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

For the city of Baltimore, MD, use the glm function to fit a logistic regression with resolved vs unresolved as the outcome and victim age, sex and race (as just defined) as predictors. **Save the output of glm as an R object; ** apply the broom::tidy to this object; and **obtain the estimate and confidence interval of the adjusted odds ratio for solving homicides comparing non-white victims to white victims keeping all other variables fixed.**

``` r
#For the city of Baltimore, MD
baltimore = wp_df %>% 
  filter(city_state =="Baltimore, MD") 
#fit a logistic regression
fit_baltimore = glm(resolved ~ victim_age + victim_sex + victim_race , family = binomial(), data = baltimore)

broom::tidy(fit_baltimore)
```

    ## # A tibble: 4 x 5
    ##   term                 estimate std.error statistic  p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)           1.05      0.227        4.62 3.78e- 6
    ## 2 victim_age           -0.00374   0.00303     -1.23 2.17e- 1
    ## 3 victim_sexMale       -0.885     0.136       -6.50 8.08e-11
    ## 4 victim_racenon_white -0.793     0.174       -4.55 5.33e- 6

``` r
#the estimate of the adjusted odds ratio for solving homicides comparing non-white victims to white victims
coef(fit_baltimore)["victim_racenon_white"]
```

    ## victim_racenon_white 
    ##            -0.792922

``` r
#the confidence interval of the adjusted odds ratio for solving homicides comparing non-white victims to white victims
confint(fit_baltimore,"victim_racenon_white")
```

    ## Waiting for profiling to be done...

    ##      2.5 %     97.5 % 
    ## -1.1367271 -0.4526207

problem2
--------
