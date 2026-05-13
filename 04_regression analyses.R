
#---- 02. Regressions -----
## Comprehensive list of all the variables that will be used in analyses
## Modify here if necessary to ensure consistency throughout
  indep_demo = c("GenderFemale", "Age", "Marital_3L", "Ethnicity_4L", "Education_3L", "Housing", "Employed", "Income_4L", "Religion_5L", "Sibling_4L") # "Citizen" - should not include
  indep_care = c("IADL", "ADL", "lowstakeMed") # "IADL_HelpDigital" -> REMOVED POST R1 AS ALREADY INCLUDED AS PART OF IADL
  indep_care_num = c("help_physical", "help_finance", "help_medical") # ADDED POST R1 TO REFLECT THE EXTENT OF CAREGIVING EXPERIENCE
  indep_delegation = c("FinanceDelegate2", "MedicalDelegate2")
  indep_LPA = c("FinanceLPA")
  dep_var = c("highstakeMed_ever", "highstakeMed_count", "highstakeMed_domain_count")
  
  # Test for missing data
    'test_numeric <- df_IADL_us %>%
      dplyr::select(indep_demo, indep_care, indep_delegation, dep_var,indep_LPA)
    md.pattern(test_numeric)
    
    md.pattern(df_IADL_us[,indep_demo])
    md.pattern(df_IADL_us[,indep_care])
    md.pattern(df_IADL_us[,indep_delegation])
    md.pattern(df_IADL_us[,dep_var])
    '

  # Independent variables
    temp <- paste(c(indep_demo, indep_care, indep_care_num, indep_delegation, indep_LPA),
                  collapse = " + ")
    
## Loop the same analyses over 2 country datasets
    datasets <- list(
      US = df_IADL_us#,
      # SG = df_IADL_sg
    )
    
    # Loop over datasets
    for (country in names(datasets)) {
      
      df <- datasets[[country]]
      df <- droplevels(df) #remove unused levels (especially among ethnicity levels)
      
#==== 02.1  Completed any of the SDM items =============
#**** Dependent variable: highstakeMed_ever_bin ******** 

    df$highstakeMed_ever_bin <- as.numeric(df$highstakeMed_ever) - 1 # OLS and logit does not work on factors
  
  # OLS 
        model1_ever_ols <- glm(as.formula(paste("highstakeMed_ever_bin","~", temp)),
                      family = gaussian(link = "identity"),
                      data = df)
  
  # Logit
        model1_ever_logit <- glm(as.formula(paste("highstakeMed_ever_bin","~", temp)),
                          family = "binomial",
                          data = df)

  # Export
    summary(model1_ever_ols)
    summary(model1_ever_logit)
    
    output_path_ever <- paste0("02_analysis/02_results/3.1_SDM_ever_", country, ".docx")
    
    modelsummary(
      list("Model 1 - OLS" = model1_ever_ols, 
           "Model 2 - Logit (OR)" = model1_ever_logit),
      exponentiate = c(FALSE, TRUE),
      estimate = "{estimate}{stars} [{conf.low}, {conf.high}] ({p.value})",       # everything in a single cell:
      statistic = NULL,                              # don't add separate rows
      conf_level = 0.95,
      fmt = 3,
      stars = TRUE,
      gof_omit = NULL,
      output = output_path_ever  # or "latex", "html", "docx", "xlsx", etc.
    )


#==== 02.2  Number of Completed SDM items =============
    #**** Dependent variable: highstakeMed_count ******** 
    
  # OLS 
    model2_itemcounts_ols <- glm(as.formula(paste("highstakeMed_count","~", temp)),
                           family = gaussian(link = "identity"),
                           data = df)
    
  # nbreg
    model2_itemcounts_nbreg <- glm.nb(as.formula(paste("highstakeMed_count","~", temp)),
                             data = df)
    model2_itemcounts_nbreg_ame <- margins(model2_itemcounts_nbreg, data = df)
    
  # Export
    summary(model2_itemcounts_ols)
    summary(model2_itemcounts_nbreg)
    summary(model2_itemcounts_nbreg_ame)
    
    output_path_count <- paste0("02_analysis/02_results/3.2_SDM_count_", country, ".docx")
        
       modelsummary(
        list("Model 1 - OLS" = model2_itemcounts_ols, 
             "Model 2 - Negative binomial" = model2_itemcounts_nbreg,
             "Model 2 - Negative binomial (Average marginal effets)" = model2_itemcounts_nbreg_ame),
        # everything in a single cell:
        estimate = "{estimate}{stars} [{conf.low}, {conf.high}] ({p.value})",       # everything in a single cell:
        statistic = NULL,                              # don't add separate rows
        conf_level = 0.95,
        fmt = 3,
        stars = TRUE,
        gof_omit = NULL,
        output = output_path_count  # or "latex", "html", "docx", "xlsx", etc.
      )
    

#==== 02.3  Number of Completed SDM domains (out of 5) =============
    #**** Dependent variable: highstakeMed_domain_count ******** 
    
  # OLS 
    model3_domaincounts_ols <- glm(as.formula(paste("highstakeMed_domain_count","~", temp)),
                           family = gaussian(link = "identity"),
                           data = df)
    
  # nbreg
    model3_domaincounts_nbreg <- glm.nb(as.formula(paste("highstakeMed_domain_count","~", temp)),
                             data = df)
    
  # Ordered logit model
    model3_domaincounts_ologit <- clm(as.formula(paste("as.factor(highstakeMed_domain_count)","~", temp))
                                  , data = df, link = "logit")  # "probit", "cloglog" available
    summary(model3_domaincounts_ologit)
        
    ## Test proportional-odds (parallel slopes) assumption
    nominal_test(model3_domaincounts_ologit)   # tests if some predictors need non-parallel effects -> all good, ie. non stat-sig
    scale_test(model3_domaincounts_ologit)     # tests/handles heteroscedasticity
        
    ## Predictions <- DON'T KNOW HOW TO USE YET / IE. WHAT FOR?
    predict(model3_domaincounts_ologit, type = "prob")

  # Export
    summary(model3_domaincounts_ols)
    summary(model3_domaincounts_nbreg)
    summary(model3_domaincounts_ologit)
    
    output_path_domaincount <- paste0("02_analysis/02_results/3.3_SDM_domain count_", country, ".docx")
    
       modelsummary(
        list("Model 1 - OLS" = model3_domaincounts_ols, 
             "Model 2 - Negative binomial" = model3_domaincounts_nbreg,
             "Model 3 - Ordered logit (OR)" = model3_domaincounts_ologit),
        exponentiate = c(FALSE, FALSE, TRUE),  # convert ologit to exponential
        # everything in a single cell:
        estimate = "{estimate}{stars} [{conf.low}, {conf.high}] ({p.value})",       # everything in a single cell:
        statistic = NULL,                              # don't add separate rows
        conf_level = 0.95,
        fmt = 3,
        stars = TRUE,
        gof_omit = NULL,
        output = output_path_domaincount  # or "latex", "html", "docx", "xlsx", etc.
      )
       

#==== 02.4  STORING RESULTS =============
    model1_ever_ols_us <- model1_ever_ols
    model1_ever_logit_us <- model1_ever_logit
    model2_itemcounts_ols_us <- model2_itemcounts_ols
    model2_itemcounts_nbreg_us <- model2_itemcounts_nbreg
    model3_domaincounts_ols_us <- model3_domaincounts_ols
    model3_domaincounts_nbreg_us <- model3_domaincounts_nbreg
    model3_domaincounts_ologit_us <- model3_domaincounts_ologit
    
    tidy_model1_ever_ols_us <- tidy(model1_ever_ols, conf.int = TRUE, conf.level = 0.95)
    tidy_model1_ever_logit_us <- tidy(model1_ever_logit, conf.int = TRUE, conf.level = 0.95)
    tidy_model2_itemcounts_ols_us <- tidy(model2_itemcounts_ols, conf.int = TRUE, conf.level = 0.95)
    tidy_model2_itemcounts_nbreg_us <- tidy(model2_itemcounts_nbreg, conf.int = TRUE, conf.level = 0.95)
    tidy_model3_domaincounts_ols_us <- tidy(model3_domaincounts_ols, conf.int = TRUE, conf.level = 0.95)
    tidy_model3_domaincounts_nbreg_us <- tidy(model3_domaincounts_nbreg, conf.int = TRUE, conf.level = 0.95)
    tidy_model3_domaincounts_ologit_us <- tidy(model3_domaincounts_ologit, conf.int = TRUE, conf.level = 0.95)  

       
#==== 03.1  Number of Should haves SDM items =============
       #**** Dependent variable: highstakeMed_should_count ******** 
  
  'US'
       # OLS 
       model4_shouldcounts_ols_US <- glm(as.formula(paste("highstakeMed_should_count","~", temp)),
                                    family = gaussian(link = "identity"),
                                    data = df_IADL_us)
       tidy_model4_shouldcounts_ols_US <- tidy(model4_shouldcounts_ols_US, conf.int = TRUE, conf.level = 0.95)
       
       # nbreg
       model4_shouldcounts_nbreg_US <- glm.nb(as.formula(paste("highstakeMed_should_count","~", temp)),
                                         data = df_IADL_us)
       tidy_model4_shouldcounts_nbreg_US <- tidy(model4_shouldcounts_nbreg_US, conf.int = TRUE, conf.level = 0.95)
       
       df2_us <- droplevels(df_IADL_us)    # needed because "Chinese" level in Ethnicity caused errors 
       model4_shouldcounts_nbreg_US_ame <- margins(model4_shouldcounts_nbreg_US, data = df2_us)
       
       # Export
       summary(model4_shouldcounts_ols_US)
       summary(model4_shouldcounts_nbreg_US)
       summary(model4_shouldcounts_nbreg_US_ame)
       
       modelsummary(
         list("Model 1 - OLS" = model4_shouldcounts_ols_US, 
              "Model 2 - Negative binomial" = model4_shouldcounts_nbreg_US,
              "Model 2 - Negative binomial Average Marginal Effect" = model4_shouldcounts_nbreg_US_ame),
         estimate = "{estimate}{stars} [{conf.low}, {conf.high}] ({p.value})",
         statistic = NULL,
         conf_level = 0.95,
         fmt = 3,
         stars = TRUE,
         gof_omit = NULL,
         output = "02_analysis/02_results/4_SDM_SHOULD count_US.docx"  # or "latex", "html", "docx", "xlsx", etc.
       )
       

#==== 09. Other post-hoc tests =============
       linearHypothesis(model1_ever_logit_us,
                        c("MedicalDelegate22 - Assist-Informal - MedicalDelegate23 - Assist-Formal = 0"))
       
       
  # FOCUS ON THE FINAL MODELS ONLY => TREND TEST!
  #**********************************
  #*
  #*

       df_IADL_us$FinanceDelegate2_ordered <- factor(df_IADL_us$FinanceDelegate2,
                                                     levels = c("0 - No", "2 - Assist-Informal", "3 - Assist-Formal"),
                                                     ordered = TRUE)
       df_IADL_us$MedicalDelegate2_ordered <- factor(df_IADL_us$MedicalDelegate2,
                                                     levels = c("0 - No", "2 - Assist-Informal", "3 - Assist-Formal"),
                                                     ordered = TRUE)
       
      ## Modify here if necessary to ensure consistency throughout
       'indep_demo = c("GenderFemale", "Age", "Marital_3L", "Ethnicity_4L", "Education_3L", "Housing", "Employed", "Income_4L", "Religion_5L", "Sibling_4L") # "Citizen" - should not include
       indep_care = c("IADL", "ADL", "IADL_HelpDigital", "lowstakeMed")'
       indep_delegation_ordered = c("FinanceDelegate2_ordered", "MedicalDelegate2_ordered")
       'indep_LPA = c("FinanceLPA")
       dep_var = c("highstakeMed_ever", "highstakeMed_count", "highstakeMed_domain_count")'
       
       
       # Independent variables
       temp_ordered <- paste(c(indep_demo, indep_care, indep_delegation_ordered, indep_LPA),
                     collapse = " + ")
      
       # Logit
       df_IADL_us$highstakeMed_ever_bin <- as.numeric(df_IADL_us$highstakeMed_ever) - 1 # OLS and logit does not work on factors
       model1_ever_logit_trend <- glm(as.formula(paste("highstakeMed_ever_bin","~", temp_ordered)),
                                family = "binomial",
                                data = df_IADL_us)
       summary(model1_ever_logit_trend) #'linear trend Finance: p = 0.21884, Medical p = 0.01042 * '
       
       # nbreg
       model2_itemcounts_nbreg_trend <- glm.nb(as.formula(paste("highstakeMed_count","~", temp_ordered)),
                                         data = df_IADL_us)
       summary(model2_itemcounts_nbreg_trend) #'linear trend Finance: p = 0.000577 ***, Medical p = 0.129141  '
       
       # Ordered logit model
       model3_domaincounts_ologit_trend <- clm(as.formula(paste("as.factor(highstakeMed_domain_count)","~", temp_ordered))
                                         , data = df_IADL_us, link = "logit")  # "probit", "cloglog" available
       summary(model3_domaincounts_ologit_trend) #'linear trend Finance: p = 9.13e-06 ***, Medical p = 0.000378 ***'
       
       
       
       
       
       'SHOULD HAVE -> NOT NEEDED FOR TREND TEST
       
       # nbreg
       model4_shouldcounts_nbreg_US <- glm.nb(as.formula(paste("highstakeMed_should_count","~", temp)),
                                              data = df_IADL_us)
       tidy_model4_shouldcounts_nbreg_US <- tidy(model4_shouldcounts_nbreg_US, conf.int = TRUE, conf.level = 0.95)
       
       '



      
