# Subgroup analyses where focusing only on those with physical/cognitive limitations 
  # Different research question: would formalisation of digital delegation
  
  # Inspect the reasons for helping
  View(data.frame(FinanceManageReasons = my.dat.us$FinanceManageReasons)) # Q5.18 - reasons for helping with financial digital task
  
  View(data.frame(DigitalAssistReasons = my.dat.us$DigitalAssistReasons))  # Q6.37 - reasons for helping with medical digital task
  
  my.dat.us$FinAssist_reason_limitation <- as.integer(grepl("limitations", my.dat.us$FinanceManageReasons)) # n=113
  my.dat.us$MedAssist_reason_limitation <- as.integer(grepl("limitations", my.dat.us$DigitalAssistReasons)) # n=116
  my.dat.us$DigiAssist_reason_limitation <- (my.dat.us$FinAssist_reason_limitation | my.dat.us$MedAssist_reason_limitation) == 1 # n = 171
  
  tabyl(my.dat.us$FinAssist_reason_limitation)
  tabyl(my.dat.us$MedAssist_reason_limitation)
  tabyl(my.dat.us$DigiAssist_reason_limitation)
  
  tabyl(my.dat.us,FinAssist_reason_limitation, IADL_ever) # 99% of those who helped because of physical/cognitive limitations have helped with IADL before - durh
  tabyl(my.dat.us,MedAssist_reason_limitation, IADL_ever) # 100% of those who helped because of physical/cognitive limitations have helped with IADL before - durh
  tabyl(my.dat.us,DigiAssist_reason_limitation, IADL_ever) # 100% of those who helped because of physical/cognitive limitations have helped with IADL before - durh
  

  tabyl(my.dat.us, FinanceDelegate2, DigiAssist_reason_limitation)%>%
    adorn_percentages("col") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns()
  
  tabyl(my.dat.us, MedicalDelegate2, DigiAssist_reason_limitation)%>%
    adorn_percentages("col") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns()
  
  
#---- 02. Regressions -----
## Comprehensive list of all the variables that will be used in analyses
## Modify here if necessary to ensure consistency throughout
  indep_demo = c("GenderFemale", "Age", "Marital_3L", "Ethnicity_4L", "Education_3L", "Housing", "Employed", "Income_4L", "Religion_5L", "Sibling_4L") # "Citizen" - should not include
  indep_care = c("IADL", "ADL", "lowstakeMed") # "IADL_HelpDigital" -> REMOVED POST R1 AS ALREADY INCLUDED AS PART OF IADL
  indep_care_num = c("help_physical", "help_finance", "help_medical") # ADDED POST R1 TO REFLECT THE EXTENT OF CAREGIVING EXPERIENCE
  indep_delegation = c("FinanceDelegate2", "MedicalDelegate2")
  indep_LPA = c("FinanceLPA")
  dep_var = c("highstakeMed_ever", "highstakeMed_count", "highstakeMed_domain_count")
  
 
# Independent variables
  temp <- paste(c(indep_demo, indep_care, indep_care_num, indep_delegation, indep_LPA),
                collapse = " + ")

  # Restrict the sample to those helping due to physical/cognitive limitations
    df <- my.dat.us[my.dat.us$DigiAssist_reason_limitation == 1,]
    df <- droplevels(df) #remove unused levels (especially among ethnicity levels)
    
    #==== 02.1  Completed any of the SDM items =============
    #**** Dependent variable: highstakeMed_ever_bin ******** 
    
    df$highstakeMed_ever_bin <- as.numeric(df$highstakeMed_ever) - 1 # OLS and logit does not work on factors

    # Logit
    tabyl(df, highstakeMed_ever_bin) # n_yes = 159 (93.0%) => model does not converge
    model1_ever_logit <- glm(as.formula(paste("highstakeMed_ever_bin","~", temp)),
                             family = "binomial",
                             data = df)
    
    #==== 02.2  Number of Completed SDM items =============
    #**** Dependent variable: highstakeMed_count ******** 
    
    # nbreg
    model2_itemcounts_nbreg <- glm.nb(as.formula(paste("highstakeMed_count","~", temp)),
                                      data = df)
    model2_itemcounts_nbreg_ame <- margins(model2_itemcounts_nbreg, data = df)
    
    #==== 02.3  Number of Completed SDM domains (out of 5) =============
    #**** Dependent variable: highstakeMed_domain_count ******** 
    
    # Ordered logit model
    model3_domaincounts_ologit <- clm(as.formula(paste("as.factor(highstakeMed_domain_count)","~", temp))
                                      , data = df, link = "logit")  # "probit", "cloglog" available
    
    
    # Export
    summary(model1_ever_logit)
    summary(model2_itemcounts_nbreg)
    summary(model2_itemcounts_nbreg_ame)
    summary(model3_domaincounts_ologit)
    
    output_path_ever <- paste0("02_analysis/02_results/5.1_SDM_US_n171_limitation only", ".docx")
    
    modelsummary(
      list("Model 1 - Logit (OR)" = model1_ever_logit,
           "Model 2 - Negative binomial" = model2_itemcounts_nbreg,
           "Model 2 - Negative binomial (Average marginal effets)" = model2_itemcounts_nbreg_ame,
           "Model 3 - Ordered logit (OR)" = model3_domaincounts_ologit),
      exponentiate = c(TRUE, FALSE, FALSE, TRUE),
      estimate = "{estimate}{stars} [{conf.low}, {conf.high}] ({p.value})",       # everything in a single cell:
      statistic = NULL,                              # don't add separate rows
      conf_level = 0.95,
      fmt = 3,
      stars = TRUE,
      gof_omit = NULL,
      output = output_path_ever  # or "latex", "html", "docx", "xlsx", etc.
    )
  
    
    
#**** BIVARIABLE LOGISTICS REGS ONLY ********
# Run the logistic regression as binary as most within n = 171 have already done at least 1 ADL
  tabyl(df$highstakeMed_ever_bin) # n_yes = 159 (92.98%)

  var <- c(indep_care, indep_delegation, indep_LPA)
  
  results <- list()
  
  for (temp in var) {
    
    model <- glm(
      as.formula(paste("highstakeMed_ever_bin ~", temp)),
      family = "binomial",
      data = df
    )
    
    # Extract components
    or <- exp(coef(model))
    ci <- exp(confint(model))
    pval <- summary(model)$coefficients[,4]
    
    # Remove intercept
    keep <- names(or) != "(Intercept)"
    
    # Format p-values
    pval_fmt <- ifelse(pval[keep] < 0.001, "<0.001",
                       sprintf("%.3f", pval[keep]))
    
    # Combine OR + CI into one string
    or_ci <- sprintf("%.3f (%.3f–%.3f)", 
                     or[keep], ci[keep,1], ci[keep,2])
    
    output <- data.frame(
      Predictor = temp,
      `OR (95% CI)` = or_ci,
      `p-value` = pval_fmt,
      row.names = NULL
    )
    
    results[[temp]] <- output
  }
  
  # Combine all results into one table
  final_table <- do.call(rbind, results)
  
  print(final_table)
  
