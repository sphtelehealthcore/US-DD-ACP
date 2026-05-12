# SUBSETTING THE RIGHT DATASET ===========================
my.dat.us   <- subset(my.dat, !(Country=="SG"))
df_IADL_us     <- my.dat.us[my.dat.us$IADL_ever == "1 - Helped with IADL before",]
df_IADL       <- subset(my.dat, IADL_ever == "1 - Helped with IADL before")


# DESCRIPTIVE STATS ===========================

## Comprehensive list of all the variables that will be used in analyses
## Modify here if necessary to ensure consistency throughout
indep_demo = c("GenderFemale", "Age", "Citizen", "Marital_3L", "Ethnicity_4L", "Education_3L", "Housing", "Employed", "Income_4L", "Religion_5L", "Sibling_4L")
indep_care = c("IADL", "ADL", "IADL_HelpDigital")
indep_delegation = c("FinanceDelegate2", "MedicalDelegate2")


# Table1 Demographics ----
temp <- paste(c(indep_demo, indep_care, indep_delegation),
              collapse = " + ")

'Table 1 for US overall sample - compare between IADL_ever status'
  table1_us_demo <- table1(as.formula(paste("~", temp, "| IADL_ever")) # split sample by "having helped with IADL" status
                           , data = my.dat.us, overall="Total", extra.col = list(`p-value`=pvalue), extra.col.pos = 4,
                           render.missing=NULL, render.categorical="FREQ (PCTnoNA%)")
  as.data.frame(table1_us_demo)
  table1_us_demo
  
  t1flex(table1_us_demo) %>% 
    save_as_docx(path="02_analysis/02_results/2_table1_demo_US.docx")


# Table2 Caretaking and delegation ----
'Table 2 for US overall sample - compare between IADL_ever status'
  table2_us_delegation <- table1(~ help_physical # Number of Immediate family + relatives + Friends helped PHYSICALLY
                                + IADL # extent of Physical help / delegation - Range 0 - 24, higher indicates greater frequency of help/caring
                                + ADL_ever + ADL # extent of Physical help / delegation - Range 0 - 24, higher indicates greater frequency of help/caring
                                # + IADL_HelpDigital -> POST R1 -> REMOVED AS ALREADY INCLUDED AS PART OF IADL_SCORE
          # Delegation - FINANCE
                          + help_finance  # Number of Immediate family + relatives + Friends helped FINANCE
                          + FinanceLPA + FinanceLPAUsePersonal + FinanceLPAUseProperty #? maybe, how to interpret / Is it useful the frequency items?
                                + FinanceJointAccount # Q5.15 whether they have a joint account
                          + FinanceDigitalAssistance  #*** Q5.17 ever assisted with legal/financial digital services ****************** <- Key var **
                                + FinanceLoginWays_4cat # Q5.19. How have you logged on to financial/legal digital services?
                                + FinanceAssistWays_4cat # Q5.20. How have you assisted financial/legal digital services?
                          + FinAssist_reason_limitation # Q5.18 -> NEW: assist due to physical/cognitive limitation 
                          + FinanceDelegate2 #* **** <- Key variable **********************
          # Delegation - MEDICAL
                          + help_medical # Number of Immediate family + relatives + Friends helped MEDICAL
                          + DigitalCaregiverAccount # Q6.33. Have you ever had a caregiver medical account for that adult?
                          + DigitalAssistMedical #***  Q6.36. ever assisted with digital medical services? ********************** <- Key var **
                                + MedicalLoginWays_4cat # Q6.38. How do you assist another adult in accessing their digital medical services?
                                + MedicalAssistWays_4cat # Q6.39. How have you helped another adult use their digital medical services online?
                          + MedAssist_reason_limitation # Q6.37 -> NEW: assist due to physical/cognitive limitation
                          + DigiAssist_reason_limitation # Q5.18 + Q6.37 -> NEW: combined assist due to physical/cognitive limitation
                          + MedicalDelegate2 #* **** <- Key variable **********************
          # lowstake SDM                   
                          + lowstakeMed_ever + lowstakeMed # range: 0 - 16 (0 - 4 levels x 4 items)
          # Highstake SDM                
                            + MedicalDecision_ever  # 3 items: Q6.14 Medical Delegation Action (High-Stake Activities): Medical Decision Making
                            + MedicalWorth_ever  # 3 items: Q6.18. Medical Delegation Action (High-Stake Activities): Quality of Life (Health Situations)
                            + MedicalEnd_ever # 3 items: Q6.22. Medical Delegation Action (High-Stake Activities): Quality of Life (Medical Care)
                            + MedicalFlex_ever # 3 items: Q6.26. Medical Delegation Action (High-Stake Activities): Flexibility for surrogate decision making
                            + MedicalAskDoc_ever # 1 item: Q6.30. Medical Delegation Action (High-Stake Activities): Asking Questions of medical providers
                            + highstakeMed_domain_count + as.factor(highstakeMed_domain_count)
                            + highstakeMed_ever + highstakeMed_count #* **** <- Key variable **********************
                          | IADL_ever, data = my.dat.us, overall="Total", extra.col = list(`p-value`=pvalue), extra.col.pos = 4,
                          render.missing=NULL, render.categorical="FREQ (PCTnoNA%)",)
  as.data.frame(table2_us_delegation)
  table2_us_delegation
  
  t1flex(table2_us_delegation) %>% 
    save_as_docx(path="02_analysis/02_results/2_table2_delegation and SDM_US.docx")


#Descriptive stats ===============
  tabyl(df_IADL_us$highstakeMed_domain_count)
  
  tabyl(df_IADL_us$MedicalDecision_ever)
  tabyl(df_IADL_us$MedicalWorth_ever)
  tabyl(df_IADL_us$MedicalEnd_ever)
  tabyl(df_IADL_us$MedicalFlex_ever)
  tabyl(df_IADL_us$MedicalAskDoc_ever)
  
  # CHECKING FOR DISTRIBUTION FOR NBREG 
  df_IADL_us %>%
    summarise(
      mean_value = mean(highstakeMed_count, na.rm = TRUE),
      variance = var(highstakeMed_count, na.rm = TRUE),
      dispersion = var(highstakeMed_count, na.rm = TRUE) / mean(highstakeMed_count, na.rm = TRUE),
      kurtosis = kurtosis(highstakeMed_count, na.rm = TRUE)
    )
  
# Caregiver profiles ===============
  'US'
  subset(df_IADL_us, FinanceLPA == "0 - No LPA")  %>% 
    tabyl(FinanceDelegate2, MedicalDelegate2) %>%            # cross-tab
    adorn_totals("both") %>%
    adorn_percentages("all") %>%          # "all" gives % of total table
    adorn_pct_formatting(digits = 1) %>%      # format to 1 decimal place
    adorn_ns(position = "front")
  
  subset(df_IADL_us, FinanceLPA == "1 - Yes LPA")  %>% 
    tabyl(FinanceDelegate2, MedicalDelegate2) %>%            # cross-tab
    adorn_totals("both") %>%
    adorn_percentages("all") %>%          # "all" gives % of total table
    adorn_pct_formatting(digits = 1) %>%      # format to 1 decimal place
    adorn_ns(position = "front")

  
# Distribution of ACP domain counts ===============  
  df_IADL_us  %>% 
    tabyl(highstakeMed_domain_count, FinanceDelegate2) %>%            # cross-tab
    adorn_totals("both") %>%
    adorn_percentages("all") %>%          # "all" gives % of total table
    adorn_pct_formatting(digits = 1) %>%      # format to 1 decimal place
    adorn_ns(position = "front")
  
  df_IADL_us  %>% 
    tabyl(highstakeMed_domain_count, MedicalDelegate2) %>%            # cross-tab
    adorn_totals("both") %>%
    adorn_percentages("all") %>%          # "all" gives % of total table
    adorn_pct_formatting(digits = 1) %>%      # format to 1 decimal place
    adorn_ns(position = "front")
  

  df_IADL_us  %>% 
    tabyl(highstakeMed_count, MedicalDelegate2) %>%            # cross-tab
    adorn_totals("both") %>%
    adorn_percentages("all") %>%          # "all" gives % of total table
    adorn_pct_formatting(digits = 1) %>%      # format to 1 decimal place
    adorn_ns(position = "front")
  
  
# Distribution of ethnicity ===============  
  my.dat.us$Ethnicity_4L <- droplevels(my.dat.us$Ethnicity_4L)
  levels(my.dat.us$Ethnicity_4L)
  chisq.test(table(my.dat.us$Ethnicity_4L, my.dat.us$IADL_ever)) 

