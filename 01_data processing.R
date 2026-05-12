## Load and filter data -------------------------------------------------------
dat <- read.csv("01_data/cleaned_combined.csv")

my.dat <- dat
View(my.dat)

## Section Q3 - factorize and group demographics ----------------------------------------------------------
  my.dat <- transform_factor(my.dat, "Country", levels = c("US", "SG"), labels = c("US", "SG"))
  my.dat <- my.dat %>%
    mutate(GenderFemale = ifelse(Gender == "Female", 1,0))
    my.dat$GenderFemale <- factor(my.dat$GenderFemale,
                              levels = c("0","1"),
                              labels = c("0 - Male", "1 - Female"))
  
  my.dat <- transform_factor(my.dat, "Marital", na = "3", levels = c("Unmarried", "Married", "Div/Sep", "Widowed"), labels = c("0", "2", "1", "1"))
    my.dat$Marital_3L <- factor(my.dat$Marital,
                 levels = c("0","1","2"),
                 labels = c("Unmarried", "Divorced/Separated/Widowed", "Married"))
  my.dat <- transform_factor(my.dat, "Citizen", levels = c("Citizen","Green card"), labels = c("1","0"))
  my.dat <- transform_factor(my.dat, "Housing", levels = c("low-income housing", "apartment", "familyhouse"), 
                             labels = c("0", "1", "2"))
    my.dat$Housing  <- factor(my.dat$Housing,
                              levels = c("0","1","2"),
                              labels = c("0 - low-income housing", "1 - apartment", "2 - familyhouse"))
  
  my.dat <- transform_order_to_numeric(my.dat, "Income", c("<$30k", "$30k", "$50k", "$100k", "$150k", ">$200k"))
    my.dat$Income_4L  <- factor(my.dat$Income,
                  levels = c("1","2","3","4","5","6"), 
                  labels = c("<$30k", "$30k - <50k", "$50k - <$100k", "$100k or more","$100k or more","$100k or more"))

  my.dat$Ethnicity_4L <- factor(my.dat$Ethnicity,
                                levels = c("White", "His/Lat", "Black", "Mixed", "Asian", "Others", "Native", 
                                           "Chinese", "Malay", "Indian", "Eurasian"),
                                labels = c("White", "His/Lat", "Black", "Mixed, Asian, Native, Others", "Mixed, Asian, Native, Others", "Mixed, Asian, Native, Others", "Mixed, Asian, Native, Others", 
                                           "Chinese", "Malay", "Indian", "Mixed, Asian, Native, Others"))  
  
  my.dat$Religion  <- factor(my.dat$Religion,
                              levels = c("No Religion", "Catholic", "Catholicism", "Christian", "Christianity", "Buddhism", "Buddhist", "Jehovah Witness",
                                         "Islam", "Muslim", "Mormon", "Spiritualist", "Pagan", "Jewish", 
                                          "Hindu", "Hinduism", "Agnostic", "Taoism", "PNA"),
                              labels = c("No Religion", "Catholic", "Catholic", "Christian", "Christian", "Buddhism", "Buddhism", "Jehovah Witness",
                                        "Islam", "Islam", "Mormon", "Spiritualist", "Pagan", "Jewish", 
                                        "Hindu", "Hindu", "Agnostic", "Taoism", "Prefer not to Answer")) # unique(Religion) = 14
    
    my.dat$Religion_5L  <- factor(my.dat$Religion,
                              levels = c("No Religion", "Catholic", "Christian", "Buddhism", "Jehovah Witness",
                                         "Islam", "Mormon", "Spiritualist", "Pagan", "Jewish", 
                                         "Hindu", "Agnostic", "Taoism", "Prefer not to Answer"), # 14 unique levels -> combined into 4 main grous'
                              labels = c("No Religion", "Catholic", "Christian", "Buddhism","Others","Others","Others","Others","Others","Others","Others","Others","Others","Others"))
    
  my.dat <- transform_factor(my.dat, "Education", levels = c("Below", "High", "Degree and above"), 
                             labels = c("< High School", "High School", "Degree and above"), ordered = TRUE, convert_to_numeric = TRUE)
    my.dat$Education_3L  <- factor(my.dat$Education,
                              levels = c("1","2","3"), 
                              labels = c("< High School", "High School", "Degree and above"))
  
  my.dat <- transform_factor(my.dat, "Employment", levels = c("Full-time", "Part-time", "Self", "Disabled", "Retired", "Student", "Unemployed"), 
                             labels = c("Employed", "Employed", "Employed", "Unemployed", "Unemployed", "Unemployed", "Unemployed"), ordered = TRUE, convert_to_numeric = TRUE)
    my.dat$Employed  <- factor(my.dat$Employment,
                              levels = c("1","2"), 
                              labels = c("Employed (Full/Part/Self)", "Unemployed"))
  
  my.dat <- transform_order_to_numeric(my.dat, "Siblings", c("0", "1", "2", "3", ">4"))
  # my.dat <- transform_order_to_numeric(my.dat, "SiblingsOrder", c("Youngest", "Middle", "Oldest"))
    my.dat <- my.dat %>%
      mutate(Sibling_4L = ifelse(Siblings == 1, 1,                             # No sibling -> only child
                ifelse(SiblingsOrder == "Oldest", 2,
                       ifelse(SiblingsOrder == "Middle", 3, 4))))

    my.dat$Sibling_4L <- factor(my.dat$Sibling_4L,
             levels = c("1","2","3","4"), 
             labels = c("1 - Only child", "2 - Oldest child", "3 - Middle child", "4 - Youngest child"))
      
  View(my.dat)

## Section Q4 - Physical Care Tasks ---------------------------------------------
  # factorize Physical Help
  my.dat <- transform_factor(my.dat, "HelpImmediateFam", levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "15", "17", "20", "25", ">30"), labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "15", "17", "20", "25", "31"), convert_to_numeric = TRUE)
  my.dat <- transform_factor(my.dat, "HelpRelatives", levels = c("0", "1", "2", "3", "4", "5", "6", "9", "10", "16", "20", "29", ">30"), labels = c("0", "1", "2", "3", "4", "5", "6", "9", "10", "16", "20", "29", "31"), convert_to_numeric = TRUE) # in relatives
  my.dat <- transform_factor(my.dat, "HelpFriends", levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "10", "11", "12", "15", "20", "25", "30", ">30", ">=100"), labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "10", "11", "12", "15", "20", "25", "30", "31", "101"), convert_to_numeric = TRUE) # in Friends
  my.dat <- transform_factor(my.dat, "HelpPassAway", na = "-1", levels = c("-1", "0", "1", "2", "3", ">3"), labels = c("-1", "0", "1", "2", "3", "4"), convert_to_numeric = TRUE) # in Pass Away
  my.dat$help_physical <- rowSums(my.dat[, c("HelpImmediateFam", "HelpRelatives", "HelpFriends")], na.rm = TRUE)    # Added after R&R1: Ask to report the extent/experience of helping
  
    
  # Factorize IADL -----------------------------------------------
    IADL_columns <- c("HelpFood", "HelpHousekeep", "HelpLaundry", "HelpTransport", "HelpMed", "HelpFinance", "HelpDigital")
    levels_list_IADL <- c("Never", "Yearly", "Monthly", "Weekly", "Daily")
    labels_list_IADL <- c("0", "1", "2", "3", "4")
    my.dat <- transform_IADL_ADL(my.dat, IADL_columns, levels_list_IADL, labels_list_IADL)
    
    my.dat <- plyr::rename(my.dat,c(
      "HelpFood" = "IADL_HelpFood", 
      "HelpHousekeep" = "IADL_HelpHousekeep", 
      "HelpLaundry" = "IADL_HelpLaundry", 
      "HelpTransport" = "IADL_HelpTransport", 
      "HelpMed" = "IADL_HelpMed", 
      "HelpFinance" = "IADL_HelpFinance", 
      "HelpDigital" = "IADL_HelpDigital"
    ))
  
    # factorize IADL(future)
    transform_IADL_future <- function(data, column_names, na = NULL, levels, labels, convert_to_numeric = FALSE) {
      for (col in column_names) {
        if (!is.null(na)) {
          data[[col]][is.na(data[[col]])] <- na
        }
        data[[col]] <- factor(data[[col]], levels = levels, labels = labels)
        if (convert_to_numeric) {
          data[[col]] <- as.numeric(as.character(data[[col]]))
        }
      }
      return(data)
    }
    columns_IADL_future <- c("HelpFoodFuture", "HelpHousekeepFuture", "HelpLaundryFuture", "HelpTransportFuture", "HelpMedFuture", "HelpFinanceFuture", "HelpDigitalFuture")
    levels_list_future <- c("-1","V.unlikely","S.unLikely","Unsure","S.likely","V.likely")
    labels_list_future <- c("-1","0","1","2","3","4")
    my.dat <- transform_IADL_future(my.dat, columns_IADL_future, na = "-1", levels = levels_list_future, labels = labels_list_future, convert_to_numeric = TRUE)
    
  # Factorize ADL -------------------------------------------------
    ADL_columns <- c("ADLBath", "ADLGroom", "ADLToilet", "ADLTransfer", "ADLIncontinence", "ADLFeed")
    levels_list_ADL <- c("Never", "Yearly", "Monthly", "Weekly", "Daily")
    labels_list_ADL <- c("0", "1", "2", "3", "4")
    my.dat <- transform_IADL_ADL(my.dat, ADL_columns, levels_list_ADL, labels_list_ADL)


## Section Q5 - Legal/ Financial Delegation ----------------------------------
  # factorize Financial Help
    my.dat <- transform_factor(my.dat, "FinanceHelpImmediateFam", levels = c("0", "1", "2", "3", "4", "5", "6", "10", "12", "18", "20", ">30"), labels = c("0", "1", "2", "3", "4", "5", "6", "10", "12", "18", "20", "31"), convert_to_numeric = TRUE)
    my.dat <- transform_factor(my.dat, "FinanceHelpRelatives", levels = c("0", "1", "2", "3", "5", "6", "7", "8", "10", "20", "30", ">30"), labels = c("0", "1", "2", "3", "5", "6", "7", "8", "10", "20", "30", "31"), convert_to_numeric = TRUE)
    my.dat <- transform_factor(my.dat, "FinanceHelpFriends", levels = c("0", "1", "2", "3", "4", "5", "6", "7", "9", "10", "12", "15", "20", ">30", ">100"), labels = c("0", "1", "2", "3", "4", "5", "6", "7", "9", "10", "12", "15", "20", "31", "101"), convert_to_numeric = TRUE)
    my.dat <- transform_factor(my.dat, "FinanceHelpPassAway", na = "-1", levels = c("-1", "0", "1", "2", "3", ">3"), labels = c("-1", "0", "1", "2", "3", "4"), convert_to_numeric = TRUE)
    my.dat$help_finance <- rowSums(my.dat[, c("FinanceHelpImmediateFam", "FinanceHelpRelatives", "FinanceHelpFriends")], na.rm = TRUE)    # Added after R&R1: Ask to report the extent/experience of helping
    
  # factorize Financial Task 
    financial_columns <- c("FinanceDecisionSupport", "FinanceAction")
    my.dat <- transform_IADL_ADL(my.dat, financial_columns, levels_list_IADL, labels_list_IADL) # in Help with Decision Support, Finance Action - frequency (daily ...)
    
  # Q5.10. Have you signed Lasting Power of Attorney (LPA) in the US?
    my.dat <- transform_factor(my.dat, "FinanceLPA", levels = c("No", "Yes"), labels = c("0 - No LPA", "1 - Yes LPA"), convert_to_numeric = FALSE) # in Help with Finance LPA
    my.dat <- transform_factor(my.dat, "FinanceLPAUsePersonal", na = "-1", levels = c("-1", "Never", "Rarely", "Sometimes", "Frequently", "Always"), labels = c("-1", "0", "1", "2", "3", "4"), convert_to_numeric = TRUE) # in Help with Finance LPA Personal Use
    my.dat <- transform_factor(my.dat, "FinanceLPAUseProperty", na = "-1", levels = c("-1", "Never", "Rarely", "Sometimes", "Frequently", "Always"), labels = c("-1", "0", "1", "2", "3", "4"), convert_to_numeric = TRUE) # in Help with Finance LPA Property Use
  
  # factorize Financial Task (Future)
    columns_financial_future <- c("FinanceDecisionSupportFuture", "FinanceActionFuture")   # in Help with Decision Support, Finance Action
    my.dat <- transform_IADL_future(my.dat, columns_financial_future, na = "-1", levels = levels_list_future, labels = labels_list_future, convert_to_numeric = TRUE)
  
  ## FINANCIAL DIGITAL DELEGATION -> SEE BELOW UNDER DIGITAL DELEGATION

## Section Q6 - Medical Delegation ---------------------------------------------
  # Q6.2 factorize Medical Help 
    my.dat <- transform_factor(my.dat, "MedicalHelpImmediateFam", levels = c("0", "1", "2", "3", "4", "5", "6", "10", "12", "18", "20", ">30", ">100"), labels = c("0", "1", "2", "3", "4", "5", "6", "10", "12", "18", "20", "31", "101"), convert_to_numeric = TRUE)
    my.dat <- transform_factor(my.dat, "MedicalHelpRelatives", levels = c("0", "1", "2", "3", "4", "5", "6", "7", "10", "20", ">30"), labels = c("0", "1", "2", "3", "4", "5", "6", "7", "10", "20", "31"), convert_to_numeric = TRUE)
    my.dat <- transform_factor(my.dat, "MedicalHelpFriends", levels = c("0", "1", "2", "3", "4", "5", "6", "7", "10", "15", "20", "26", ">30", ">=100"), labels = c("0", "1", "2", "3", "4", "5", "6", "7", "10", "15", "20", "26", "31", "101"), convert_to_numeric = TRUE)
    my.dat <- transform_factor(my.dat, "MedicalHelpPassAway", na = "-1", levels = c("-1", "0", "1", "2", "3", ">3"), labels = c("-1", "0", "1", "2", "3", "4"), convert_to_numeric = TRUE)
    my.dat$help_medical <- rowSums(my.dat[, c("MedicalHelpImmediateFam", "MedicalHelpRelatives", "MedicalHelpFriends")], na.rm = TRUE)    # Added after R&R1: Ask to report the extent/experience of helping
    
  # Q6.5 - Q6.13. factorize Medical Task Low Stake 
    # in Help with Making Medical Appt, in Accompanying with Medical Appt, Help with Taking Medicine, Help with Overseeing Treatment
    low_stake_columns <- c("MedicalMakeAppt", "MedicalAccompanyAppt", "MedicalTakeMeds", "MedicalSeeTreatment")
    my.dat <- transform_IADL_ADL(my.dat, low_stake_columns, levels_list_IADL, labels_list_IADL) # frequency: daily, etc. 
  # Factorize Medical Task Low Stake (future)
    columns_financial_future <- c("MedicalMakeApptFuture", "MedicalAccompanyApptFuture", "MedicalTakeMedsFuture", "MedicalSeeTreatmentFuture")
    my.dat <- transform_IADL_future(my.dat, columns_financial_future, na = "-1", levels = levels_list_future, labels = labels_list_future, convert_to_numeric = TRUE)
  
  # Q6.14 - Q6.31. Factorize Medical Task High Stake 
    # Making medical decisions, Quality of life, Medical Care - end of life, Flexibility for Surrogate in Making Decisions
  column_groups <- list(
    MedicalDecision = c("MedicalDecisionPerson", "MedicalDecisionDoctor", "MedicalSignPapers"),
    MedicalWorth = c("MedicalWorthPerson", "MedicalWorthDoctor", "MedicalWorthFamily"),
    MedicalEnd = c("MedicalEndPerson", "MedicalEndDoctor", "MedicalEndFamily"),
    MedicalSurrogate = c("MedicalFlexibilityPerson", "MedicalFlexibilityDoctor", "MedicalFlexibilityFamily", "MedicalDoctorQuestions"),
    MedicalHighStake = c("MedicalDecision", "MedicalWorth", "MedicalEnd", "MedicalSurrogate")
  )
    
    high_stake_columns <- c("MedicalDecisionPerson", "MedicalDecisionDoctor", "MedicalSignPapers", #MedicalDecision
                            "MedicalWorthPerson", "MedicalWorthDoctor", "MedicalWorthFamily", #MedicalWorth
                            "MedicalEndPerson", "MedicalEndDoctor", "MedicalEndFamily", #MedicalEnd
                            "MedicalFlexibilityPerson", "MedicalFlexibilityDoctor", "MedicalFlexibilityFamily", "MedicalDoctorQuestions") #MedicalSurrogate
    levels_list_high_stake <- c("No, because I didn't need to", "No, but I should have", "Yes")
    labels_list_high_stake <- c("0", "1", "2")
    my.dat <- transform_IADL_ADL(my.dat, high_stake_columns, levels_list_high_stake, labels_list_high_stake)
    
  # Q6.32. Medical DIGITAL Delegation
    # in Managing with Caregiver Account (Num)
    unique(my.dat$DigitalCaregiverAccountNum)
    my.dat$DigitalCaregiverAccountNum = factor(my.dat$DigitalCaregiverAccountNum,
                                               levels = c("0","1","2","3","4","5","8","10","12","17","23",">30",">=100"),
                                               labels = c("0","1","2","3","4","5","8","10","12","17","23","31","100"))
    my.dat$DigitalCaregiverAccountNum <- as.numeric(as.character(my.dat$DigitalCaregiverAccountNum))
    
    
    
## NEW VARIABLES -----------------------------------------------------------
    # Generate IADL and ADL - counts and binary
      my.dat$IADL = rowSums(my.dat[ , c("IADL_HelpFood","IADL_HelpHousekeep","IADL_HelpLaundry","IADL_HelpTransport","IADL_HelpMed","IADL_HelpFinance","IADL_HelpDigital")])
                            # Post R1 -> IADL_HelpDigital was added back into IADL_ever and not analysed as a separate item
                            # -> there was high correlation between IADL_HelpDigital and IADL_ever(without IADL_HelpDigital) anyway!
                            # Justification for modification of IADL to exclude 1) using telephone and 2) shopping
      my.dat$IADL_ever = ifelse(my.dat$IADL >0, 1, 0)
          my.dat$IADL_ever = factor(my.dat$IADL_ever,
                                    levels = c("0","1"),
                                    labels = c("0 - Never","1 - Helped with IADL before"))
      
      my.dat$ADL = rowSums(my.dat[ , c("ADLBath","ADLFeed","ADLGroom","ADLIncontinence","ADLToilet","ADLTransfer")]) 
      my.dat$ADL_ever = ifelse(my.dat$ADL >0, 1, 0)
          my.dat$ADL_ever = factor(my.dat$ADL_ever,
                                    levels = c("0","1"),
                                    labels = c("0 - Never","1 - Helped with ADL before"))
  
    
    #* NEW! * POST R1 -  Generate reasons for helping with digital tasks - due to physical/cognitive limitations
          my.dat$FinAssist_reason_limitation <- as.integer(grepl("limitations", my.dat$FinanceManageReasons))
          my.dat$MedAssist_reason_limitation <- as.integer(grepl("limitations", my.dat$DigitalAssistReasons))
          my.dat$DigiAssist_reason_limitation <- (my.dat$FinAssist_reason_limitation | my.dat$MedAssist_reason_limitation) == 1
          
    # Generate high stake medical decisions - counts and binary - based on 15-item ACP survey by Sudore
      ## Subgroup 1: Medical decision maker
          my.dat$MedicalDecision_count = rowSums(my.dat[, c("MedicalDecisionPerson", "MedicalDecisionDoctor", "MedicalSignPapers")] == 2) 
          my.dat$MedicalDecision_ever = ifelse(my.dat$MedicalDecision_count >0, 1, 0)
      
      ## Subgroup 2: Health situations
          my.dat$MedicalWorth_count = rowSums(my.dat[, c("MedicalWorthPerson", "MedicalWorthDoctor", "MedicalWorthFamily")] == 2)
          my.dat$MedicalWorth_ever = ifelse(my.dat$MedicalWorth_count >0, 1, 0)
      
      ## Subgroup 3: EOL medical care
          my.dat$MedicalEnd_count = rowSums(my.dat[, c("MedicalEndPerson", "MedicalEndDoctor", "MedicalEndFamily")] == 2)
          my.dat$MedicalEnd_ever = ifelse(my.dat$MedicalEnd_count >0, 1, 0)
      
      ## Subgroup 4: Flexibility in SDM
          my.dat$MedicalFlex_count = rowSums(my.dat[, c("MedicalFlexibilityPerson", "MedicalFlexibilityDoctor", "MedicalFlexibilityFamily")] == 2)
          my.dat$MedicalFlex_ever = ifelse(my.dat$MedicalFlex_count >0, 1, 0)

      ## Subgroup 5: Asking Medical questions on behalf
          my.dat$MedicalAskDoc_count = my.dat$MedicalDoctorQuestions == 2  # This subgroup only has 1 item
          my.dat$MedicalAskDoc_ever = ifelse(my.dat$MedicalAskDoc_count >0, 1, 0)
          
      ## Overall
          my.dat$lowstakeMed = rowSums(my.dat[ , c("MedicalMakeAppt","MedicalAccompanyAppt","MedicalTakeMeds","MedicalSeeTreatment")])
          my.dat$lowstakeMed_ever = ifelse(my.dat$lowstakeMed >0, 1, 0)
          my.dat$lowstakeMed_ever = factor(my.dat$lowstakeMed_ever,
                                           levels = c("0","1"),
                                           labels = c("0 - Never","1 - Helped with low stake medical decision before"))
          
          # High stake ACP SDM
          my.dat$highstakeMed_count = rowSums(my.dat[, c("MedicalDecision_count", "MedicalWorth_count", "MedicalEnd_count", 
                                                         "MedicalFlex_count", "MedicalAskDoc_count")])
          my.dat$highstakeMed_ever = ifelse(my.dat$highstakeMed_count >0, 1, 0)
          my.dat$highstakeMed2 = rowSums(my.dat[ , high_stake_columns]) # this Highstake2 contains ALL items Q6.14 - Q6.31 (13 items)
          my.dat$highstakeMed2
          
          # High stake ACP SDM Domain counts
          my.dat$highstakeMed_domain_count = rowSums(my.dat[, c("MedicalDecision_ever", "MedicalWorth_ever", "MedicalEnd_ever", 
                                                         "MedicalFlex_ever", "MedicalAskDoc_ever")])
          
          # Count the number of "No, because I do not need to"
          my.dat$highstakeMed_NOneed_count = rowSums(my.dat[ , high_stake_columns] == 0)
          my.dat$highstakeMed_NOneed_all = ifelse(my.dat$highstakeMed_NOneed_count == 13, 1, 0) 
          my.dat$highstakeMed_NOneed_all = factor(my.dat$highstakeMed_NOneed_all,
                                                  levels = c("0","1"),
                                                  labels = c("0 - Indicated some intention or performed actual SDM","1 - ALL No because no need"))
          
          # Count the number of "No, but I should have"
          my.dat$highstakeMed_should_count = rowSums(my.dat[ , high_stake_columns] == 1)
          
        ### Factorise all the EVER variables  
        my.dat <- my.dat %>%
          mutate(across(c(MedicalDecision_ever, MedicalWorth_ever, MedicalEnd_ever, 
                          MedicalFlex_ever, MedicalAskDoc_ever, highstakeMed_ever),
                        ~ factor(.,
                                 levels = c("0","1"),
                                 labels = c("0 - Never","1 - Done before"))))




# GROUPING DIGITAL DELEGATE HERE ====>> IMPORTANT-------------------------------------------------
    
  #** 01. IADL  *********************** 
    ## Q4.18.Digital tasks: These are tasks such as logging on to mobile apps, operating applications, checking emails, and changing settings on devices.
    ##      At the most frequent, I have helped another adult with digital tasks...
      my.dat$IADL_HelpDigital
  
  #** 02. FINANCE  ***********************
    ## Q5.15: I have a joint account (note: the raw values can be retrieved via 'FinanceJointAccount_raw' variable)
    ## Q5.15. Do you have any joint bank accounts with someone in order to help them with digital transactions?
      my.dat$FinanceJointAccount = factor(my.dat$FinanceJointAccount,
                                          levels = c("No","Yes","Unsure"),
                                          labels = c("0 - No joint account","1 - Yes joint account","0 - No joint account"))
    
    # Q5.17: I have helped with digital financial access
      my.dat$FinanceDigitalAssistance = factor(my.dat$FinanceDigitalAssistance,
                                               levels = c("No","Yes"),
                                               labels = c("0 - No did not assist","1 - Yes, assisted with legal/financial digital services"))
    
    # Q5.19: Own a separate account (note: the raw values can be retrieved via 'FinanceLoginWays' variable)
    ## Q5.19. How have you helped another adult to log on to their financial/legal digital services? You may select more than one option.
      ## Some how recoding of this variable does not work??
      my.dat$FinanceLoginWays
      my.dat$FinanceLoginInstitution
      my.dat$FinanceLoginInstitution[my.dat$FinanceLoginWays==""] = "0"
      
      # Generate 4 levels for finance delegation: 3 - Institution login > 2 - know username and password > 1 - the adults login on their own > 0 - not help at all (NAs)
      my.dat <- my.dat %>%
        mutate(FinanceLoginWays_4cat = ifelse(grepl("institution", FinanceLoginWays), 3,      # Highest level of delegation
                                      ifelse(grepl("I know the username and password", FinanceLoginWays), 2,
                                      ifelse(grepl("and I help them", FinanceLoginWays), 1, 
                                      ifelse(grepl("fingerprint", FinanceLoginWays), 1, 0)))))
        my.dat$FinanceLoginWays_4cat = factor(my.dat$FinanceLoginWays_4cat,
                                          levels = c("0","1","2","3"),
                                          labels = c("0 - Did not help digitally",
                                                     "1 - Helped but did not know login details",
                                                     "2 - Know username and password",
                                                     "3 - Separate institutional login and password to help online account(s) "))
      
        
    ## Q5.20. How have you helped another adult use their financial/legal digital services online? You may select more than one option.    
      my.dat <- my.dat %>%
      mutate(FinanceAssistWays_4cat = ifelse(grepl("even when they are not present", FinanceServiceWays), 3,      # Highest level of delegation
                                    ifelse(grepl("while they are present", FinanceServiceWays),2 ,
                                    ifelse(grepl("ongoing assistance", FinanceServiceWays), 1, 
                                    ifelse(grepl("mostly independently", FinanceServiceWays), 1, 0)))))
      
      my.dat$FinanceAssistWays_4cat = factor(my.dat$FinanceAssistWays_4cat,
                                            levels = c("0","1","2","3"),
                                            labels = c("0 - Did not help digitally",
                                                       "1 - Some help but mostly independent or ongoing help next to the person",
                                                       "2 - Use the person's account on their behalf, while they are present",
                                                       "3 - Use the person's account on their behalf, even when they are not present"))

      
      #---- 01. FinanceDelegate2 data preparation for easier regression output readings -----
      
      # Finance delegation2 -> Checking for consistency
      tabyl(subset(my.dat, (Country == "US" & FinanceDigitalAssistance=="1 - Yes, assisted with legal/financial digital services")), FinanceJointAccount, FinanceDelegate2)
      tabyl(subset(my.dat, (Country == "US" & FinanceDigitalAssistance=="0 - No did not assist")), FinanceJointAccount, FinanceDelegate2)
      
      'something went wrong with the Singapore data of this variable *********'
      tabyl(subset(my.dat, (Country == "SG" & FinanceDigitalAssistance=="1 - Yes, assisted with legal/financial digital services")), FinanceJointAccount, FinanceDelegate2) #a lot have joint accounts but were classified as informal'
      tabyl(subset(my.dat, (Country == "SG" & FinanceDigitalAssistance=="0 - No did not assist")), FinanceJointAccount, FinanceDelegate2)
      
      'Replace FinanceDelegate2 variable for SG to match the definition used in US -> joint account means Assis-Formal'
        my.dat$FinanceDelegate2[my.dat$Country == "SG" & my.dat$FinanceDigitalAssistance == "1 - Yes, assisted with legal/financial digital services" &
                 (my.dat$FinanceJointAccount == "1 - Yes joint account" | my.dat$FinanceLoginWays_institution == "Yes")] = "Assist-formal"
      
      my.dat$FinanceDelegate2[my.dat$FinanceDelegate2 == "Exclude"] <- NA # exclude because of contradictory 'FinanceJointAccount_raw' inputs but FinanceDigitalAssistance == "Yes"
      my.dat$FinanceDelegate2 <- factor(my.dat$FinanceDelegate2,
                                         levels = c("No","Assist-Informal","Assist-formal"),
                                         labels = c("0 - No",
                                                    "2 - Assist-Informal",
                                                    "3 - Assist-Formal"))
      my.dat$FinanceDelegate2 <- relevel(my.dat$FinanceDelegate2, ref = "0 - No")
  
  #** 03. MEDICAL  ***********************
    ## Q6.33: I have a joint account
    ## Q6.33. A caregiver account is a digital account where you have access to another adult's medical records. Have you ever had a caregiver account for that adult?
      my.dat$DigitalCaregiverAccount = factor(my.dat$DigitalCaregiverAccount,
                                              levels = c("Unsure","No, but I should have","Yes"),
                                              labels = c("0 - No account","0 - No account","1 - Yes caregiver account"))
      #my.dat$DigitalCaregiverAccount <- as.numeric(as.character(my.dat$DigitalCaregiverAccount))
      
    # Q6.36: I have assisted another adult with digital medical services
      my.dat$DigitalAssistMedical = factor(my.dat$DigitalAssistMedical,
                                           levels = c("No",	"Yes"),
                                           labels = c("0 - Never assisted",	"1 - Yes assisted"))
      #my.dat$DigitalAssistMedical <- as.numeric(as.character(my.dat$DigitalAssistMedical))
    
    # Q6.38: My own separate account(note: the raw values can be retrieved via 'DigitalAssistAccessWays' variable)
      my.dat <- my.dat %>%
        mutate(MedicalLoginWays_4cat = ifelse(grepl("institution", DigitalAssistAccessWays), 3,      # Highest level of delegation
                                      ifelse(grepl("I know the username and password", DigitalAssistAccessWays), 2,
                                      ifelse(grepl("and I help them", DigitalAssistAccessWays), 1, 
                                      ifelse(grepl("fingerprint", DigitalAssistAccessWays), 1, 0)))))
        
        my.dat$MedicalLoginWays_4cat = factor(my.dat$MedicalLoginWays_4cat,
                                               levels = c("0","1","2","3"),
                                               labels = c("0 - Did not help digitally",
                                                          "1 - Helped but did not know login details",
                                                          "2 - Know username and password",
                                                          "3 - Separate institutional login and password to help online account(s) "))
        

    # Q6.39. How have you helped another adult use their digital medical services online? You may select more than one option.
      ## (note: the raw values can be retrieved via 'DigitalAssistAccessWays' variable)
      my.dat <- my.dat %>%
        mutate(MedicalAssistWays_4cat = ifelse(grepl("even when they are not present", DigitalAssistUseWays), 3,      # Highest level of delegation
                                               ifelse(grepl("while they are present", DigitalAssistUseWays),2 ,
                                                      ifelse(grepl("ongoing assistance", DigitalAssistUseWays), 1, 
                                                             ifelse(grepl("mostly independently", DigitalAssistUseWays), 1, 0)))))
      
      my.dat$MedicalAssistWays_4cat = factor(my.dat$MedicalAssistWays_4cat,
                                             levels = c("0","1","2","3"),
                                             labels = c("0 - Did not help digitally",
                                                        "1 - Some help but mostly independent or ongoing help next to the person",
                                                        "2 - Use the person's account on their behalf, while they are present",
                                                        "3 - Use the person's account on their behalf, even when they are not present"))

      
      #---- 02. MedicalDelegate2 data preparation for easier regression output readings -----  
      # Medical delegation -> Checking for consistency
      tabyl(subset(my.dat, (Country == "US" & DigitalAssistMedical=="1 - Yes assisted")), DigitalCaregiverAccount, MedicalDelegate2)
      tabyl(subset(my.dat, (Country == "US" & DigitalAssistMedical=="0 - Never assisted")), DigitalCaregiverAccount, MedicalDelegate2)
      
      tabyl(subset(my.dat, (Country == "SG" & DigitalAssistMedical=="1 - Yes assisted")), DigitalCaregiverAccount, MedicalDelegate2)
      tabyl(subset(my.dat, (Country == "SG" & DigitalAssistMedical=="0 - Never assisted")), DigitalCaregiverAccount, MedicalDelegate2)    
      
      my.dat$MedicalDelegate2 <- factor(my.dat$MedicalDelegate2,
                                         levels = c("No","Assist-informal","Assist-formal"),
                                         labels = c("0 - No",
                                                    "2 - Assist-Informal",
                                                    "3 - Assist-Formal"))
      my.dat$MedicalDelegate2 <- relevel(my.dat$MedicalDelegate2, ref = "0 - No")  
      
#============Imputing Missing Values===
  test_numeric <- my.dat %>%
      dplyr::select(Age,Gender,Citizen,Education,Income,Employment,Housing,Religion,
             Siblings,SiblingsOrder,Ethnicity,
             Marital,ADL,IADL,FinanceLPA,DigitalCaregiverAccount,FinanceDigitalAssistance)
  

    md.pattern(test_numeric)
