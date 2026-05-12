library(rstan)
library(ggplot2)
library(scales)
library(jtools)
library(interactions)
library(MASS)
library(GGally)
library(ppcor)
library(grafify)
library(ggpubr)
library(Hmisc)
library(agricolae)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(e1071)
library(outliers)
library(MASS)
library(klaR)
library(caret)
library(MVN)
require(graphics)
library(pROC)
library(corrplot)
library(apyramid)
library(nnet)
library(magick)
#library(summarytools) # outdated package
library(mice)
library(knitr)
library(psych)
library(tables)
library(pivottabler)
library(caTools)
library(car)
library(quantmod)
library(openxlsx)
library(coin)
library(dplyr)
library(table1)
library(flextable)
library(janitor) # for summary statistics
library(broom) # for displaying 95%CI regression results
library(modelsummary) # for model summary and export
library(ordinal) # for ordered logit regression
library(margins)
library(marginaleffects)
library(patchwork)
library(forcats)
library(DT)
library(gt)

# function to transform factor levels and labels
transform_factor <- function(data, column_name, na = NULL, levels, labels, ordered = FALSE, convert_to_numeric = FALSE) {
  if (!is.null(na)) {
    data[[column_name]][is.na(data[[column_name]])] <- na
  }
  data[[column_name]] <- factor(data[[column_name]], levels = levels, labels = labels, ordered = ordered)
  if (convert_to_numeric) {
    data[[column_name]] <- as.numeric(data[[column_name]])  # convert factor directly to numeric
  }
  return(data)
}

# function to transform ordered factor to numeric
transform_order_to_numeric <- function(data, column_name, ordering) {
  orderFactor <- factor(data[[column_name]], levels = ordering)
  data[[column_name]] <- as.numeric(orderFactor)
  return(data)
}

# function to filter out a specific country from the dataset
country_data <- function(data, country) {
  temp_data <- subset(data, !(Country == country))
  return(temp_data)
}

# function to remove outliers
remove_outliers <- function(data, conditions, outlier_values) {
  for (i in seq_along(conditions)) {
    data <- data[!data[[conditions[i]]] %in% outlier_values[[i]], ]
  }
  return(data)
}

# function to repeat rowSums,scale_values,round
calculate_and_process_scores <- function(data, columns, scale = TRUE, digits = 2) {
  scores <- rowSums(data[, columns], na.rm = TRUE)
  if (scale) {
    scores <- scale_values(scores)}
  scores <- round(scores, digits = digits)
  return(scores)}

convert_to_factors <- function(data, columns) {
  for (col in columns) {
    data[[col]] <- as.factor(data[[col]])}
  return(data)}

# Function to calculate row sums for specified columns and round the result to 2 digits
calculate_and_process_sums <- function(data, column_groups) {
  for (group_name in names(column_groups)) {
    data[[group_name]] <- rowSums(data[, column_groups[[group_name]]])
  }
  return(data)}

# function to factorize IADL and ADL columns
transform_IADL_ADL <- function(data, columns, levels, labels) {
  for (col in columns) {
    data[[col]] <- factor(data[[col]], levels = levels, labels = labels)
    data[[col]] <- as.numeric(as.character(data[[col]]))
  }
  return(data)
}


# Create table 1 Summary statistics
pvalue <- function(x, ...) {
  x <- x[-length(x)]  # Remove "overall" group
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

# More general pvalue function that also allows for >2 groups comparison (using ANOVA)
pvalue2 <- function(x, ...) {
  x <- x[-length(x)]  # Remove "overall" group
  
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(seq_along(x), times = sapply(x, length)))
  
  # Optional: guard against degenerate cases
  if (length(y) == 0 || nlevels(g) < 2) {
    return(c("", NA_character_))
  }
  
  if (is.numeric(y)) {
    # For numeric variables: t-test for 2 groups, ANOVA for >2 groups
    if (nlevels(g) == 2) {
      p <- t.test(y ~ g)$p.value
    } else {
      p <- summary(aov(y ~ g))[[1]][["Pr(>F)"]][1]
      # alternatively: p <- anova(lm(y ~ g))$`Pr(>F)`[1]
    }
  } else {
    # For categorical variables: chi-squared test of independence (2+ groups)
    p <- chisq.test(table(y, g))$p.value
  }
  
  c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}
