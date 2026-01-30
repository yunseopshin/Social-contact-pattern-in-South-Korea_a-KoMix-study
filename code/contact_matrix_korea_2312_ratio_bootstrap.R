################################################################################
# Bootstrap analysis for R0 ratio estimation
# Comparing two time points
# Data: KoMix (2023.11) vs. baseline
################################################################################

# Load required libraries
library(stringr)
library(dplyr)

################################################################################
# Data preparation - Current time point (2023.11)
################################################################################

setwd('C:/project/Social-contact-pattern-in-South-Korea-a-KoMix-study/data/Survey on Changes in Contact Patterns After COVID-19 (23.11)')
data_current = read.csv('Data(2311).csv', fileEncoding = "utf-8")

# Column index for contact age groups (50 contacts maximum)
contact_num_index = 8:57  # Q8A_1_1 to Q8A_50_1

# Replace NA with 0 for COVID-19 vaccination data
data_current$Q3A[is.na(data_current$Q3A)] = 0

# Extract contact data columns:
# SQ2(1), SQ5AA(2), Q8(7), Q8A_1_1~Q8A_50_1(8-57), Q4(5), Q7(6), Q2(3), Q3A(4)
contact_data_current = data_current[, c(1, 2, 7, contact_num_index, 5, 6, 3, 4)]
contact_data_current[is.na(contact_data_current)] = 0

################################################################################
# Data preparation - Baseline time point
# NOTE: Modify this section to load your baseline data
################################################################################

# Option 1: Use different time as baseline (e.g., 2021.12)
# data_base = read.csv('Data(2112).csv', fileEncoding = "utf-8")

# Option 2: For testing, use the same data as baseline (replace with actual baseline)
data_base = read.csv('Data(2311).csv', fileEncoding = "utf-8")

data_base$Q3A[is.na(data_base$Q3A)] = 0
contact_data_base = data_base[, c(1, 2, 7, contact_num_index, 5, 6, 3, 4)]
contact_data_base[is.na(contact_data_base)] = 0

################################################################################
# Load Korea population structure
################################################################################

setwd('C:/project/Social-contact-pattern-in-South-Korea-a-KoMix-study/data')
skage = read.csv('skage.csv', header = T)
skage = skage[1:101, 5]
skage = as.numeric(sapply(skage, function(x) gsub(',', '', x)))

################################################################################
# R0 calculation function
################################################################################

R0_function = function(contact_data) {
    tryCatch({
        # Initialize contact matrices
        phi = matrix(0, nrow = 10, ncol = 10)
        phi_week = matrix(0, nrow = 10, ncol = 10)
        phi_weekend = matrix(0, nrow = 10, ncol = 10)
        
        # Age group boundaries
        age = c(0, 3, 7, 13, 16, 19, 30, 40, 50, 60)
        
        # Filter weekday and weekend data
        contact_week = contact_data %>% filter(SQ5AA == 5)
        contact_weekend = contact_data %>% filter(SQ5AA == 2)
        
        # Construct contact matrices by age group
        for(i in 1:10) {
            if(i != 10) {
                temp = contact_data %>% filter(SQ2 >= age[i] & SQ2 < age[i+1])
                temp_week = contact_week %>% filter(SQ2 >= age[i] & SQ2 < age[i+1])
                temp_weekend = contact_weekend %>% filter(SQ2 >= age[i] & SQ2 < age[i+1])
            } else {
                temp = contact_data %>% filter(SQ2 >= age[10])
                temp_week = contact_week %>% filter(SQ2 >= age[10])
                temp_weekend = contact_weekend %>% filter(SQ2 >= age[10])
            }
            
            for(j in 1:10) {
                if(j != 10) {
                    phi[i, j] = sum(sapply(1:nrow(temp), function(x) length(which(temp[x, 4:53] == j)))) / nrow(temp)
                    phi_week[i, j] = sum(sapply(1:nrow(temp_week), function(x) length(which(temp_week[x, 4:53] == j)))) / nrow(temp_week)
                    phi_weekend[i, j] = sum(sapply(1:nrow(temp_weekend), function(x) length(which(temp_weekend[x, 4:53] == j)))) / nrow(temp_weekend)
                } else {
                    phi[i, j] = sum(sapply(1:nrow(temp), function(x) length(which(temp[x, 4:53] >= j)))) / nrow(temp)
                    phi_week[i, j] = sum(sapply(1:nrow(temp_week), function(x) length(which(temp_week[x, 4:53] >= j)))) / nrow(temp_week)
                    phi_weekend[i, j] = sum(sapply(1:nrow(temp_weekend), function(x) length(which(temp_weekend[x, 4:53] >= j)))) / nrow(temp_weekend)
                }
            }
        }
        
        # Weighted average of weekday and weekend contacts
        phi_res = (5/7) * phi_week + (2/7) * phi_weekend
        
        # Calculate population distribution by age group
        n = rep(0, 10)
        for(i in 1:9) {
            n[i] = sum(skage[(age[i]+1):(age[i+1])])
        }
        n[10] = sum(skage[61:101])
        
        # Symmetric contact matrix (Sebastian method)
        phi_res_sb = matrix(0, nrow = 10, ncol = 10)
        for(i in 1:10) {
            for(j in 1:10) {
                phi_res_sb[i, j] = (phi_res[i, j] * n[i] + phi_res[j, i] * n[j]) / (2 * n[i])
            }
        }
        
        # Next Generation Matrix (NGM)
        NGM = matrix(0, nrow = length(n), ncol = length(n))
        for(i in 1:length(n)) {
            for(j in 1:length(n)) {
                NGM[i, j] = phi_res_sb[i, j] * n[i] / n[j]
            }
        }
        
        # Return R0 (largest eigenvalue of NGM)
        return(eigen(NGM)$values[1])
    }, error = function(e) {
        return(NA)
    })
}

################################################################################
# R0 ratio calculation function
################################################################################

R0_ratio_function = function(contact_data_current, contact_data_base) {
    tryCatch({
        R0_current = R0_function(contact_data_current)
        R0_base = R0_function(contact_data_base)
        
        # Check for invalid values
        if(is.infinite(R0_current) || is.nan(R0_current) || 
           is.infinite(R0_base) || is.nan(R0_base) || R0_base == 0) {
            return(NA)
        }
        
        R0_ratio_value = R0_current / R0_base
        return(R0_ratio_value)
    }, error = function(e) {
        return(NA)
    })
}

# Calculate original R0 values
R0_current_original = R0_function(contact_data_current)
R0_base_original = R0_function(contact_data_base)
R0_ratio_original = R0_ratio_function(contact_data_current, contact_data_base)

cat("=== Original R0 Values ===\n")
cat("R0 (Current):", R0_current_original, "\n")
cat("R0 (Baseline):", R0_base_original, "\n")
cat("R0 Ratio:", R0_ratio_original, "\n\n")

################################################################################
# Bootstrap analysis for R0 ratio
################################################################################

set.seed(2025)  # For reproducibility
B <- 1000  # Number of bootstrap iterations

# Initialize progress bar
cat("Bootstrap in progress...\n")
pb_counter_ratio <- 0
pb_ratio <- txtProgressBar(min = 0, max = B, style = 3, char = "=")

# Get sample sizes
n_current <- nrow(contact_data_current)
n_base <- nrow(contact_data_base)

# Bootstrap iterations
boot_values_ratio <- numeric(B)

for(i in 1:B) {
    tryCatch({
        # Bootstrap sample for current time point
        indices_current <- sample(1:n_current, size = n_current, replace = TRUE)
        boot_data_current <- contact_data_current[indices_current, , drop = FALSE]
        
        # Bootstrap sample for baseline time point
        indices_base <- sample(1:n_base, size = n_base, replace = TRUE)
        boot_data_base <- contact_data_base[indices_base, , drop = FALSE]
        
        # Calculate R0 ratio
        R0_ratio_value <- R0_ratio_function(boot_data_current, boot_data_base)
        boot_values_ratio[i] <- R0_ratio_value
        
        # Update progress bar
        pb_counter_ratio <- pb_counter_ratio + 1
        setTxtProgressBar(pb_ratio, pb_counter_ratio)
    }, error = function(e) {
        boot_values_ratio[i] <- NA
        pb_counter_ratio <- pb_counter_ratio + 1
        setTxtProgressBar(pb_ratio, pb_counter_ratio)
    })
}

# Close progress bar
close(pb_ratio)

################################################################################
# Results
################################################################################

cat("\n\n=== Bootstrap Results for R0 Ratio ===\n")
cat("R0 Ratio (Original):", R0_ratio_original, "\n")
cat("R0 Ratio (Bootstrap Mean):", mean(boot_values_ratio, na.rm = TRUE), "\n")

# Calculate standard deviation
R0_ratio_sd = sd(boot_values_ratio, na.rm = TRUE)
cat("Standard Deviation (SD):", R0_ratio_sd, "\n")

# Calculate 95% confidence interval (percentile method)
R0_ratio_CI <- quantile(boot_values_ratio, probs = c(0.025, 0.975), na.rm = TRUE)
cat("95% Confidence Interval (Percentile): [", R0_ratio_CI[1], ", ", R0_ratio_CI[2], "]\n")

# Calculate 95% confidence interval (normal approximation)
R0_ratio_normal_CI <- c(R0_ratio_original - 1.96 * R0_ratio_sd, 
                        R0_ratio_original + 1.96 * R0_ratio_sd)
cat("95% Confidence Interval (Normal Approximation): [", R0_ratio_normal_CI[1], ", ", R0_ratio_normal_CI[2], "]\n")

################################################################################
# Summary table
################################################################################

result_summary_ratio <- data.frame(
    R0_ratio_original = R0_ratio_original,
    R0_ratio_bootstrap_mean = mean(boot_values_ratio, na.rm = TRUE),
    SD = R0_ratio_sd,
    CI_lower_percentile = R0_ratio_CI[1],
    CI_upper_percentile = R0_ratio_CI[2],
    CI_lower_normal = R0_ratio_normal_CI[1],
    CI_upper_normal = R0_ratio_normal_CI[2]
)

cat("\n=== Summary Table ===\n")
print(result_summary_ratio)

################################################################################
# Optional: Save results
################################################################################

# write.csv(result_summary_ratio, "R0_ratio_bootstrap_result.csv", row.names = FALSE)
