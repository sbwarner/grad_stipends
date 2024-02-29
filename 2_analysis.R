#############################
# Replication for:
# PhD Stipends and Program Placement Success in PS
#
# Script 2 - Analyses
#############################

library(MASS)

###
# 1. Distribution of key variables

# Stipends
hist(analysis$stipend, breaks = 20, xlab = "Mean Stipend (in thousands)", ylab = "Numer of programs",
     col = "grey", main = "Figure 1. Estimated Mean PhD Stipends in 85 Political Science Departments")

# Placements
hist(analysis$alltenure, breaks = 15, ylab = "Frequency", xlab = "Number of placements",
     main = "Figure 2. Distribution of tenure-track placements by program (2019-2021)")


###
# 3. Create weights for main analysis based on propensity to report
responsefit <- glm(listed ~ stipend + rank_rev + 
                     type + region, data = analysis, family = "binomial")
summary(responsefit)
NROW(responsefit$model)
analysis$weight <- 1 / responsefit$fitted.values


###
# 4. Main analyses

# Number of placements
summary(lm(alltenure ~ stipend + rank_decile + log_endow + size + union + type + factor(metro_rating), data = analysis, weight = weight))
summary(lm(ustenure ~ stipend + rank_decile + log_endow + size + union + type + factor(metro_rating), data = analysis, weight = weight))

# Placements per 100
summary(lm(allrt ~ stipend + rank_decile + log_endow + size + union + type + factor(metro_rating), data = analysis, weight = weight))
summary(lm(usrt ~ stipend + rank_decile + log_endow + size + union + type + factor(metro_rating), data = analysis, weight = weight))

# Using COL-adjusted pay
summary(lm(alltenure ~ coipay_std + rank_decile + log_endow + size + union + type + factor(metro_rating), data = analysis, weight = weight))
summary(lm(allrt ~ coipay_std  + rank_decile + log_endow + size + union + type + factor(metro_rating), data = analysis, weight = weight))
summary(lm(ustenure ~ coipay_std + rank_decile + log_endow + size + union + type + factor(metro_rating), data = analysis, weight = weight))
summary(lm(usrt ~ coipay_std  + rank_decile + log_endow + size + union + type + factor(metro_rating), data = analysis, weight = weight))


###
# 5. Appendix

# Negative binomial
summary(glm.nb(alltenure ~ stipend + metro_rating + rank_decile + size + union + log_endow + type, data = analysis, weight = weight))
summary(glm.nb(ustenure ~ stipend + metro_rating + rank_decile + size + union + log_endow + type, data = analysis, weight = weight))

# TT ratio
summary(lm(tenure_ratio ~ stipend + type + size + rank_decile + union + log_endow, data = analysis, weight = weight))
summary(lm(us_tenure_ratio ~ stipend + type + size + rank_decile + union + log_endow, data = analysis, weight = weight))

