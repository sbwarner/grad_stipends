#############################
# Replication for:
# PhD Stipends and Program Placement Success in PS
#
# Script 1 - Data preparation
#############################


library(readxl)
library(dplyr)

###
# 1. Merge stipend and program data

programs <- read_excel("programs.xlsx")
stipends <- read_excel("stipends.xlsx")

mean_stipends <- stipends %>% 
  group_by(program) %>%
  summarize(stipend_sd = sd(stipend, na.rm = T),
            stipend = mean(stipend), 
            coiadj_pay = mean(coiadj_pay, na.rm = T), 
            nsubj = n())

analysis <- merge(programs, mean_stipends, by = "program", all.x = T)


###
# 2. Create variables

# Placements per 100 enrolled grad students
analysis$allrt <- (analysis$alltenure / analysis$size)*100
analysis$usrt <- (analysis$ustenure / analysis$size)*100

# TT placements as a percentage of all grad placements
analysis$tenure_ratio <- analysis$alltenure / (analysis$alltenure + analysis$non_tt)
analysis$us_tenure_ratio <- analysis$ustenure / (analysis$ustenure + analysis$non_tt)

# Stipend in thousands
analysis$stipend <- analysis$stipend/1000

# Cost of living
analysis$col <- analysis$stipend / analysis$coiadj_pay

# COI-adjusted stipends, standardized
analysis$coipay_std <- scale(analysis$coiadj_pay)[,1]

# Logged stipend per student
analysis$log_endow <- log(analysis$endowment_fte)

# Rank reversed so higher ~ stronger
analysis$rank_rev <- abs(analysis$rank13 - max(analysis$rank13))

# Rank to deciles
breaks <- quantile(analysis$rank_rev, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
labels <- 10:1
analysis <- analysis %>%
  mutate(rank_decile = ntile(rank_rev, 10))


### Do not clear environment and proceed to Script 2 ###