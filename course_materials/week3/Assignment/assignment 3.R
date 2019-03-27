# week 3 assignment

# 1. Try creating maps of risk ratios with these 
# new case-control data (simulated)
nairobi_cases <- read.csv("cases_nairobi.csv")

# 3. Open this dataset of hookworm in Uganda, 
# and compare the best IDW surface to a kriged prevalence across 
# the window. 
HK<-read.csv("tanzania_uganda_hkprev.csv")
# HINT 1: when using the logit transform, 
# you may need to add a small amount
# (i.e 0.001) to any values of 0 prevalence

# HINT 2: when finding the best IDW surface,
# try using multiple different powers. You 
# can do this mannually or you can write a function
# (if you know how to write a function!)

