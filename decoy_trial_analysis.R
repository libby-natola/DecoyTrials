###### Script to analyse decoy trial data
###### Analysis by Libby Natola 24 June 2020

### set working directory
setwd("~/Documents/UBC/Field Work/Data Collection/SapsuckerRecordings/DecoyTrials")

### library packages
library(ggplot2)

### read in decoy trial data
decoys <- read.csv("decoy_trials_data.csv")

########### compare closest approach (continuous numerical, response) by decoy type (categorical, explanatory)

### first I'm going to visualize closest approach by decoy types in facet wrapped histograms
ggplot(decoys, aes(x=closest_approach)) + geom_histogram() + facet_wrap(~decoy_type)

### I need to do a two-sample t test
t.test(decoys$closest_approach ~ decoys$decoy_type)
Welch Two Sample t-test

# data:  decoys$closest_approach by decoys$decoy_type
# t = -2.264, df = 37.082, p-value = 0.02951
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -4.256913 -0.236161
# sample estimates:
#   mean in group P mean in group T 
# 2.377273        4.623810 

### now I'm going to visualize number of attacks by decoy types in histogram
ggplot(decoys, aes(x=decoy_attacked)) + geom_histogram(stat="count") + facet_wrap(~decoy_type) 

### make a contingency table of attacked vs decoy type
attack_table <- table(decoys$decoy_attacked, decoys$decoy_type)

# P  T
# n 20 20
# y  2  1

### run fisher's exact test because of low cell values

fisher.test(attack_table)

# Fisher's Exact Test for Count Data
# 
# data:  attack_table
# p-value = 1
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
# 0.008084048 10.493349395
# sample estimates:
# odds ratio 
# 0.5078594 



### now I'm going to visualize number of approaches on tree in facet wrapped histograms
ggplot(decoys, aes(x=num_approaches_on_tree)) + geom_histogram() + facet_wrap(~decoy_type)

### I need to do a two-sample t test
t.test(decoys$num_approaches_on_tree ~ decoys$decoy_type)

#Welch Two Sample t-test

# data:  decoys$num_approaches_on_tree by decoys$decoy_type
# t = 1.4674, df = 33.188, p-value = 0.1517
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.32367  1.99986
# sample estimates:
#   mean in group P mean in group T 
# 2.571429        1.733333 

### maybe I should just do a y/n did the bird land on the tree? I will make a new column that will say TF
decoys$tree_tf <- print(decoys$num_approaches_on_tree > 0)

### now I'm going to visualize number landed on tree by decoy types in histogram
ggplot(decoys, aes(x=tree_tf)) + geom_histogram(stat="count") + facet_wrap(~decoy_type) 

### make a contingency table of landed on tree vs decoy type
tree_table <- table(decoys$tree_tf, decoys$decoy_type)

print(tree_table)
# 
# P  T
# FALSE  7  8
# TRUE  15 13

### chisquare test

chisq.test(tree_table)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tree_table
# X-squared = 0.012465, df = 1, p-value = 0.9111

