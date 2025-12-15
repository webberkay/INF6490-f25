# load data 
data("ToothGrowth")
head(ToothGrowth)

# check structure 
str(ToothGrowth)

# summary statistics 
aggregate(len ~ supp, data = ToothGrowth, mean)

# two-sample t-test 
t_test_result <- t.test(len ~ supp, data = ToothGrowth, var.equal = FALSE)
t_test_result

#confidence interval 
t_test_result$conf.int