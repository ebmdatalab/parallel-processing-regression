options(scipen=10)
options(digits=6)

# create a dataset
row <- 25000000
age_range <- 40:96

df <- data.frame(
  outcome = sample(c(0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), row, replace=TRUE),
  age = sample(age_range, row, replace=TRUE), 
  gender = sample(c('MALE', 'FEMALE'), row, replace=TRUE), 
  smoking = sample(c(0, 1, 2), row, replace=TRUE),
  any_disease = sample(c(0,1), row, replace=TRUE), 
  any_med = sample(c(0,1), row, replace=TRUE), 
  disease1 = sample(c(0, 0, 0, 0, 0, 0, 0, 1), row, replace=TRUE),
  disease2 = sample(c(0, 0, 0, 0, 0, 0, 0, 1), row, replace=TRUE), 
  disease3 = sample(c(0, 0, 0, 0, 0, 0, 0, 1), row, replace=TRUE), 
  time = sample(0:30, row, replace=TRUE)
  )

df$time <- ifelse(df$outcome==1, df$time, 0)

# run basic cox regression (univariate)
res1 <- coxph(Surv(time, outcome) ~ gender, data=df)
summary(res1)

# run multicox regression
## start time
time_in = Sys.time()

# regression model
res2 <- coxph(Surv(time, outcome) ~ age + gender + disease1, data = df)
summary(res2)

time_out = Sys.time()

test_time = time_out - time_in
test_time