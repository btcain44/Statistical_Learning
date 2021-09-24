###Part A:-------------EXAMINE DATASET VARIABLES---------------------------------

###Load the data into the environment
load("D:\\Stat_Learning\\spam.RData") ##Loads as dat in the environment

##Examine structure of the dataset
str(dat)
####All variables are of numeric type indicating the data types are correct (all variables are numerical according to dataset description)

##Define a function to perform z-tests (data should be filtered into x1 and x2 before being entered into function)
z_test <- function(x1, x2) {
  
  ##Obtain the length of each vector
  n1 = length(x1)
  n2 = length(x2)
  
  ##Compute sample means
  x1_bar = mean(x1)
  x2_bar = mean(x2)
  
  ##Compute sample standard deviation
  x1_std = var(x1)**2
  x2_std = var(x2)**2
  
  ##Calculate the z-statistic
  z = (x1_bar-x2_bar)/sqrt(x1_std/n1+x2_std/n2)
  return(x1_bar)
  ##Conditional to assess if the feature is significant
  #if (z<1.96) {return('Yes')} else {return('no')}
}
##Help with the function: https://cran.r-project.org/web/packages/distributions3/vignettes/two-sample-z-test.html

##Perform for loop to test if features are significant with the defined z_test function
covariates = list(names(dat))
covariates = covariates[covariates != 'spam'] ##Filter out response variable
for (i in covariates) {
  
  ##Create vectors
  x1 = dat[which(dat$spam==0),i]
  x2 = dat[which(dat$spam==1),i]
  
  ##Perform z-test
  #z_result = z_test(x1,x2)
  
  ##Display z-test results
  #print(paste(i,': ',z_result))
  
}

