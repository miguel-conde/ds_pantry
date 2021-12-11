# http://www.dagitty.net/learn/primer/Dagitty-Cheat-Sheet.pdf

# https://currentprotocols.onlinelibrary.wiley.com/doi/full/10.1002/cpz1.45

# http://www.dagitty.net/
# http://www.dagitty.net/dags.html
# http://www.dagitty.net/primer/


library(tidyverse)

# Basic Protocol 2: TESTING DAGs AGAINST CATEGORICAL DATA -----------------

library( dagitty )

myDAG <- dagitty( 'dag {
Age -> Irradiation
Age -> Menopause
Irradiation -> Recurrence
Menopause -> Recurrence
}' )

plot( myDAG )

# download at: https://github.com/ankurankan/2020-dagitty-manual/
data <- read.table( here::here("dagitty", "data", "brca.txt"), header=TRUE )


# Test if implied independencies are contradicted by the data (Fig. 4). To do 
# this, we will first generate a list of the independencies that are implied by 
# the DAG.

impliedConditionalIndependencies( myDAG )

# This will show that there are two conditional independencies that are implied: 
# “Age ⊥ Recurrence | Irradiation, Menopause” and “Irradiation ⊥ Menopause | Age.” 
# To test these, we use the command ciTest as follows:
  
ciTest( "Age", "Recurrence", c( "Irradiation", "Menopause" ),
          data, type= "cis.chisq" )
ciTest( "Irradiation", "Menopause", "Age", data, type="cis.chisq" )

# Since the variables are categorical, a chi-square test is used to test for 
# correlations between them. If one of the independencies is contradicted, this 
# is reflected by a low p-value and high RMSEA and χ2. See Guidelines for 
# Understanding Results for more information.

# There is an alternative method available that automatically lists relevant 
# independencies implied by the input DAG, tests them, and returns all results 
# in a single table (shown in Fig. 4):
  
  localTests( myDAG, data, type="cis.chisq" )
  
# In this case, the results do not support the first independence, 
# “Age ⊥ Recurrence | Irradiation, Menopause.” This could mean, for example, 
# that a direct effect exists from Age to Recurrence that is not mediated by 
# either Irradiation or Menopause. It could also mean that one or both of the 
# variables “Irradiation” or “Menopause” have not been measured sufficiently 
# precisely to capture the corresponding indirect effects in full, which can 
# create the illusion of a residual direct effect. See Guidelines for 
# Understanding Results for more information.
  

# Basic Protocol 4: TESTING DAGs AGAINST A COMBINATION OF CATEGORI --------

# adult.csv (download dataset at: https://github.com/ankurankan/2020-dagitty-manual/

data <- read.csv( here::here("dagitty", "data", "adult.csv" ))
  
## Define the ordinal categorical variables. The categories of Age, Education, 
# HoursPerWeek, and Income in our dataset have a natural ordering, hence they are 
# defined as ordinal variables.
  
data$Age <- ordered( data$Age,
                     levels=c( "<20", "20-34", "35-49", "50-65", ">65" ) )
data$Education <- ordered(data$Education,
                          levels=c( "Non-HS-Grad", "HS-grad",
                                    "College-Associate", "Academic-Degree" ) )
data$HoursPerWeek <- ordered( data$HoursPerWeek,
                              levels=c( "<20", "20-39", "40", ">40" ) )
data$Income <- ordered(data$Income,
                         levels=c( "<=50K", ">50K" ) )
  
## Define binary variables. The rest of the variables in the dataset are binary 
# and we will convert them to integers. This will assign an arbitrary order to 
# each variable: the lexicographically smaller value will be assigned a 1, and 
# the larger value a 2. For example, the variable “Immigrant” has the values “no” 
# and “yes,” so “no” will be translated to 1 and “yes” to 2.
  
data$Race <- as.integer( factor(data$Race) )
data$Sex <- as.integer( factor(data$Sex ))
data$Immigrant <- as.integer( factor(data$Immigrant ))
  
## Optional: Deal with non-ordinal categorical variables with more than 2 
# categories. In the last step, we defined the non-ordinal variables with two 
# categories as binary but this does not work for variables with more than 2 
# categories, and they need to be dummy encoded. In our dataset, we have the 
# variable Marital Status with the categories: Is-Married, Never-Married, 
# Was-Married. We decide to merge the two categories Never-Married and 
# Was-Married into a single category and rename the resulting two categories, 
# which can be accomplished as follows:
    
levels(data$MaritalStatus) <- list(
  Married="Is-Married",
  NotMarried=c( "Was-Married", "Never-married" ) )
    
# Then we convert this variable to a binary number like previously:
    
data$MaritalStatus <- as.integer( factor(data$MaritalStatus ))
    
# Here, 1 will now mean “Married” and 2 will mean “Not Married.”
  
# Compute the correlation matrix. We can use the lavCor function implemented in 
# the lavaan package to compute the polychoric correlation matrix from the 
# dataset as:
    
library( lavaan )
corr <- lavCor( data )

# In the current version of lavaan (0.6), the function lavCor gives a warning 
# message: “estimation of the baseline model failed.” This warning can be safely 
# ignored.
  
# Create the model structure. We can use the dagitty web interface to create the 
# model structure and import it to R as shown in Basic Protocol 1. Below, we 
# directly give an abbreviated DAG code for reference.
  
library( dagitty )

model <- dagitty( 'dag {
bb="-4.6,-3.8,3.7,3.7"
Age [pos="-2,-2.6"]
Education [pos="1.4,0.5"]
HoursPerWeek [pos="-0.6,-0.1"]
Immigrant [pos="1.1,-2.6"]
Income [pos="-1.6,2.7"]
MaritalStatus [pos="-3.7,-2.6"]
Race [pos="3.0,-2.5"]
Sex [pos="-0.5,-2.6"]
Age -> { Education HoursPerWeek Income MaritalStatus }
Education -> Income
HoursPerWeek -> Income
Immigrant -> { Education HoursPerWeek }
Immigrant <-> Race
MaritalStatus -> Income
Race -> Education
Sex -> { Education HoursPerWeek }
}' )

# The imported model can be plotted in R using plot( model ) to verify the 
# structure. The plot for the model defined above is shown in Figure 10.

# Test the implied conditional independencies of the model structure against the 
# correlation matrix.

localTests( model, sample.cov=corr, sample.nobs=nrow( data ) )

# The output of localTests is shown in Figure 11. When testing using the sample 
# covariance matrix, localTests returns an estimate, the p-value of the test, 
# and the confidence interval for the estimate. An estimate of around 0 with a 
# p-value higher than 0.05 would mean that the data do not provide evidence 
# against the implied conditional independence being tested. Note that there is 
# a strong negative relationship between being female and being married, 
# indicating a major bias in the collection of this dataset.

## Optional: Test only some of the implied conditional independencies.

localTests( x=model, sample.cov=corr,
            sample.nobs=nrow( data ),
            max.conditioning.variables=2 )

# In the case that we only want to test some of the implied conditional 
# independencies of the model, the tests argument of localTests allows users to 
# specify a subset of the independencies to test. In the example above, only 
# independence conditions with less than 3 conditional variables are tested.

# Optional: Plot the test results.

plotLocalTestResults( localTests( model,
                                  sample.cov=corr, sample.nobs=nrow( data ) ) )

# Similar to the previous protocols, all the test results can be plotted to 
# inspect the results visually. An example output is shown in Figure 12.