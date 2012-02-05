###############################################################################
## Tutorial Practice.R
##
## TODO: Practice from R Tutorial Session (1/26/12)
## 
## Author: Haaland
###############################################################################


#library(psych) loads the psych package - this should be at the start
##of each file to load all the commands in

#ex. conc <- read.csv("your data set.csv") : this inserts your data
##into a named object (ex. "conc")
###OR since the driver file already establishes the data in prefile 
###(or postfile), you can say this instead of the actual file

#attach(data object) : you need to attach your data to the workspace
##before you can start working with it

#The above is coded in the driver.R file (which then runs the two
##read data files

#search() : this tells you what's in your workspace (shows you what
##packages you have - as long as they've been loaded in the script)

#if you download a package, install it by typing 
##install.packages("packagename") into the command window

#each function has default settings. if you don't specify any, they
##will just go to default. To find out the defaults for a function,
##type in " " - this will show you all the possible specifications.
##For example, if the first row of your data set is headers, you need
##to change the header = F default to header = T.

#If there are missing values in your data set, you input NA (case 
##sensitive)

#names(object) gives you variable names
#describe(object) gives you statistics

#to check a correlation among measures, you can create a new dataframe 
##that picks out the varialbes you want to look at.
###conc1cor[this just names your new frame] = data.frame(var1, var2,...)
###cor(conc1cor, use = "complete")
#I think I might need to do this to get an input for Factor Analysis

#the summary command gives you a nice output of the results (to clean
##up ugly scripts)
##need to save the results of the function into an object and then do
##summary on that
###aov.x1=aov(sac~sex)
###summary (aov.x1)

#in R, regarding T-tests of ANOVAs, the default assumption is that 
##homogeneity of variance is not true. It forces you to test the
##assumption first, or just lie in your code, ex:
###t.test(sac~sex, var.equal = T)

#lm is the function for running a regression ("linear model")
##model1.z = lm(scale(scatii) ~ scale(gad7) + scale (phq9))
##summary(model1.z)
### the ~ means "as a function of"
###doing scale(x) makes it possible to compare the magnitude of many
###different variables