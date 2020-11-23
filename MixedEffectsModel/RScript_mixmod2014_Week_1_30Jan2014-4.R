# example script mixed-models course; week 1
# Bernd Figner, Jan 30, 2014


# first you need to install the needed packages; some of them are 'development' versions not yet available on the default repository, thus for some packages, slightly different than the usual commands to install them have to be used

# I recommend to install them in the following order

# 1. package Matrix (lme4 needs this for its computation; when you load lme4, it will automatically load the package 'Matrix;' thus, once 'Matrix' is installed, you can basically forget about it)
install.packages('Matrix', repos='http://cran.us.r-project.org')

# check which version was installed with the following command
packageVersion('Matrix') # it should be 1.1.1.1 or higher 


# 2. package pbkrtest (this will be used later in the course, as it is one way to get p values for predictors)
install.packages('pbkrtest', repos='http://cran.us.r-project.org')

# check the version
packageVersion('pbkrtest') # it should be 0.3.8 or higher 



# 3. package lme4; this is our main mixed-models package
install.packages("lme4", repos = c("http://lme4.r-forge.r-project.org/repos", getOption("repos")))

# check the version
packageVersion('lme4') # it should be 1.1.2 or higher 


library(lme4)

fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(fm1)




?lme4

# after installation


library(lme4)