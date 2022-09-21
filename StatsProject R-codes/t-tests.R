#-------------------------------------------------------------------------------
original_data_file  <- "D:/StatsProject R-codes/Data/AdjustedOriginal.csv"
collected_data_file <- "D:/StatsProject R-codes/Data/AdjustedCollected.csv"

#folder where the result tables are saved
original_data_folder  <- "D:/StatsProject R-codes/Data Analysis/Original/"
collected_data_folder <- "D:/StatsProject R-codes/Data Analysis/Collected/"
#-------------------------------------------------------------------------------


library(car)       #For levene's test
library(reshape2)  #For melt(), converts lists into data frames 

#-------------------------------------------------------------------------------
# shortened round function to make code easier to read
# all values are rounded down to make the results easier to read

rn <- function(X, n = 2){ round(X, digits = n) }

#-------------------------------------------------------------------------------
# This function performs levene's test on the inputted columns and
# returns a true value if returned p-value is >= 0.05 (i.e we assume different variances)
# This was made into a separate function as the function leveneTest only accepts data frames

leveneT <- function(Xi, Yi){
  
  Z = list(x = Xi, y = Yi)
  tempdata <- melt(Z)
  
  f <- leveneTest(value~as.factor(L1), tempdata)
  
  vareql <- (f$`Pr(>F)`>=0.05)[1]
  vareql
}

#-------------------------------------------------------------------------------
#This function performs t-tests on the supplied lists, along with the result of the levene test
#  (that is var.equal), and the alpha. This function returns a named list which contains
#  the t-statistic, df, p-value, and the confidence interval

Ttest <- function(x, y, var.equal, alpha = 0.05){
  
  nx <- length(x) ;ny <- length(y)
  mx <- mean(x)   ;my <- mean(y)
  vx <- var(x)    ;vy <- var(y)
  
  #if the variance is found to be equal, standard t-test is performed. Else
  #  Welch's t-test is performed
  #This if-block calculates df and standard error acc. to the t-test being performed
  if(var.equal) {                                
    df <- nx+ny-2
    v <- ((nx-1)*vx + (ny-1)*vy)/df
    stderr <- sqrt(v*(1/nx+1/ny))
  } else {
    stderr <- sqrt((vx/nx) + (vy/ny))
    df <- stderr^4/((vx/nx)^2/(nx-1) + (vy/ny)^2/(ny-1))
  }
  
  #t-statistic
  tstat <- (mx - my)/stderr
  
  #p-value
  pval <- 2 * pt(-abs(tstat), df)
  
  #calculation for the confidence interval
  cint <- qt(1 - alpha/2, df)
  cint <- (tstat + c(-cint, cint))*stderr
  
  #creating and returning the named list
  #the confidence interval part is to return a string in the form [a; b], i.e as an interval
  rval <- list(statistic = rn(tstat), parameter = rn(df), p.value = rn(pval,4),
               conf.int = paste("[",rn(cint[1]), "; ", rn(cint[2]),"]"))
  rval
  
} 
##-------------------------------------------------------------------------------

################################################################################
#How the code is going to work : 
#  First two groups are made which we want to compare, such as Men vs Women
#    X and Y store the indices of these groups
#
#  Then makecsv() is called, passing through it is X and Y, along with the names
#    of the groups
#
#  Inside makecsv(), makecols() is called for each construct of trust, making a 
#    column for each of the trust constructs and then making a data frame out of
#    those columns. The rows of the data frame are name according to what data
#    was calculated in makecols(), and the data frame is stored as a csv in the
#    target folder, with the name of the groups.
#
#  Inside makecols(), the arguments taken are indices of the groups and the trust
#    construct which is going to be compared. Xi and Yi are assigned the values 
#    of those trust constructs fot the indices in X and Y.
#    levene's test of varience is conducted and on that result the appropriate 
#    t-test is conducted.
#    then this function makes and returns a column which contains, in order: 
#    mean of Xi, mean of Yi, sd of Xi, sd of Yi, t-statistic and df (as t(df) = t*),
#    p-value on comparison, 95% C.I. (as [a; b]), and lengths of Xi and Yi (as a_b)
################################################################################


###The code from here on is mostly for aesthetic purposes (of output)


##-------------------------------------------------------------------------------
makecols <- function(X, Y, trustConstruct){
  Xi = na.omit(trustConstruct[X])  #All the values for this trustConstruct by group X
  Yi = na.omit(trustConstruct[Y])  # and Y, with the N/A values removed
  
  varequal <- leveneT(Xi, Yi)
  
  #conducting t.test assuming variance is equal iff levene's test returns True
  temp <- Ttest(Xi, Yi, var.equal = varequal)
  
  #making a list of the results
  tempcol <- c(rn(mean(Xi)),        
               rn(mean(Yi)), 
               rn(sd(Xi)), 
               rn(sd(Yi)),
               paste("t(",temp$parameter,") = ",     #entry of the form
                     temp$statistic, sep=""),        #   t(*df*) = *t-statistic*
               temp$p.value,                         #p-value
               temp$conf.int,                        
               paste(length(Xi),length(Yi), sep = "_"))     #Lengths of Xi and Yi
  
  tempcol    #returns the tempcol
}

##-------------------------------------------------------------------------------
makecsv <- function(X,Y,n1="g1",n2="g2")  #takes in the indices of the groups and names of the groups
{
  ddtdata <- data.frame(Benevolence    = makecols(X, Y, benevolence),
                        Integrity      = makecols(X, Y, integrity),
                        Competence     = makecols(X, Y, competence),
                        Identification = makecols(X, Y, identification),
                        Concern        = makecols(X, Y, concern))  #dataframe of data calculated
  
  rows <-  c(paste(n1, "mean",sep = " "), 
             paste(n2, "mean",sep = " "),
             paste(n1, "sd"  ,sep = " "), 
             paste(n2, "sd"  ,sep = " "), 
             "t-statistic", 
             "p-value", 
             "95% C.I.",
             "n")                           #gives the rows appropriate names according to the explanation
  rownames(ddtdata) <- rows
  
  dest <- paste(target_folder,n1,"_vs_",n2,"w.csv",sep="")  #Location and name of the file
  write.csv(ddtdata, file = dest, quote = F)               #saving the file as csv
}


################################################################################
#first the data from the original data is analyzed
#target folder is the folder where the csv files are stored
#ddt is the data with the mean values of the trust constructs subtracted from 6
#(this has been done as in the original data each construct has 5 columns
# and lower values show higher trust, so subtracting the mean from 6 shows
# higher value is higher trust)
################################################################################

target_folder <- original_data_folder

ddt = read.csv(original_data_file)

attach(ddt)

#male vs female, gender = 1 is male and = 2 is female
X = which(Gender == 1)
Y = which(Gender == 2)
makecsv(X, Y, "Male", "Female")

#By_age, age>1 is more than or equal to 21, =1 is equal to or less than 20
X = which(Age > 1)
Y = which(Age == 1)
makecsv(X, Y, "Age_more_or_equal_to_21", "Age_less_or_equal_to_20")

#By_Usage, time > 4 is using once or less times a day, < 5 is few times or more
X = which(Time < 5)
Y = which(Time > 4)
if(length(X) > 19 & length(Y) > 19)
makecsv(X, Y, "Using_few_times_a_day", "Once_or_less")

#using a particular social media website
#The columns 5:12 in this csv show which sites the respondents use, 1 means use, 0 means not
for(i in 5:12){
  X = which(ddt[,i] == 1)
  Y = which(ddt[,i] == 0)
  makecsv(X, Y, paste(names(ddt)[i], "user", sep="_"),    #Here the names of columns are
          paste("non-",names(ddt)[i], "_user", sep=""))   #used as the name of groups
}

#what data is shared
# columns 15:21 in this csv show which data the respondent has shared on their social media
for(i in 15:21){
  X = which(ddt[,i] == 1)
  Y = which(ddt[,i] == 0)
  makecsv(X, Y, paste(names(ddt)[i], "shared", sep=" "), 
          paste(names(ddt)[i], "not_shared", sep="_"))
}

#By amount of data shared
# this column is sum of columns 15:21. So it gives a metric of how much data the respondent 
#  has shared on their social media
X = which(Shared_data<4)
Y = which(Shared_data>3)
makecsv(X, Y, "Less_shared", "More_shared")


detach(ddt)

################################################################################
#next the data we collected is analyzed
#everything done here is same as above
################################################################################

target_folder <- collected_data_folder

ddt = read.csv(collected_data_file)
attach(ddt)

#female vs male
X = which(Gender == 1)
Y = which(Gender == 2)
makecsv(X, Y, "Male", "Female")

#age
X = which(Age > 19)
Y = which(Age < 20)
makecsv(X, Y, "Age_more_or_equal_to_20", "Age_less_or_equal_to_19")

#Usage
X = which(Time < 5)
Y = which(Time > 4)
if(length(X) > 19 & length(Y) > 19)
  makecsv(X, Y, "Using_few_times_a_day", "Once_or_less")

#using a particular social media website
# columns 8:16 in this csv show which data the respondent has shared on their social media
for(i in 8:16){
  X = which(ddt[,i] == 1)
  Y = which(ddt[,i] == 0)
  makecsv(X, Y, paste(names(ddt)[i], "user", sep="_"), 
          paste("non-",names(ddt)[i], "_user", sep=""))
}

#what data is shared
# columns 20:26 in this csv show which sites the respondents use
for(i in 20:26){
  X = which(ddt[,i] == 1)
  Y = which(ddt[,i] == 0)
  makecsv(X, Y, paste(names(ddt)[i], "shared", sep=" "), 
          paste(names(ddt)[i], "not_shared", sep="_"))
}

#By amount of data shared
X = which(Shared_data<4)
Y = which(Shared_data>3)
makecsv(X, Y, "Less_shared", "More_shared")

detach(ddt)

################################################################################
#Finally, comparing our data to theirs
#we basically combine those two to form a data frame and continue as above
################################################################################

ddt1 = read.csv(original_data_file)
ddt2 = read.csv(collected_data_file)

index<- append(rep(0,dim(ddt1)[1]), rep(1, dim(ddt2)[1]))

for (i in c("benevolence","integrity","competence","identification","concern"))
  {
  assign(i, append(ddt1[,i], ddt2[,i]))
  }

ddt = data.frame(index, benevolence, integrity, competence, identification, concern)
attach(ddt)

target_folder <- original_data_folder

X <- which(index == 0)
Y <- which(index == 1)
makecsv(X, Y, "..Original", "Collected")

detach(ddt)
