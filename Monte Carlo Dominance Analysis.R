### ---
### Load Dominance Analysis Package for regular DA
### and package to run reliability analyses
### ---

library(dominanceanalysis)
library(psych)

### ---
### Load domWeightTool function in other tab BEFORE PROCEEDING
### ---

### ---
### Import Data
### ---

NCO_Promotion_Study <- read.csv("NCO_Promotion_Study_CLEAN VERSION_09FEB16.csv")

### ---
### Rename variables
### ---

### ---
### Rename competency variables (predictors)
### ---
posenviron <- NCO_Promotion_Study$CREATEAPOSITIVEENVIRONMENT
getresults <- NCO_Promotion_Study$GETRESULTS
preplead <- NCO_Promotion_Study$PREPARESELFTOLEAD
devlead <- NCO_Promotion_Study$DEVELOPLEADERS
leadbyexp <- NCO_Promotion_Study$LEADBYEXAMPLE
comms <- NCO_Promotion_Study$COMMUNICATE

### ---
### Adaptive Performance (outcome)
### ---

APscore <- NCO_Promotion_Study$APSCORE

### --- 
### Run regular Dominance Analysis to compare results
### to more advanced tool
### ---

library(dominanceanalysis)
lm.competency<-lm(NCO_Promotion_Study$APSCORE~.,competency)
da.competency<-dominanceAnalysis(lm.competency)
View(boot$boot)

print(da.competency)
summary(da.competency)
View(da.competency)

bda.competency=bootDominanceAnalysis(lm.competency, R=100)
summary(bda.competency)

### ---
### Run reliability tests for use in Monte Carlo dominance analysis
### ---

reliatest <- NCO_Promotion_Study[,69:74]

alpha(reliatest, keys=NULL,cumulative=FALSE, title="reliability", max=10,na.rm = TRUE,
      check.keys=FALSE,n.iter=1,delete=TRUE,use="pairwise",warnings=TRUE,n.obs=NULL)

### ---
### Run Monte Carlo Dominance Analysis
### accounting for measurement error (alpha level)
### ---
### Result of analysis: Most dominant predictor
### in predicting Adaptive Performance in military personnel
### ---

df1 <- data.frame(posenviron,
                       getresults,
                       preplead,
                       devlead,
                       leadbyexp,
                       comms,
                       APscore)

### ---
### Impute missing values with the mean values
### ---

df2 <- data.frame(
  sapply(
    df1,
    function(x) ifelse(is.na(x),
                       mean(x, na.rm = TRUE),
                       x)))

### ---
### Run analysis using Monte Carlo Dominance Analysis tool
### Note: it takes some time to run!
### ---

test <- DW.accuracy(df2, 
              iv.relia = c(.81,.81,.81,.81,.81,.81),
              dv.relia = .77, 
              iv.names = c("posenviron",
                           "getresults",
                           "preplead",
                           "devlead",
                           "leadbyexp",
                           "comms"),
              whichCor = 0, 
              spurIV = T,
              epsilon = F,
              n.sims = 100)

### --- 
### Print output and view weights
### ---

print(test)

View(test$avg.weights)


