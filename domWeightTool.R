###########################################################################
#### Modification, adaptation, or use of any portion of this code for  ####
#### any purpose must include a reference to the full manuscript       ####
#### citation provided below. This code is provided 'as is.' The       ####
#### authors are under no obligation to provide assistance or          ####
#### instruction pertaining to the use or operation of this code.      ####
####                                                                   ####
#### To cite this model use:                                           ####
#### citation redacted for blind review                                ####
###########################################################################

DW.accuracy <- function(userDat,n.obs,iv.relia,dv.relia,iv.names,whichCor,spurIV,epsilon,n.sims) {

### Loads necessary libraries ###

library(yhat) # library for calculating relative weight indices
library(boot) # library for bootstrapping function
library(MASS) # library for mvrnorm function for generating multivariate normal data

### Setting default values for the simulation ###

if(class(userDat) == 'data.frame') {if(missing(n.obs)) {n.obs <- NA}}
if(missing(iv.relia)) {iv.relia <- rep(1,(ncol(corMatrix)-1))} # sets the default for predictor reliability to 1
if(missing(dv.relia)) {dv.relia <- 1} # sets the default for DV reliability to 1
if(missing(iv.names)) {iv.names <- paste("X",1:length(iv.relia),sep="")} # sets the default for predictor names to X#
if(missing(whichCor)) {whichCor <- 0} # sets the default matrix to uncorrected
if(missing(spurIV)) {spurIV <- T} # sets the default for adding a spurious predictor to False
if(missing(epsilon)) {epsilon <- F} # sets the default to reporting general dominance weights. If true will report relative importance weights (epsilon) instead
if(missing(n.sims)) {n.sims <- 100} # sets default for number of simulation runs to 1000

### initializing function to conduct relative importance analysis ###

reg.results <- function(regdata) {
  lm.out <- lm(DV ~ ., data=regdata)
  regrOut <- calc.yhat(lm.out,prec=8)
  if(epsilon == TRUE) {weight <- "RLW"} else {weight <- "GenDom"}
  if (ncol(regdata) > 3) {
  boot.out <- boot(regdata,boot.yhat,100,lmOut=lm.out,regrout0=regrOut)
  results <- booteval.yhat(regrOut, boot.out, bty = "norm",  level = .95, prec=8)
  return(list(regrOut$PredictorMetrics[,weight],regrOut$OrderedPredictorMetrics[,weight],data.frame(results$combCIpmDiff[,weight],stringsAsFactors=FALSE)))
  } else {return(list(regrOut$PredictorMetrics[,weight],regrOut$OrderedPredictorMetrics[,weight]))}
}

if(class(userDat) != 'data.frame') {
	### Making corMatrix a matrix object ###

	corMatrix <- matrix(as.vector(t(userDat)), nrow = sqrt(length(userDat)),byrow = T)

	### Make fully corrected matrix and operational validity matrix ###

	matrixCor  <- matrixUnc <- matrixOper <- corMatrix

	# fully corrected matrix
	matrixCor[1:(nrow(corMatrix)-1),1:(nrow(corMatrix)-1)]  <- corMatrix[1:(nrow(corMatrix)-1),1:(nrow(corMatrix)-1)] / sqrt(matrix(apply(expand.grid(iv.relia,iv.relia),1,prod),nrow=(nrow(matrixCor)-1),byrow=T))
	matrixCor[nrow(corMatrix),1:(nrow(corMatrix)-1)] <- matrixCor[1:(nrow(corMatrix)-1),nrow(corMatrix)] <- corMatrix[nrow(corMatrix),1:(nrow(corMatrix)-1)] / sqrt(iv.relia*dv.relia)
	diag(matrixCor) <- 1

	# Operational validity - only correct for unreliability in DV
	matrixOper[nrow(corMatrix),1:(nrow(corMatrix)-1)] <- matrixOper[1:(nrow(corMatrix)-1),nrow(corMatrix)] <- (corMatrix[nrow(corMatrix),1:(nrow(corMatrix)-1)] / sqrt(rep(1,(ncol(corMatrix)-1))*dv.relia))

	### Pick the correlation matrix that was chosen above ###

	matrixR <- (whichCor==2)*matrixOper + (whichCor==1)*matrixCor + (whichCor==0)*matrixUnc
	rownames(matrixR) <- colnames(matrixR) <- varnames <- c(iv.names,"DV")

	### Add random predictor to correlation matrix (if selected) ###

	if (spurIV == T) {
		matrixR <- cbind(rep(0,nrow(matrixR)),matrixR)
		matrixR <- rbind(c(1,rep(0,(ncol(matrixR)-1))),matrixR)
		rownames(matrixR)[1] <-c("spurr")
		colnames(matrixR)[1] <-c("spurr") 
	}

	### Actual simulation to compute relative importance weights for each sample along with differences ### 

	out <- t(sapply(1:n.sims, function(x) {  data <- mvrnorm(n.obs,rep(0,ncol(matrixR)),matrixR)
                                       data <- as.data.frame(data)
                                       reg.results(data)}))
} else {
	names(userDat) <- varnames <- c(iv.names,"DV")
	if (spurIV == T) {
		userDat <- cbind(rnorm(nrow(userDat),0,1),userDat)
		names(userDat)[1] <- c("spurr")
	}
	out <- t(sapply(1:n.sims, function(x) {  
		dataBS <- userDat[sample(nrow(userDat),nrow(userDat),replace=T),]
		corMatrix <- cor(dataBS)
		matrixCor  <- matrixUnc <- matrixOper <- corMatrix

		if(whichCor > 0) {
			# fully corrected matrix
			if(spurIV == T) {iv.relia_t <- c(1,iv.relia)} else {iv.relia_t <- iv.relia}	
			matrixCor[1:(nrow(corMatrix)-1),1:(nrow(corMatrix)-1)]  <- corMatrix[1:(nrow(corMatrix)-1),1:(nrow(corMatrix)-1)] / sqrt(matrix(apply(expand.grid(iv.relia_t,iv.relia_t),1,prod),nrow=(nrow(corMatrix)-1),byrow=T))
			matrixCor[nrow(corMatrix),1:(nrow(corMatrix)-1)] <- matrixCor[1:(nrow(corMatrix)-1),nrow(corMatrix)] <- corMatrix[nrow(corMatrix),1:(nrow(corMatrix)-1)] / sqrt(iv.relia_t*dv.relia)
			diag(matrixCor) <- 1

			# Operational validity - only correct for unreliability in DV
			matrixOper[nrow(corMatrix),1:(nrow(corMatrix)-1)] <- matrixOper[1:(nrow(corMatrix)-1),nrow(corMatrix)] <- (corMatrix[nrow(corMatrix),1:(nrow(corMatrix)-1)] / sqrt(rep(1,(ncol(corMatrix)-1))*dv.relia))
		}
		### Pick the correlation matrix that was chosen above ###

		matrixR <- (whichCor==2)*matrixOper + (whichCor==1)*matrixCor + (whichCor==0)*matrixUnc		
		data <- mvrnorm(nrow(userDat),rep(0,ncol(matrixR)),matrixR,empirical=T)
		data <- as.data.frame(data)	
		reg.results(data)}))
}

### Format simulation output ###

outWeights <- data.frame(matrix(unlist(out[,1]), nrow=n.sims, byrow=T),stringsAsFactors=FALSE)
outRanks <- data.frame(matrix(unlist(out[,2]), nrow=n.sims, byrow=T),stringsAsFactors=FALSE)
# label the columns, but the last column will be R2
if(spurIV == T) {
	colnames(outWeights) <- c("spurr",varnames[1:(length(varnames)-1)],"R2")
	colnames(outRanks) <- c("spurr",varnames[1:(length(varnames)-1)])
} else {
	colnames(outWeights) <- c(varnames[1:(length(varnames)-1)],"R2")
	colnames(outRanks) <- varnames[1:(length(varnames)-1)]
}

### calculate mean and sd of weights ###

meanWeight <- apply(outWeights,2,mean)
sdWeight <- apply(outWeights,2,sd)

### calculate mean and sd of ranks ###

meanRank <- apply(outRanks,2,mean)
sdRank <- apply(outRanks,2,sd)

### calculate confidence intervals ###

if(whichCor == 0) {
weight95 <- apply(outWeights,2,function(x){quantile(x, c(.025,.975))})
rank95 <- apply(outRanks,2,function(x){quantile(x, c(.025,.975))})
} else { weight95 <- rank95 <- c(NA,NA)}

### Determine whether weights are significantly different from one another ###
if(ncol(outWeights) > 3) { # only runs if greater than 2 predictors (including spurious)

sigDiffs <- t(sapply(1:n.sims, function(x) {
	temp <- data.frame(out[,3][[x]],stringsAsFactors = F)
	temp <- as.character(temp[,1])
	temp1 <- rep(0,length(temp))
	temp1[grep("*",temp,fixed = T)] <- 1
	return(temp1)
}))
colnames(sigDiffs) <- rownames(out[,3][[1]])
meanSigDiffs <- apply(sigDiffs,2,mean)

### Determine if weights are significantly different from zero (if included) ###

if(spurIV == T) {
diffZero <- meanSigDiffs[grep("spurr",names(meanSigDiffs),fixed=T)] # predictors diff from zero
names(diffZero) <- iv.names
diffPredictors <- meanSigDiffs[-grep("spurr",names(meanSigDiffs),fixed=T)]
} else {diffPredictors <- meanSigDiffs}

} # ends if statement regarding number of predictors
### Compiles and names output ###

weightOut <- data.frame(meanWeight,sdWeight,t(weight95))
colnames(weightOut) <- c("mean","sd","lower","upper") 
rankOut <- data.frame(meanRank,sdRank,t(rank95))
colnames(rankOut) <- c("mean","sd","lower","upper") 

### Returns up to six objects - avg.weights, avg.ranks, raw.weights, raw.ranks, diff.zero, and sig.diffs ###

if(ncol(outWeights) > 3) {
  if(spurIV == T) {  
	finalOut <- list(weightOut,rankOut,outWeights,outRanks,diffPredictors,diffZero)
	names(finalOut) <- c("avg.weights","avg.ranks","raw.weights","raw.ranks","sig.diffs","diff.zero")
  } else {
	finalOut <- list(weightOut,rankOut,outWeights,outRanks,diffPredictors)
	names(finalOut) <- c("avg.weights","avg.ranks","raw.weights","raw.ranks","sig.diffs")
  }
} else {
  finalOut <- list(weightOut,rankOut,outWeights,outRanks)
  names(finalOut) <- c("avg.weights","avg.ranks","raw.weights","raw.ranks")
}

return(finalOut)
} # end simulation function