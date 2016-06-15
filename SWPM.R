# SWPM.R
# Version 1
# Written by George McWilliams
# Version started on 26th of Nov, 2014

# Functions to value libor-based derivatives

# Changes from last version:
# - 

# To do:
# - Treasury curve discount factors?
# - Functions to calculate precise cash flow timings for swaps


# source('//NJFILE02//GFIQSHR//QUANT_RESEARCH//R_FUNCTIONS//SWPM.R')


source('//NJFILE02//GFIQSHR//QUANT_RESEARCH//R_FUNCTIONS//Utilities.R')

# -------------------------------------------------------------------------------------
# Functions

# To do:
# - 


# ------------------------------------------------------------------------------------------------------------------------
# Discount factors
getDiscountFactors = function(effdate) {
# A function to get (libor) discount factors from Bloomberg
# using cash and swap rates

# To compare results, look at ICVS (23 for US libor) function

# Notes:
# - Currently only uses discount rates and swap rates. Need to include ED futures!

# effdate = as.Date('2015/09/08')

	library(RBloomberg)
	cal1 = Calendar(holidays = as.Date(holidayNYSE(year(effdate):(year(effdate) + 50))))

	overnight		= 'US00O/N'
	week1			= 'US0001W'
	monthVec		= c(1,3,6,12)
	swapMat			= c(2:12,15,20,25,30)
	tickersCash		= paste(c(overnight, week1, paste0('US00', formatC(monthVec, flag=0, width=2), 'M')), 'Index')
	tickersSwaps	= paste(paste0('USSW', swapMat), ' Curncy')
    conn = blpConnect()
        mktCash			= bdh(conn, tickersCash, 'PX_LAST', as.character(effdate, format='%Y%m%d'), as.character(effdate, format='%Y%m%d'))
        mktSwap			= bdh(conn, tickersSwaps, 'PX_LAST', as.character(effdate, format='%Y%m%d'), as.character(effdate, format='%Y%m%d'))
		# mktSpotDt		= bdp(conn, tickersCash[1], 'TRADING_DT_REALTIME')
		mktSwap_days	= bdp(conn, tickersSwaps, 'DAYS_TO_MTY')
    blpDisconnect(conn)
	
	# spotDate = as.Date(mktSpotDt$TRADING_DT_REALTIME)
	spotDate = adjust.next(adjust.next(effdate + 1, cal1) + 1, cal1)	# Spot date is T+2
	spotDate_chron = chron(as.character(spotDate, format='%Y%m%d'), format='ymd')
	
	mktCash$SpotDate	= spotDate
	mktCash$SpotDate[1] = effdate
	mktCash$MatDate		= mktCash$SpotDate + 1		# Initialise O/N rate
	mktCash$MatDate[2]	= mktCash$SpotDate[2] + 7	# Initialise 1W rate
	
	timeToSpot = yearFrac(effdate, spotDate, dayCountBasis=2)
	
	# Calculate months
	for(ii in 1:length(monthVec)) {
		mktCash$MatDate[ii + 2] = seq(spotDate, len=2, by = paste(monthVec[ii], 'months'))[2]
	}
	# # Note: have to use seq.chron here since it treats near-end-of-month dates consistnetly with BBG
	# # e.g. seq(chron("01/31/04"), length = 4, by = "month") c.f. seq(as.Date("2004-01-31"), length = 4, by = "month")
	# mktCash$MatDate[2+(1:length(monthVec))] = as.Date(seq(spotDate_chron, len=max(monthVec)+1, by = 'months'))[monthVec+1]
	
	# Correct for holidays
	# for(ii in 1:nrow(mktCash)) {
		# while(! is.bizday(mktCash$MatDate[ii], cal1)) {
			# mktCash$MatDate[ii] = adjust.next(mktCash$MatDate[ii], cal1)
		# }
	# }
	mktCash$MatDate		= adjust.next(mktCash$MatDate, cal1)
	mktCash$TimeToMat	= yearFrac(mktCash$SpotDate, mktCash$MatDate, dayCountBasis=2)	# ACT/360
	# mktCash$TimeToMat	= c(12/365.25, 12/52, monthVec)/12			# This is inaccurate! Need exact daycount here (ACT/360)
	
	mktCash$MktRate		= mktCash$PX_LAST/100								# Annualised MktRate rate (unitless)
	mktCash$DF			= 1/(1 + mktCash$MktRate*mktCash$TimeToMat)	# Note: no 'compounding' here! Simple 1 period (annualised) rate
	
	mktCash$IFR			= NA
	mktCash$IFR[1]		= -log(mktCash$DF[1])/mktCash$TimeToMat[1]
	
	for(ii in 2:nrow(mktCash)) {
		mktCash$IFR[ii] = -mktCash$IFR[ii-1] - 2*log(mktCash$DF[ii]/mktCash$DF[ii-1]) / (mktCash$TimeToMat[ii] - mktCash$TimeToMat[ii-1])
	}
	
	# ED Futures
	mktEd				= GetEDStrip(effdate, nYears = 3)
	mktEd$ticker		= row.names(mktEd)
	mktEd$SpotDate		= effdate
	mktEd$TimeToStart	= yearFrac(effdate, mktEd$StartDate, dayCountBasis=2)	# ACT/360 - Note: Settle date for futures is today (effdate)! Not T+2 (spotDate)
	mktEd$TimeToMat		= yearFrac(effdate, mktEd$MatDate, dayCountBasis=2)
	
	mktEd$DF	= 1
	mktEd$IFR	= 0
	
	mktEd$DF[1] = integrateIFR(0, mktEd$TimeToStart[1], mktCash) * (mktEd$MktRate[1]/2 + 1)^(-2*(mktEd$TimeToMat[1] - mktEd$TimeToStart[1]))
	mktEd$IFR[1] = -mktCash$IFR[4] - 2*log(mktEd$DF[1]/mktCash$DF[4]) / (mktEd$TimeToMat[1] - mktCash$TimeToMat[4])	# Piecewise linear IFR
	# mktEd$IFR[1] = - log(mktEd$DF[1]/mktCash$DF[4]) / (mktEd$TimeToMat[1] - mktCash$TimeToMat[4])	# Piecewise const IFR
	
	for(i in 2:nrow(mktEd)) {
		mktEd$DF[i] = integrateIFR(0, mktEd$TimeToStart[i], mktEd) * (mktEd$MktRate[i]/2 + 1)^(-2*(mktEd$TimeToMat[i] - mktEd$TimeToStart[i]))
		mktEd$IFR[i] = -mktEd$IFR[i-1] - 2*log(mktEd$DF[i]/mktEd$DF[i-1]) / (mktEd$TimeToMat[i] - mktEd$TimeToMat[i-1])	# Piecewise linear IFR
		# mktEd$IFR[i] = - log(mktEd$DF[i]/mktEd$DF[i-1]) / (mktEd$TimeToMat[i] - mktEd$TimeToMat[i-1])	# Piecewise const IFR
	}
	
	# Swaps
	mktSwap$DaysToMat	= mktSwap_days$DAYS_TO_MTY
	mktSwap$SpotDate	= spotDate
	mktSwap$MatDate		= spotDate
	for(ii in 1:nrow(mktSwap)) {
		mktSwap$MatDate[ii] = seq(spotDate, len=2, by=paste(mktSwap$DaysToMat[ii], 'days'))[2]
	}
	mktSwap$MatDate		= adjust.next(mktSwap$MatDate, cal1)
	mktSwap$TimeToMat	= yearFrac(spotDate, mktSwap$MatDate, dayCountBasis=0)	# 30/360
	
	mktSwap$MktRate	= mktSwap$PX_LAST/100
	mktSwap$DF		= NA
	mktSwap$IFR		= NA
	
	cashFlowFloat = data.frame(Dates = adjust.next(seq(spotDate, max(mktSwap$MatDate), by='3 months'), cal1))
	
	cashFlowFixed			= data.frame(Dates = adjust.next(seq(spotDate, max(mktSwap$MatDate), by='6 months'), cal1))
	nFix					= nrow(cashFlowFixed)
	cashFlowFixed$TimeToMat = yearFrac(spotDate, cashFlowFixed$Dates, dayCountBasis=0)	# 30/360
	cashFlowFixed$YearFrac	= cashFlowFixed$TimeToMat - c(0,cashFlowFixed$TimeToMat[1:(nFix-1)])
	cashFlowFixed			= cashFlowFixed[2:nrow(cashFlowFixed),]
	
	# Iteratively calculate IFR for swaps
	colAll = intersect(names(mktCash), intersect(names(mktEd), names(mktSwap)))
	# mktAll = rbind(mktCash[colAll], mktSwap[colAll])	# Cash and swaps only
	# mktAll = rbind(mktCash[1:4,colAll], mktEd[colAll], mktSwap[colAll])	# Include EDF - Cash only up to 3M
	mktAll = rbind(mktCash[1:4,colAll], mktEd[1:(6),colAll], mktSwap[1:nrow(mktSwap), colAll])	# Include EDF - Cash only up to 3M - Swaps from 3yr and out
	
	# nCash = nrow(mktCash)
	nCash = which(mktAll$ticker == mktSwap$ticker[1]) - 1	# TEMP!!! [2]
	for(ii in 1:nrow(mktSwap)) {
		iiLoop = ii + nCash
		
		cashFlowLoop = subset(cashFlowFixed, Dates < mktAll$MatDate[iiLoop])
		cashFlowLoop$DF = NA
		for(jj in 1:nrow(cashFlowLoop)) {
			cashFlowLoop$DF[jj] = integrateIFR(0, cashFlowLoop$TimeToMat[jj], mktAll)
		}
		
		levelLoop			= sum(cashFlowLoop$YearFrac*cashFlowLoop$DF)
		# Note: 1st term discounts from spotDate to effdate
		mktAll$DF[iiLoop]	= (1 - mktAll$MktRate[iiLoop]*levelLoop) / 
								(1 + (mktAll$TimeToMat[iiLoop] - cashFlowLoop$TimeToMat[nrow(cashFlowLoop)])*mktAll$MktRate[iiLoop])
		
		
		# Update IFR
		mktAll$IFR[iiLoop] = -mktAll$IFR[iiLoop-1] - 2*log(mktAll$DF[iiLoop]/mktAll$DF[iiLoop-1]) / (mktAll$TimeToMat[iiLoop] - mktAll$TimeToMat[iiLoop-1])	# Piecewise linear IFR
		# mktAll$IFR[iiLoop] = - log(mktAll$DF[iiLoop]/mktAll$DF[iiLoop-1]) / (mktAll$TimeToMat[iiLoop] - mktAll$TimeToMat[iiLoop-1])	# Piecewise const IFR
	}
	
	mktAll$ZeroRate			= (mktAll$DF)^(-1/mktAll$TimeToMat) - 1		# Annually compounded ZCB rate
	mktAll$ZeroRate_Semi	= 2*((mktAll$DF)^(-1/(2*mktAll$TimeToMat)) - 1)		# Semi-annually compounded ZCB rate. For comparison with BBG (Swap ZCB rates are quoted as such)
	
	# Look at 3M fwd rates (tenor of 3M, variable maturity)
	# mktAll$fwd3m = (mktAll$DF[4]/mktAll$DF - 1)/(mktAll$TimeToMat - mktAll$TimeToMat[4])	# Simple
	mktAll$fwd3m = (mktAll$DF[4]/mktAll$DF)^(1/(mktAll$TimeToMat - mktAll$TimeToMat[4])) - 1	# Annualised
	
	# Maturity of 3M, variable tenor
	mktAll$DF_Fwd = 0
	for(ii in 1:nrow(mktAll)) {
		mktAll$DF_Fwd[ii] = integrateIFR(0, mktAll$TimeToMat[ii] + 0.25, mktAll)
	}
	mktAll$Fwd3m2 = ( mktAll$DF/mktAll$DF_Fwd)^(1/0.25) - 1	# Annualised
	
	# Need to compare fwd rates with BBG! Not sure of their formula...
	
	return(mktAll)
}

integrateIFR = function(tStart, tEnd, dfIFR) {
# integrateIFR
# A function to integrate istantaneous forward rates to get discount factors
# assuming a piecewise linear IFR curve


# To test...
# dfIFR = mktCash
# tStart = 0.001
# tEnd = 0.9

	dfIFR2 = dfIFR[c('TimeToMat','IFR')]
	if(dfIFR2$TimeToMat[1] > 0) {
		dfAdd				= dfIFR2[1,]
		dfAdd$TimeToMat[1]	= 0
		dfIFR2				= rbind(dfAdd, dfIFR2)
	} else {
		dfIFR2 = dfIFR
	}
	dfIFR2$YearFrac = c(0, diff(dfIFR2$TimeToMat))

	indStart	= which(tStart < dfIFR2$TimeToMat)[1]
	indEnd		= which(tEnd <= dfIFR2$TimeToMat)[1]

	if(is.na(indEnd)) {
		indEnd = nrow(dfIFR2) + 1
		
		dfAdd			= dfIFR2[indEnd-1,]
		dfAdd$TimeToMat = 100
		dfAdd$YearFrac	= 100 - dfIFR2$TimeToMat[indEnd-1]
		dfIFR2	= rbind(dfIFR2, dfAdd)
	}
	
	intSum = 0
	for(jj in indStart:indEnd) {
		t1 = dfIFR2$TimeToMat[jj-1]
		t2 = dfIFR2$TimeToMat[jj]
		# indLoop = c(jj-1, jj)
		indLoop = 1:jj
		timeDiff	= ifelse(jj==indEnd, tEnd, t2) - ifelse(jj==indStart, tStart, t1)
		# avgIFR = approxExtrap(dfIFR2$TimeToMat[indLoop], dfIFR2$IFR[indLoop], min(tEnd, t2), na.rm=T)$y	# Const. IFR
		avgIFR = 0.5*sum(approxExtrap(dfIFR2$TimeToMat[indLoop], dfIFR2$IFR[indLoop], c(max(tStart, t1), min(tEnd, t2)), na.rm=T)$y)	# Linear IFR
		
		intSum = intSum + avgIFR*timeDiff
	}

	return(exp(-intSum))
}

GetEDStrip = function(effdate, nYears = 2) {
# A function to return the ED futures strip

	effdate_qry = as.character(effdate, format='%Y%m%d')
	today_qry = as.character(Sys.Date(), format='%Y%m%d')

	tickerDf = data.frame(MonthNum = (nYears + 1)*(1:4),
							Ticker = c('H','M','U','Z'))
	currentMonth = month(effdate)
	nQtr			= 4*(nYears + 1)
	yearVec			= rep(year(effdate), nQtr) + floor((1:nQtr - 1)/4)
	yearVec1		= as.numeric(substr(yearVec, 4, 4))
	
	stripAll = paste0('ED', tickerDf$Ticker, yearVec1, ' Comdty')
	
	endDate = as.Date(timeLastDayInMonth(seq(effdate, length=2, by = paste(nYears, 'year'))[2]))
	
    conn = blpConnect()
        mktEdRate	= bdp(conn, stripAll, 'CONVEXITY_ADJUSTED_RATE')
        mktEdSt		= bdp(conn, stripAll, 'INT_RATE_FUT_START_DT')
        mktEdMat	= bdp(conn, stripAll, 'INT_RATE_FUT_END_DT')
		
        mktEdPrice_effdate	= bdh(conn, stripAll, 'PX_LAST', effdate_qry, effdate_qry)
        mktEdPrice_today	= bdh(conn, stripAll, 'PX_LAST', today_qry, today_qry)
    blpDisconnect(conn)
	
	mktEd				= cbind(mktEdSt, mktEdMat, mktEdRate, mktEdPrice_effdate$PX_LAST, mktEdPrice_today$PX_LAST)
	names(mktEd)		= c('StartDate','MatDate','MktRate_today','MktPrice_effdate','MktPrice_today')
	mktEd$StartDate		= as.Date(mktEd$StartDate)
	mktEd$MatDate		= as.Date(mktEd$MatDate)
	mktEd				= mktEd[order(mktEd$StartDate), ]
	mktEd2				= subset(mktEd, MatDate > effdate & MatDate < endDate)
	
	# Convert today's rates to effdate rates based on effdate price and convexity - approximation!
	mktEd2$ConvAdj			= mktEd2$MktRate_today - (100 - mktEd2$MktPrice_today)
	mktEd2$MktRate_effdate	= mktEd2$ConvAdj + (100 - mktEd2$MktPrice_effdate)
	
	mktEdOut = mktEd2[c('StartDate','MatDate','MktRate_today','ConvAdj')]
	names(mktEdOut) = gsub('_today', '', names(mktEdOut))
	
	mktEdOut$MktRate = mktEdOut$MktRate/100
	
	return(mktEdOut)
}



# ------------------------------------------------------------------------------------------------------------------------
# Trees
IRTree_OneFactor = function() {
# IRTree_OneFactor
# Generate trinomial tree for one-factor YC models of the form:
#	dx = theta(t) - (a.x)dt + (sig)dz

	# Sample YC data (c.f. Hull, White 1996)
	ycData = read.csv('//NJFILE02//GFIQSHR//QUANT_RESEARCH//R_FUNCTIONS//Research//ZC_19940708.csv')
	ycData$TimeToMat	= ycData$Days/365	# Assume actual/365
	ycData$MktRate		= ycData$Rate/100
	ycData$DF			= 1/(1 + ycData$MktRate*ycData$TimeToMat)
	
	# Parameters
	a		= 0.1
	sig		= 0.01
	nSteps	= 3		# Number of steps in the tree
	
	# ZCB option info
	expiry	= 3		# 3yr option expiry
	mat		= 9		# Bond matures in 9yr
	strike	= 63
	
	delT	= expiry/nSteps
	M		= exp(-a*delT) - 1						# Mean of x
	V		= (sig^2)*(1 - exp(-2*a*delT)) / (2*a)	# Variance of x
	delr	= sqrt(3*V)
	jMax	= ceiling(-0.184/M)	# Integer first node where non-standard branching occurs
	
	# Create transition prob table
	jTransMat	= transProbMat(M, jMax)	# Create transition matrix
	jAll		= -jMax:jMax
	
	# Create alpha table
	aTable				= data.frame(Index = 0:mat)	# Can do, since mat is integer
	aTable$TimeToMat	= aTable$Index				# See above comment
	aTable$ZeroRate		= approxExtrap(ycData$TimeToMat, ycData$MktRate, aTable$TimeToMat)$y
	aTable$DF			= exp(-aTable$TimeToMat*aTable$ZeroRate)	# To be consistent with HW paper
	aTable$alpha		= 0
	
	# Create rates tree
	ratesList	= vector('list', nSteps + 1)
	QList		= ratesList
	jList		= ratesList
	for(ii in 1:length(ratesList)) {
		nNodes		= min(2*ii - 1, jMax*2 + 1)
		jVec		= -min(ii-1, jMax):min(ii-1, jMax)
		jInd		= which(jAll %in% jVec)
		rateVec		= jVec*delr
		
		# Calculate required shift (alpha)
		qVec = rep(1, nNodes)
		if(nNodes == 1) {
			qVec = 1
		} else {
			qVecPrev = QList[[ii-1]]
			jVecPrev = jList[[ii-1]]
			
			rateVecPrev	= jVecPrev*delr
			
			jIndPrev = which(jAll %in% jVecPrev)
			
			# Loop from bottom node to top
			for(jj in 1:nNodes) {
				# Find index of all prior nodes that can reach node (ii,jj)
				probVec		= jTransMat[jIndPrev, jInd[jj]]
				qVec[jj]	= sum( qVecPrev*probVec*exp( -(aTable$alpha[ii-1] + rateVecPrev)*delT) )
			}
			
		}
		QList[[ii]] = qVec
		jList[[ii]] = jVec
		
		aTable$alpha[ii] = ( log(sum(qVec*exp(-rateVec*delT))) - log(aTable$DF[ii+1]) ) / delT	# HW 1994 eq 5 (rearranged)
		
		ratesList[[ii]] = rateVec + aTable$alpha[ii]
	}
	
	
	# Test 1: 2yr ZCB
	mat1			= 2
	nSteps1			= mat1/delT
	ratesFinal		= ratesList[[nSteps1 + 1]]
	payoutLoop		= rep(1, length(ratesFinal))
	
	# Traverse tree backwards
	payoutList = ratesList[1:nSteps1]
	for(ii in (nSteps1):1) {
		
		jVec		= jList[[ii]]
		jVecNext	= jList[[ii+1]]
		jInd		= which(jAll %in% jVec)
		jIndNext	= which(jAll %in% jVecNext)
		
		ratesLoop	= ratesList[[ii]]
		payoutNext	= payoutLoop
		payoutLoop	= rep(0, length(jVec))
		
		for(jj in 1:length(jVec)) {
			probVec			= jTransMat[jInd[jj], jIndNext]
			payoutLoop[jj]	= sum(probVec*payoutNext)*exp(-ratesLoop[jj]*delT)
		}
		payoutList[[ii]] = payoutLoop
	}
	
	# Test 2: 3yr option on 9yr ZCB
	
	# 1st get terminal payoff
	# This involves getting ZCB prices at 3yrs
	
	t1 = expiry
	t2 = expiry + delT	# To calculate r_t1
	t3 = mat

	DF_t1		= subset(aTable, TimeToMat == t1, DF, drop=T)
	DF_t2		= subset(aTable, TimeToMat == t2, DF, drop=T)
	DF_t3		= subset(aTable, TimeToMat == t3, DF, drop=T)
	# F_t1		= (approx(aTable$TimeToMat, aTable$ZeroRate, 2.9)$y*2.9 - approx(aTable$TimeToMat, aTable$ZeroRate, 3.1)$y*3.1) / 0.2
	F_t1		= differ(aTable$TimeToMat, aTable$ZeroRate*aTable$TimeToMat, 3, 0.2)

	B_12			= (1 - exp(-a*(t2 - t1))) / a
	A_12			= (DF_t2/DF_t1) * exp( B_12*F_t1 - (sig^2)*(B_12^2)*(1 - exp(-2*a*t1))/(4*a) )
	B_13			= (1 - exp(-a*(t3 - t1))) / a
	A_13			= (DF_t3/DF_t1) * exp( B_13*F_t1 - (sig^2)*(B_13^2)*(1 - exp(-2*a*t1))/(4*a) )
	
	rateVec_t1	= ratesList[[t1 + 1]]
	r_t1		= (rateVec_t1 + log(A_12)) / B_12

	P_13 = A_13*exp(-B_13*r_t1)	# ZCB price at option expiry (t1)
	
	zcCall = pmax(strike - 100*P_13, 0)
	
	# Finally, work backwards through tree
	nSteps2			= expiry/delT
	payoutLoop		= zcCall
	
	for(ii in (nSteps2):1) {
	
		jVec		= jList[[ii]]
		jVecNext	= jList[[ii+1]]
		jInd		= which(jAll %in% jVec)
		jIndNext	= which(jAll %in% jVecNext)
		
		ratesLoop	= ratesList[[ii]]
		payoutNext	= payoutLoop
		payoutLoop	= rep(0, length(jVec))
		
		for(jj in 1:length(jVec)) {
			probVec			= jTransMat[jInd[jj], jIndNext]
			payoutLoop[jj]	= sum(probVec*payoutNext)*exp(-ratesLoop[jj]*delT)
		}
		payoutList[[ii]] = payoutLoop
	
	}
	
}

differ = function(xx, yy, x1, dx) {
	xlow = x1 - dx
	xhigh = x1 + dx
	(approxExtrap(xx, yy, xhigh)$y - approxExtrap(xx, yy, xlow)$y) / (2*dx)
}


transProb = function(j, M, jMax) {
# transProb
# Trans probs for equation:
#	dx = -(ax)dt + (sig)dz
# where x = f(r) = r

	# Precalculate
	jm		= j*M
	j2m2	= jm^2

	if(j >= jMax) {
		# Top edge of tree
		p_u = 7/6 + (j2m2 + 3*jm)/2
		p_m = -1/3 - j2m2 - 2*jm
		p_d = 1/6 + (j2m2 + jm)/2
	} else if (j <= -jMax) {
		# Bottom edge of tree
		p_u = 1/6 + (j2m2 - jm)/2
		p_m = -1/3 - j2m2 + 2*jm
		p_d = 7/6 + (j2m2 - 3*jm)/2
	} else {
		# Standard tree branch
		p_u = 1/6 + (j2m2 + jm)/2
		p_m = 2/3 - j2m2
		p_d = 1/6 + (j2m2 - jm)/2
	}
	
	return(c(p_d, p_m, p_u))
}

transProbMat = function(M, jMax) {
	
	# Create transition matrix
	jInd		= -jMax:jMax
	nMat		= 2*jMax + 1
	jTransMat	= matrix(0, nMat, nMat)
	for(i in 1:nMat) {
		if(!(i %in% c(1, nMat))) {
			jTransMat[i, (i-1):(i+1)] = transProb(jInd[i], M, jMax)
		} else if(i == 1) {
			jTransMat[i, i:(i+2)] = transProb(jInd[i], M, jMax)
		} else if(i == nMat) {
			jTransMat[i, (i-2):i] = transProb(jInd[i], M, jMax)
		}
	}
	row.names(jTransMat) = jInd
	colnames(jTransMat) = jInd
	
	return(jTransMat)
}


# assignProbNode = function(jVec, jVecPrev, j, jTable) {
# # assignProbNode
# # A function to assign transition probabilities to a given node from
# # all previous nodes

# # To test
# jVec = 1:5 - 3
# jVecPrev = 1:3 - 2

	# nNodes		= length(jVec)
	# nNodesPrev	= length(jVecPrev)
	# jMax		= max(jVec)
	
	# # Error checks - do we need?
	# if(TRUE) {	# Can switch off...
		# if(abs(-min(jVec) - jMax) > 10^-8) {
			# stop('**assignProbNode: jVec must be symetric!\n')
		# }
		# if(abs(-min(jVecPrev) - max(jVecPrev)) > 10^-8) {
			# stop('**assignProbNode: jVecPrev must be symetric!\n')
		# } 
		# if( (nNodes != nNodesPrev) | (nNodes != nNodesPrev+2)) {
			# stop('**assignProbNode: length of jVecPrev not compatible with jVec!\n')
		# }
	# }

	# # Prob assignment algorithm
	# pVec	= jVecPrev*0
	# jDiff	= jMax - abs(j)	# Distance of j relative to edge
	# if (nNodes == (nNodesPrev + 2)) {
		# # Normal branching regime
		# if(jDiff >= 2) {
			# # Node position is in 'middle' - three branches connect to this node
			# jPrev = j + (j-1):(j+1)
			# for(ii in 1:nNodesPrev) {
				# pVec[ii] = jTable[jTable$j == jPrev[ii], ii+2]	# +2 to get to Pd in jTable
			# }
		# } else if (jDiff == 1) {
			# # Node is one from the edge - two branch
			# abvInd	= ifelse(j<0, 1, 0)	# 0 if there is a branch from below, 1 if above
			# jPrev	= j + -1:0 + abvInd
			# for(ii in 1:length(jPrev)) {
				# pVec[ii + 1 - abvInd] = jTable[jTable$j == jPrev[ii], ii+2 + abvInd]
			# }
		# } else {
			# # Node is at edge - one branch
			# abvInd		= ifelse(j < 0, 1, 0)
			# index		= ifelse(j < 0, 1, length(pVec))
			# jPrev		= j + 2*abvInd - 1		# jVecPrev[index]
			# pVec[index]	= jTable[jTable$j == jPrev, 3 + 2*(1-abvInd)]
		# }
		
	# } else if (nNodes == nNodesPrev) {
		# # Modified branching regime
		# if(jDiff >= 2) {
			# # Node position is in 'middle' - three branches connect to this node
		# } else if (jDiff == 1) {
			# # Node is one from the edge - three branches connect to this node
		# } else {
			# # Node is at edge - one branch
		# }
	
	# } else if(nNodes == 1) {
		# pOut = 1
	# }
	
# }
















