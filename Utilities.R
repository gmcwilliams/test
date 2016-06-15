# Utilities.R
# Version 2
# Written by George McWilliams
# Version started on 18th of Nov, 2014

# Generic functions that are useful in many different capacities

# Changes from last version:
# - modified texTable to incorporate functionality from SEC_VALUATION_REPORT.R

# To do:
# - 


# source('//NJFILE02//GFIQSHR//QUANT_RESEARCH//R_FUNCTIONS//Utilities.R')

# library(fame)		# Contains 'day' function
# library(timeDate)
# library(RODBC)
# library(RBloomberg)
# options(stringsAsFactors=FALSE)


# -------------------------------------------------------------------------------------
# Functions

library(RODBC)
library(Hmisc)		# For approxExtrap function
library(tis)		# For between, ymd functions
library(zoo)		# For coredata function
library(bizdays)	# For add.bizdays and other business day handling functionality
library(timeDate)	# For timeLastDayInMonth/Quarter, etc
library(RColorBrewer)	# For brewer.pal function

# ------------------------------------------------------------------------------------------------------------------------
# Utilities Environment

getEnv_Utilities = function() {
# getEnv_Attribution
# A function to return the environment containing all the common file paths and parameters
# for attribution purposes
# Note:
# - This function contains Muni directories too

	ENV_Utilities = new.env()	# Environment containing common variables

	# Parameters
	ENV_Utilities$AcctsCashBM = c('CREDVAL','BRDMARK','IBRDMKT')	# Master list of all accounts with Cash BM
	ENV_Utilities$AcctVarName = 'SNAME'	# Variable name used to refer to ENTITY_SHORT_NAME
	
	return(ENV_Utilities)
}

ENV_Utilities = getEnv_Utilities()	# For use elsewhere in file

# ------------------------------------------------------------------------------------------------------------------------
# General functions
mergeWithOrder = function(x, y, ..., sort=T, keepOrder=1) {
# mergeWithOrder
# this function works just like merge, only that it adds the option to return the merged data.frame 
# ordered by x (keepOrder = 1) or by y (keepOrder = 2)

	addIdCol <- function(DATA)
	{
		data.frame(DATA, ID_SORT = seq_len(nrow(DATA)), check.names=F)
	}
	
	orderByIdAndRemove <- function(DATA)
	{
		# gets in a data.frame with the "ID_SORT" column.  Orders by it and returns it
		if(!any(colnames(DATA)=='ID_SORT')) stop("The function orderByIdAndRemove only works with data.frame objects which includes the 'ID_SORT' order column")

		ss_r = order(DATA$ID_SORT)
		ss_c = colnames(DATA) != 'ID_SORT'
		DATA[ss_r, ss_c]		
	}

	if(!is.null(keepOrder)) {
		if(keepOrder == 1) return(orderByIdAndRemove(merge(x=addIdCol(x), y=y, ..., sort = FALSE)))
		if(keepOrder == 2) return(orderByIdAndRemove(merge(x=x, y=addIdCol(y), ..., sort = FALSE)))
		# if you didn't get "return" by now - issue a warning.
		warning("The function mergeWithOrder only accepts NULL/1/2 values for the keepOrder variable")
	} else {
		return(merge(x=x, y=y, ..., sort=sort))
	}
}

htmlDir = function(dirIn, pattern='.png', outFileName='test', graphsPerLine=5, width=600, height=600) {
# A function to put all the files in dirIn that match pattern into an html file for viewing purposes
	files = dir(dirIn, pattern, full.names=T)
	if(length(files) > 0 ) {
		htmlText = '<html>\n'
		htmlText = paste(htmlText, '</body>\n', sep='')
		fileTag  = ifelse(length(grep('pdf', pattern)) > 0, 'embed', 'img')
		# fileTag  = ifelse(length(grep('pdf', pattern)) > 0, 'iframe', 'img')
		for(i in 1:length(files)) {
			parag=''
			if( i%%graphsPerLine ==0 ) {parag='</p>'}	# End of paragraph character
			# htmlText = paste(htmlText, '<img src="',files[i],'" width="',width,'" height="',height,'"> ',parag,' \n', sep='')
			htmlText = paste(htmlText, '<', fileTag, ' src="',files[i],'" width="',width,'" height="',height,'"> ',parag,' \n', sep='')
		}
		htmlText = paste(htmlText, '</body>\n', sep='')
		htmlText = paste(htmlText, '</html>\n', sep='')
		writeLines(htmlText, paste(dirIn, '//', outFileName, '.html', sep=''))
	} else {
		cat(paste('No files with pattern',pattern,'were found\n'))
	}
}

sanitize = function(x, digits=0) {
# vsanitize
# A function to turn text into LaTeX friendly text

    if (is.na(x)){
        result = ' '
    }   else if (is.numeric(x)) {
        if (x==0)   {
            result = '--'
        } else {
            # d = ifelse(abs(x) > 110, 0, 0)
            # result=formatC(x, digits=digits, big.mark=',', format='f', drop0trailing=FALSE)
            result=formatC(round2(x, digits=digits), big.mark=',', format='f', drop0trailing=TRUE)
        }
    }
    else{
        result = x
        result = gsub('\\\\', 'SANITIZE.BACKSLASH', result)
        result = gsub('$', '\\$', result, fixed = TRUE)
        result = gsub('>', '$>$', result, fixed = TRUE)
        result = gsub('<', '$<$', result, fixed = TRUE)
        result = gsub('|', '$|$', result, fixed = TRUE)
        result = gsub('{', '\\{', result, fixed = TRUE)
        result = gsub('}', '\\}', result, fixed = TRUE)
        result = gsub('%', '\\%', result, fixed = TRUE)
        result = gsub('&', '\\&', result, fixed = TRUE)
        result = gsub('_', '\\_', result, fixed = TRUE)
        result = gsub('#', '\\#', result, fixed = TRUE)
        result = gsub('^', '\\verb|^|', result, fixed = TRUE)
        result = gsub('~', '\\~{}', result, fixed = TRUE)
        result = gsub('SANITIZE.BACKSLASH', '$\\backslash$', result, fixed=TRUE)
    }
      return(result)
}
vsanitize = Vectorize(sanitize)

texTable = function(valDF, 
					tableTitle		= NULL, 
					digits			= 0, 
					rotateHeaders	= F, 
					rotAngle		= 60, 
					varCheck		= NULL, 
					shadeEvenLines	= T, 
					nFooter			= 0,
					tableType		= 'longtable') {
# Function to convert dataframe into LaTeX table
# Notes:
# - digits is number of digits to round numbers to
# - varCheck is the name of the column to check when deciding where to put 
#	horizontal lines. If NA, no lines will be drawn. If there exists a '\'
#	character in the value of varCheck, the string will be split on this
#	character and the first portion of the string will be used.
# - If a column is numeric, it is center-aligned, other it is left-justified
# - tableTitle is a caption above the table's top boundary
# - rotateHeaders = TRUE/FALSE or a vector of TRUE/FALSE of length ncol(valDF)
# - tableType = longtable/tabular. Choose tabular if the resultant table is to be
#	used within a tabular environment (eg UnivAnalysis.tex

	nRows	= nrow(valDF)
	outTxt	= ''
	outTxt	= paste0(outTxt, ' \\rowcolors{1}{}{', ifelse(shadeEvenLines, 'lightgray', ''), '}\n ')
	# outTxt	= paste(outTxt, '\\begin{longtable}{l|', paste(rep(ifelse(rotateHeaders, 'l', 'c'), ncol(valDF)-1), collapse=''),'}\n', sep='')
	outTxt	= paste0(outTxt, '\\begin{', tableType, '}{l|', paste(ifelse(sapply(valDF[,2:ncol(valDF)], is.numeric), 'c', 'l'), collapse=''),'}\n')
	outTxt	= paste(outTxt, ifelse(is.null(tableTitle), '', paste0('\\multicolumn{', ncol(valDF), '}{l}{', tableTitle,'}\\\\\n')))
	outTxt	= paste(outTxt, '\\hline\n')
	outTxt	= paste0(outTxt, ' \\global\\rownum=0\\relax\n ')	# Reset row counter (ensures first row is white)
	
	# Print header
	if(length(rotateHeaders) == 1) {
		outTxt = paste0(outTxt, '{', paste(vsanitize(names(valDF)), collapse=paste0('} & ', ifelse(rotateHeaders, paste0('\\rotatebox{', rotAngle, '}'), ''), '{')), '}')
	} else {
		if(length(rotateHeaders) != ncol(valDF)) {stop('**texTable: length(rotateHeaders) != ncol(valDF)!')}
		for(ii in 1:length(rotateHeaders)) {
			outTxt = paste0(outTxt, ifelse(ii>1, ' & ', ''), ifelse(rotateHeaders[ii], paste0('\\rotatebox{', rotAngle, '}{'), '{'), vsanitize(names(valDF)[ii]), '}')
		}
	}
	outTxt = paste0(outTxt, ' \\\\\\hline',ifelse(tableType=='longtable', '\\endhead', ''),'\n')
	# outTxt = paste(outTxt, '\\\\\\hline\n')
	
	# Print body of table
	currPos = ifelse(is.null(varCheck), 'NOLINE', strsplit(valDF[1, varCheck], '/')[[1]][1])
	for(i in 1:(nRows - nFooter)) {
		nextPos = ifelse(is.null(varCheck), currPos, strsplit(valDF[min(i+1, nRows), varCheck], '/')[[1]][1])
		if(currPos != nextPos) {	# Determine where to place horizontal line
			eol = '\\\\\\hline\n'
		} else {
			eol = '\\\\\n'
		}
		outTxt	= paste(outTxt, paste(vsanitize(valDF[i,], digits), collapse = ' & '), eol)
		currPos	= nextPos
	}
	outTxt = paste(outTxt, '\\hline\n')
	
	if(nFooter > 0) {
		eol		= '\\\\\n'
		outTxt	= paste(outTxt, '\\hiderowcolors\n')
		for(i in (nRows - nFooter + 1):nRows) {
			outTxt	= paste(outTxt, paste(vsanitize(valDF[i,], digits), collapse = ' & '), eol)
		}
		outTxt = paste(outTxt, '\\hline\n')
	}
	outTxt = paste0(outTxt, ' \\end{', tableType, '}\n')
	outTxt = paste(outTxt, '\\rowcolors{1}{}{}\n')

}

texTable_old = function(valDF, digits=0, rotateHeaders=T) {
# Function to convert dataframe into LaTeX table
# OLD VERSION - TO DELETE...

	outTxt = ''
	# nn		= names(valDF)
	outTxt	= paste(outTxt, '\\rowcolors{1}{white}{lightgray}\n')
	outTxt	= paste(outTxt, '\\begin{longtable}{l|', paste(rep(ifelse(rotateHeaders, 'l', 'c'), ncol(valDF)-1), collapse=''),'}\n', sep='')
	outTxt	= paste(outTxt, '{', paste(vsanitize(names(valDF)), collapse=paste0('} & ', ifelse(rotateHeaders, '\\rotatebox{60}', ''), '{')), '}', sep='')
	outTxt	= paste(outTxt, '\\\\\\hline\\endhead\n')
	
	currPos = strsplit(valDF[1,1], '/')[[1]][1]
	for(i in 1:nrow(valDF)) {
		nextPos = strsplit(valDF[min(i+1, nrow(valDF)),1], '/')[[1]][1]
		if(currPos != nextPos) {	# Determine where to place horizontal line
			eol = '\\\\\\hline\n'
		} else {
			eol = '\\\\\n'
		}
		outTxt	= paste(outTxt, paste(vsanitize(valDF[i,], digits), collapse = ' & '), eol)
		currPos	= nextPos
	}
	outTxt = paste(outTxt, '\\hline\n')
	outTxt = paste(outTxt, '\\end{longtable}\n')

}

secant = function(fun, x0, x1, tol=1e-07, niter=500){
# secant
# A simple root finding algorithm

	for ( i in 1:niter ) {
		x2 = x1 - fun(x1)*(x1-x0)/(fun(x1)-fun(x0))
			# cat(paste('Iteration', i,': x0=', x0, 'x1=', x1, 'x2=', x2, '\n'))	# Uncomment for testing
		if (abs(fun(x2)) < tol)
			return(x2)
		x0 = x1
		x1 = x2
	}
	stop("secant: exceeded allowed number of iteractions")
}
secant_Debug = function(fun, x0, x1, tol=1e-07, niter=500){
# secant
# A simple root finding algorithm

	for ( i in 1:niter ) {
		x2 = x1 - fun(x1)*(x1-x0)/(fun(x1)-fun(x0))
			# cat(paste('Iteration', i,': x0=', x0, 'x1=', x1, 'x2=', x2, '\n'))	# Uncomment for testing
		cat(paste('x0 =', x0,'| x1 =', x1, '| x2 =', x2, '\n'))
		if (abs(fun(x2)) < tol)
			return(x2)
		x0 = x1
		x1 = x2
	}
	stop("secant: exceeded allowed number of iteractions")
}

round2 = function(x, digits=0) {
# round2
# A more robust rounding function that treats the rounding of 0.5 correctly

	posneg	= sign(x)
	z		= abs(x)*10^digits
	z		= z + 0.5
	z		= trunc(z)
	z		= z/10^digits
	return(z*posneg)
}

roundDF = function(x, digits=0) {
# roundDF
# A function that rounds numerical entries in a dataframe

    data.frame(
		lapply(x, function(y) {
			if(is.numeric(y)) {
				round2(y, digits)
			} else {
				y
			}
		})
	)
}

isLeapyear = function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

yearFrac = function(startDate, endDate, dayCountBasis=0) {
# Note:
# - startDate/endDate can be vectors as long as the length of one is a multiple of the other
# - basis is defined as:
# 0 - 30/360
# 1 - actual/actual	(ISDA)
# 2 - actual/360
# 3 - actual/365

# For testing
# startDate = as.Date(c('2014/09/11', '2014/10/11'))
# endDate	= as.Date('2017/11/21')

	nDayStart	= length(startDate)
	nDayEnd		= length(endDate)
	if(! (nDayStart%%nDayEnd == 0 | nDayEnd%%nDayStart == 0) ) {
		stop('longer object length is not a multiple of shorter object length')
	}

	diffyrs = switch(as.character(dayCountBasis),
			'0' = {	# 30/360
				yrDiff		= year(endDate) - year(startDate)
				mnthDiff	= month(endDate) - month(startDate)
				dayDiff		= day(endDate) - day(startDate)
				yfr			= yrDiff + mnthDiff/12 + dayDiff/360
			},
			'1' = {	# Actual/Actual
				dayDiff = as.numeric(difftime(endDate, startDate, units='days'))
				
				# Calculate adjustment for leapyears
				# Adjust the number of days that appears in a leapyear by the factor 365/366
				if(nDayStart > nDayEnd) {
					indStart	= 1:length(startDate)
					indEnd		= rep(1:length(endDate), nDayStart/nDayEnd)
				} else {
					indStart	= rep(1:length(startDate), nDayEnd/nDayStart)
					indEnd		= 1:length(endDate)
				}
				
				nDaysVec = rep(0, max(nDayStart, nDayEnd))
				for(i in 1:max(nDayStart, nDayEnd)) {
					startDateLoop	= startDate[indStart[i]]
					endDateLoop		= endDate[indStart[i]]
					yrVec			= year(startDateLoop):year(endDateLoop)
					lyBool			= isLeapyear(yrVec)
					nLeapYears		= sum(lyBool)
					if(nLeapYears > 0) {
						nDaysAdj = 0
						nLeapYears_adj = nLeapYears
						if(lyBool[1]) {	# If first year is leapyear
							nDaysAdj = nDaysAdj + as.numeric(difftime(as.Date(paste0(year(startDateLoop), '/12/31')), startDateLoop, units='days'))
							nLeapYears_adj = nLeapYears_adj-1
						}
						if(lyBool[length(yrVec)]) {	# If last year is leapyear
							nDaysAdj = nDaysAdj + as.numeric(difftime(as.Date(paste0(year(endDateLoop), '/12/31')), endDateLoop, units='days'))
							nLeapYears_adj = nLeapYears_adj-1
						}
						nDaysVec[i] = nDaysAdj + 366*nLeapYears_adj
					}
				}
				
				yfrAdj = (dayDiff - nDaysVec)/365 + nDaysVec/366
			},
			'2' = {	# Actual/360
				as.numeric(difftime(endDate, startDate, units='days'))/360
			},
			'3' = {	# Actual/365
				as.numeric(difftime(endDate, startDate, units='days'))/365
			}
	)

	return(diffyrs)
}

addDurCell = function(dfIn, durCellIn = c('<0','0to3','3to5','5to8','8to10','10+')) {
# addDurCell
# A function that takes effdur and returns durCell

	if(!'EFFDUR' %in% names(dfIn)) {
		warning('**addDurCell: dfIn does not have EFFDUR variable! Using first variable!\n')
		names(dfIn)[1] = 'EFFDUR'
	}
	
	durCell = data.frame(	DUR_CELL	= durCellIn,
							DUR_LWR		= 0,
							DUR_UPR		= Inf)
	dfIn$DUR_CELL = NA
	for(i in 1:nrow(durCell)) {
		cName		= strsplit(durCell$DUR_CELL[i], 'to')[[1]]
		plusBool	= length(grep('[+]', cName)) > 0
		minusBool	= length(grep('[<]', cName)) > 0
		bb 			= gsub('([^0-9.]*)', '', cName)		# Get rid of all non-numerics
		lwr			= as.numeric(ifelse(minusBool, -Inf, bb[1]))
		upr			= as.numeric(ifelse(minusBool, 0, ifelse(plusBool, Inf, bb[2])))
		durCell$DUR_LWR[i] = lwr
		durCell$DUR_UPR[i] = upr
		
		# Assign DUR_CELL to constituents
		dfIn$DUR_CELL[which(dfIn$EFFDUR >= lwr & dfIn$EFFDUR < upr)] = durCell$DUR_CELL[i]
		# dfIn$DUR_CELL[which(dfIn$EFFDUR >= lwr & dfIn$EFFDUR < upr), 'DUR_CELL'] = durCell$DUR_CELL[i]
	}
	return(dfIn$DUR_CELL)
}

reorderCols = function(dfIn, colsMove, moveTo='last', refCol=NULL) {
# reorderCols
# A function to rearrange columns specified by colsMove of a data frame dfIn
# Notes:
# - moveTo = c('first','last','before','after')
# - if moveTo = c('before','after'), refCol is reference column that moveTo is relative to

  temp = setdiff(names(dfIn), colsMove)
  x = switch(
    moveTo,
    first	= dfIn[c(colsMove, temp)],
    last	= dfIn[c(temp, colsMove)],
    before	= {
      if (is.null(refCol)) stop("must specify refCol column")
      if (length(refCol) > 1) stop("refCol must be a single character string")
      dfIn[append(temp, values = colsMove, after = (match(refCol, temp)-1))]
    },
    after	= {
      if (is.null(refCol)) stop("must specify refCol column")
      if (length(refCol) > 1) stop("refCol must be a single character string")
      dfIn[append(temp, values = colsMove, after = (match(refCol, temp)))]
    })
  x
}

mapDf = function(	dfMain, 
					dfMap, 
					colsMatch	= intersect(names(dfMain), names(dfMap)), 
					colMain		= colsMatch, 
					colMap		= colsMatch,
					colReturn	= setdiff(names(dfMap), colMap),
					noMatch		= NA) {
# mapDf
# A function to retrieve values of variables colReturn within dfMap corresponding
# to dfMain based on equal values in colMain and colMap
# Notes:
# - Can be used instead of merge
# - If multiple matches are found, the first match only will be returned
# - NEED TO PUT IN ERROR CHECKS!
# - noMatch affects the nomatch argument in match().
#	- noMatch=0 => nomatch=0 - i.e. no value (empty) is returned for non-matched elements of dfMain
#	- noMatch=NA => nomatch=(nrow(dfMap)+1) - i.e. NA value is returned for non-matched elements

	if(length(colMain) != length(colMap)) {
		stop('Number of columns to match must be equal!\n')
	}
	noMatch2 = ifelse(is.na(noMatch), nrow(dfMap)+1, noMatch)
	if(length(colMain) == 1) {
		return(dfMap[match(dfMain[[colMain]], dfMap[[colMap]], nomatch=noMatch2), colReturn])		# nomatch=(nrow()+1) to return NA (instead of 0)
	} else {
		mainFact	= interaction(dfMain[colMain])
		mapFact		= interaction(dfMap[colMap])
		return(dfMap[match(mainFact, mapFact, nomatch=noMatch2), colReturn])
	}

}

invWhich = function(indices, outlength) {
# A function that performs the inverse operation to which
# i.e. for logical vector x, invWhich(which(x), length(x)) == x

    rv = logical(outlength)
	rv[indices] = TRUE
    return(rv)
}

vecIndSearch = function(numSearch, inVec) {
# vecIndSearch
# A function to find the closest elements in a (sorted) numeric vector, inVec, to
# a given value, numSearch.
# The return value is a vector containing the index of the lower-bound element
# (or 1, if numSearch < inVec[1], or length(inVec)-1, if numSearch >= tail(inVec, 1))
# and the linearly-interpolated lower weight. 

	vecInd1 = min(max(c(which(inVec <= numSearch), 1)), length(inVec)-1)	# Deals with numSearch < min(inVec) and numSearch > max(inVec)
	
	if(numSearch <= inVec[vecInd1]) {
		wdur1 = 1
	} else if (numSearch >= inVec[vecInd1+1]) {
		wdur1 = 0
	} else {
		wdur1 = (inVec[vecInd1+1] - numSearch)/(inVec[vecInd1+1] - inVec[vecInd1])
	}
	
	return(c(vecInd1, wdur1))
}

getCurrentDate = function() {
# getCurrentDate
# A function to query the most recent available date from Oracle

	channel = getConnectionODBC()
		dts = sqlQuery(channel, paste(" select effdate from IPGADM.calendar c where effdate between trunc(sysdate - 10) and trunc(sysdate - 1)",
										" and c.cal_type = 'B'",
										" and country = 'US'"))
	odbcClose(channel)
	
	return(as.Date(max(dts$EFFDATE)))
}

isFeUpdated = function() {

	feCheckQry = paste(' select c.lag_date, a.effdate',
						'   from ipgadm.calendar c,',
						'        (select max(effdate) as effdate from ipgadm.fe_analytics) a',
						'  where c.cal_type = \'B\'',
						'    and c.country = \'US\'',
						'    and c.effdate = trunc(sysdate)')
						
	channel = getConnectionODBC()
		feCheck = sqlQuery(channel, feCheckQry)
	odbcClose(channel)
	
	feCheck$LAG_DATE == feCheck$EFFDATE
}

asDate2 = function(dtVecStr) {
# asDate2
# A function to try different date formats before throwing an error
# Notes:
# - Function assumes date format is consistent for entire vector

# For testing...
# tst1 = '5/1/22'
# tst2 = '20081231'
# tst3 = '02-jan-14'

# dtVecStr = currentAllocation$Maturity
# dtVecStr = tst1
	# Prevent errors if input is already a Date
	if(class(dtVecStr)[1] == 'Date') {
		return(dtVecStr)
	}

	# indNotNa	= !is.na(dtVecStr)
	dtVecStr2	= as.character(dtVecStr[!is.na(dtVecStr)])
	
	if(length(dtVecStr2) > 0) {
		dtStr1 = gsub('[[:blank:]*](.*)$', '', dtVecStr2[1])	# Discard anything after whitespace (get rid of hr:min:sec)
		
		# First, figure out delimiter
		delimAll = gsub('[^[:punct:]]', '', dtStr1)	# Insist that delimiter is punctuation character
		if(nchar(delimAll) > 0) {
			# Error checks...
			if(nchar(delimAll) != 2) {
				stop('**asDate2: Must be exactly 0 or 2 delimiters in date!\n')
			}
			delim1 = substr(delimAll, 1, 1)
			if(delim1 != substr(delimAll, 2, 2)) {
				stop('**asDate2: Delimiters must be identical!\n')
			}

			dtSplit = strsplit(dtStr1, delim1)[[1]]	# Vector of date components - need to find out which is which!
			formatVec = c('','','')
			
			# Next, figure out if year is 2 or 4 digit
			if(length(grep('[0-9]{4}', dtStr1)) > 0) {
				# Then figure out if year is at beginning or end
				if(length(grep('[0-9]{4}', dtSplit[3])) > 0) {
					formatVec[3] = '%Y'
				} else {
					formatVec[1] = '%Y'
				}
			} else {
				# If 2-digit year, year must be at end
				formatVec[3] = '%y'
			}
			
			# Next, check if month is characters
			monCharVec = grep('[[:alpha:]]', dtSplit)
			if(length(monCharVec) > 0) {
				if(nchar(dtSplit[monCharVec]) != 3) {	# Note: must be 3 letters exactly!
					stop('**asDate2: Month must be represented by 3 characters (eg jan, FEB, etc) or 1-2 digits!\n')
				}
				formatVec[monCharVec]			= '%b'
				formatVec[nchar(formatVec)==0]	= '%d'	# Remaining element must be day!
			} else {
				# Figure out if dd/mm or mm/dd
				indRem = which(nchar(formatVec)==0)	# Remaining elements to be determined
				
				dayInd = as.numeric(dtSplit[indRem]) > 12
				if(sum(dayInd) == 1) {	# Unambiguous day element found!
					formatVec[indRem[dayInd]]	= '%d'
					formatVec[indRem[!dayInd]]	= '%m'
				} else {
					# Check next elements in dtVecStr2
					if(length(dtVecStr2) > 1) {
						for(ii in 2:length(dtVecStr2)) {
							dtSplitLoop = strsplit(gsub('[[:blank:]*](.*)$', '', dtVecStr2[ii]), delim1)[[1]]
							dayIndLoop	= as.numeric(dtSplitLoop[indRem]) > 12
							if(sum(dayIndLoop) == 1) {
								formatVec[indRem[dayIndLoop]]	= '%d'
								formatVec[indRem[!dayIndLoop]]	= '%m'
								break
							}
						}
					}
					
					if(sum(nchar(formatVec)==0) > 0) {
						formatVec[indRem[1]] = '%m'
						formatVec[indRem[2]] = '%d'
						warning('**asDate2: Ambiguous date format! Month indistinguishable from day! Defaulting to mm/dd...\n')
						# stop('**asDate2: Ambiguous date format! Month indistinguishable from day!\n')
					}
				}
			}
			
			return(as.Date(dtVecStr, format = paste(formatVec, collapse=delim1)))

		} else {
			# If date is entirely numeric, must be in yyyymmdd format
			if(length(grep('^([0-9]{8})$', dtStr1)) == 0) {
				stop('**asDate2: All-numeric date must be in yyyymmdd format!\n')
			}
			return(as.Date(dtVecStr, format='%Y%m%d'))
		}
	} else {
		return(as.Date(dtVecStr))
	}
}

asNumeric2 = function(x) {
# asNumeric2
# A function to cast as numeric and force an error if x cannot be successfully
# converted to a numeric value

	varName = deparse(substitute(x))

	tryCatch(as.numeric(x),
			warning = function(e) {
				stop(paste('**asNumeric2:', varName,'is of unexpected format!\n',
									'Must be numeric!\n'))
			},
			error = function(e) {
				stop(paste('**asNumeric2:', varName,'is of unexpected format!\n',
									'Must be numeric!\n'))
			}
	)
	
}

calcVolEw = function(dfIn, expParam=NULL, monthlyBool=TRUE) {
# A function to calculate the exp-weighted (annual) standard deviation
# Notes: 
# - expParam = 0.06 corresponds to a half-life of 1yr for monthly data
# - dfIn is assumed to be rate/spread levels

	diffCol			= diff(as.matrix(dfIn))
	nRow			= nrow(diffCol)
	if(is.null(expParam)) {
		weightVec = rep(1, nRow)
	} else {
		weightVec = exp(-(nRow:1)*expParam)
	}
	demeanedDiff = diffCol - apply(diffCol, 2, mean)
	
	stDev = sqrt( ifelse(monthlyBool, 12, 252) * (nRow/(nRow-1)) * colSums(weightVec*demeanedDiff^2) / (sum(weightVec)))
	
}

# ------------------------------------------------------------------------------------------------------------------------
# System functions
resetOptions = function() {
# A function to reset all options to default

  is_win <- (.Platform$OS.type == "windows")
  options(
    add.smooth            = TRUE,
    browserNLdisabled     = FALSE,
    CBoundsCheck          = FALSE,
    check.bounds          = FALSE,
    continue              = "+ ",
    contrasts             = c(
      unordered = "contr.treatment", 
      ordered   = "contr.poly"
    ), 
    defaultPackages       = c(
      "datasets", 
      "utils", 
      "grDevices", 
      "graphics", 
      "stats",
      "methods"
    ),  
	# deparse.cutoff			= NULL,
	# deparse.max.lines		= NULL,
    demo.ask              = "default",
    device                = if(is_win) windows else x11,
    device.ask.default    = FALSE,
    digits                = 7,
    echo                  = TRUE,
    editor                = "internal",
    encoding              = "native.enc",
	error					= NULL,
    example.ask           = "default",
    expressions           = 5000,
    help.search.types     = c("vignette", "demo", "help"),    
    help.try.all.packages = FALSE,
    help_type             = "text",
    HTTPUserAgent         = with(
      R.version, 
      paste0(
        "R (", 
        paste(major, minor, sep = "."), 
        " ", 
        platform, 
        " ", 
        arch, 
        " ", 
        os, 
        ")"
      )
    ),
    internet.info         = 2,
    keep.source           = TRUE,
    keep.source.pkgs      = FALSE,
    locatorBell           = TRUE,
    mailer                = "mailto",
    max.print             = 99999,
    menu.graphics         = TRUE,
    na.action             = "na.omit",
    nwarnings             = 50,
    OutDec                = ".",
    pager                 = "internal",
    papersize             = "a4",
    pdfviewer             = file.path(R.home("bin"), "open.exe"),
    pkgType               = if(is_win) "win.binary" else "source",
    prompt                = "> ",
    repos                 = c(
      CRAN      = "@CRAN@", 
      CRANextra = "http://www.stats.ox.ac.uk/pub/RWin"
    ), 
    scipen                = 0,
    show.coef.Pvalues     = TRUE,
    show.error.messages   = TRUE,
    show.signif.stars     = TRUE,
    str                   = list(
      strict.width = "no",
      digits.d     = 3,
      vec.len      = 4
    ),
    str.dendrogram.last   = "`",
    stringsAsFactors      = TRUE,
    timeout               = 60,
    ts.eps                = 1e-05,
    ts.S.compat           = FALSE,
    unzip                 = "internal",
    useFancyQuotes        = TRUE,
    verbose               = FALSE,
    warn                  = 0,
    warning.length        = 1000,
    width                 = 120,
    windowsTimeouts       = c(100, 500)
  )
}

resetSession = function() {
# A function to call at the start of any script to initialise options,
# connections and devices
# Note:
# - rm(list=ls()) doesn't work as function is not in global environment
	
	resetOptions()
	rm(list=ls(pos='.GlobalEnv'), pos='.GlobalEnv')
	closeAllConnections()
	graphics.off()
	options(stringsAsFactors = FALSE)
	source('//NJFILE02//GFIQSHR//QUANT_RESEARCH//R_FUNCTIONS//Utilities.R')

}

getConnectionODBC = function() {
# getConnectionODBC
# A function to return the an RODBC connection
# To be used so that changing username and password is all done in one place
	# return(odbcConnect(dsn = 'pachp', uid = 'ipgdev', pwd = 'excel94', case = 'toupper'))
	
	tmpDf = read.table('//NJFILE02//GFIQSHR//QUANT_RESEARCH//R_FUNCTIONS//OracleCredentials_George.txt', sep='\t', row.names=1)
	return(odbcConnect(dsn = 'pachp', uid = tmpDf['uid', ], pwd = tmpDf['pwd', ], case = 'toupper'))
	# return(odbcConnect(dsn = 'pachq1', uid = 'pace_masterdbo', pwd = 'pace07', case = 'toupper'))
}

getConnectionODBC_QA = function() {
	return(odbcConnect(dsn = 'pachq1', uid = 'pace_masterdbo', pwd = 'pace07', case = 'toupper'))
}

# File manipulation
getDriveLetter = function(dirPath_unc) {
# getDriveLetter
# A function that takes an existing unc directory path and returns there
# network drive letter

	drivesAll	= paste0(toupper(letters), ':')
	uncPath		= paste0(strsplit(dirPath_unc, '//')[[1]][1:3], collapse='//')	# gsub('^([^//]*//[^//]*)', '\\1', dirPath_unc)

	driveLetter	= NA
	for(i in 1:length(drivesAll)) {
		if(file.exists(gsub(uncPath, drivesAll[i], dirPath_unc))) {
			driveLetter = drivesAll[i]
			break
		}
	}
	
	return(list(driveLetter, uncPath))
}

createDirUnc = function(dirCreate_unc, dirTest_unc) {
# createDir
# A function to create a directory given a UNC path dirCreate_unc
# The function converts the path to a machine-dependent "drive letter" path
# using uncToNetworkPath with dirTest_unc before calling dir.create
# dirTest_unc is necessary, because dirCreate_unc doesn't yet exist!

# Notes:
# - dirCreate_unc and dirTest_unc must reside in the same network drive!

	if(!file.exists(dirCreate_unc)) { 
		letterList = getDriveLetter(dirTest_unc)
		dir.create(gsub(letterList[[2]], letterList[[1]], dirCreate_unc), recursive=T)
	}
}

# ------------------------------------------------------------------------------------------------------------------------
# Plotting functions
# Designed to 
# - Modified from //NJFILE02//GFIQSHR//QUANT_RESEARCH//George//YieldCurve//YC_Functions.r

# To do...
# plotDf_Line = function(dts_, spdMat_, spdLT_, mainTitle_, xLabel_, yLabel_, graphName_=NULL, xLim_=NULL, cex_=1) {
plotDf_Line = function(dfIn, 
						xVar, 
						yVar			= setdiff(names(dfIn), xVar),
						xLab			= xVar,
						yLab			= yVar,
						dateAxisBool	= class(dfIn[[xVar]]) == 'Date',
						graphTitle		= NULL,
						fullChartName	= NULL,
						legendBool		= TRUE,
						cex_			= 1,
						addEndPoints	= FALSE) {
# plotDf_Line
# A function to generate a line graph from a "wide" data frame
# Notes:
# - 

# For testing...
# dfIn = spreadAll
# xVar = 'Date'
# yVar			= setdiff(names(dfIn), xVar)
# xLab			= xVar
# yLab			= 'Spread'
# dateAxisBool	= class(dfIn[[xVar]]) == 'Date'
# graphTitle		= NULL
# fullChartName	= NULL
# legendBool		= TRUE
# cex_			= 1
# addEndPoints	= FALSE
	
	colVecAll	= getColourPalette()
	colVec		= colVecAll[1:length(yVar)]
	
    xPlot = dfIn[[xVar]]
	yPlot = dfIn[yVar]
	
	oneCol = FALSE
	if(ncol(yPlot) == 1 | is.null(ncol(yPlot))) {
		oneCol = TRUE
	}
	
	# X-axis
	# Assuming date
	if(dateAxisBool) {
		xPosAll = 1:length(xPlot)
		xMin 	= 1
		xMax 	= max(xPosAll, na.rm=T)
		nDates	= length(xPosAll)
		myAll	= month(xPlot) + year(xPlot)*100	# Month-Year vec of 
		yearVec = unique(year(xPlot))
		# dateDiff = xAllLabs[length(xAllLabs)] - xAllLabs[1]
		dateDiff = xPlot[length(xPlot)] - xPlot[1]
		
		# First decide on yearly, quarterly, monthly or daily incrememnts
		minYearsX = 5	# min year span for the x-axis to be incremented in years
		if(dateDiff/minYearsX > 360) {	# Yearly
			myChoose = 12 + 100*rep(yearVec, each=1)	# Select December each year
		} else if(4*dateDiff/minYearsX > 360) {	# Quarterly
			myChoose = seq(3,12,3) + 100*rep(yearVec, each=4)
		} else if(12*4*dateDiff/minYearsX > 360) {	# Monthly
			myChoose = 1:12 + 100*rep(yearVec, each=12)
		} else {	
			# Daily
			myAll = day(xPlot) + month(xPlot)*100 + year(xPlot)*10^4
			myChoose = myAll
		}
		
		# Then select subset of myChoose to reduce length to below 11
		while(length(myChoose) > 10) {
			myChoose = myChoose[seq(2, length(myChoose), by=2)]
		}
		
		# if(length(myAll) < 30) {
			# # Daily dates
			# myChoose = myAll
		# } else if( nDates < 260) {	# Monthly
			# myChoose = 1:12 + 100*rep(yearVec, each=12)
		# } else if( nDates < 260*5 ) {	# Quarterlydesdes
			# myChoose = seq(3,12,3) + 100*rep(yearVec, each=4)
		# } else {	# Yearly
			# myChoose = 12 + 100*rep(yearVec, each=1)	# Select December each year
		# }
		# # xPos= rev(na.omit((match(myChoose, rev(myAll), nomatch=0))))		# Rev to match the last day in each month
		xPosRev	= na.omit((match(myChoose, rev(myAll))))		# Rev to match the last day in each month
		xPos	= which(rev(invWhich(xPosRev, length(myAll))))
		
		# xPos	= xPlot[indTick]		# match(rev(rev(xPlot)[indTick]), xPlot)
		xVal	= as.character(xPlot[xPos], format='%b-%y')
	} else {
		# To test...
		xMin 	= min(xPlot, na.rm=T)
		xMax 	= max(xPlot, na.rm=T)
		sciExp	= floor(log10(xMax - xMin))
		fact1	= 10^(sciExp)
		xPos	= 1
		xPosNew	= 1
		while(length(xPosNew) < 10) {
			xPos	= xPosNew
			xPosNew	= seq(fact1*floor(xMin/fact1), fact1*ceiling(xMax/fact1), bx = fact1)
			fact1	= fact1/2
		}
		if(length(xPos) == 1) {
			xPos = xPosNew	# Guard against case where length(xPosNew(1)) >= 10
		}
		xVal	= paste(xPos)
	}
    
    # Y-axis
	yMin 	= min(yPlot, na.rm=T)
	yMax 	= max(yPlot, na.rm=T)
	sciExp	= floor(log10(yMax - yMin))
	fact1	= 10^(sciExp)
	yPos	= 1
	yPosNew	= 1
	while(length(yPosNew) < 10) {
		yPos	= yPosNew
		yPosNew	= seq(fact1*floor(yMin/fact1), fact1*ceiling(yMax/fact1), by = fact1)
		fact1	= fact1/2
	}
	if(length(yPos) == 1) {
		yPos = yPosNew	# Guard against case where length(yPosNew(1)) >= 10
	}
	yLim	= c(min(yPos), yPos[min(which(yPos > yMax))])
	yVal	= paste(yPos)
	
	# Legend position
	posMax = which(yPlot == max(yPlot, na.rm=T)) %% length(xPosAll)
	legendRight = TRUE
	if(posMax > ((xMax - xMin)/2)) {
		legendRight = F
	}

    # Plot
	if(!is.null(fullChartName)) { pdf(fullChartName) }
    plot(c(xMin, xMax), c(yMin, yMax),
				type 	= 'n',
				xlab 	= xLab,
				ylab 	= yLab,
				xlim 	= c(xMin, xMax),
				ylim 	= yLim,
				main 	= graphTitle,
				axes 	= F,
				ljoin	= 2,
				cex		= cex_,
				cex.lab = cex_,
				cex.main = cex_)
	for(jj in 1:ncol(yPlot)) {
		lines(xPosAll, yPlot[,jj], type='l', col=colVec[jj], lty=1, cex=cex_)	# lty=2 is dashed line
		# lines(c(xMin, xMax), rep(spdLT_[jj],2), type='l', col=colVec[jj], lty=2)
		
		# Add end points
		if(addEndPoints) {
			points(tail(xPosAll,1), tail(yPlot[,jj],1), cex=.9*cex_, col=colVec[jj], pch=16)
			text(tail(xPosAll,1), tail(yPlot[,jj],1), labels = round2(tail(yPlot[,jj],1), digits=2), xpd = TRUE, cex=.75*cex_, pos=4, col=colVec[jj])
			# text(tail(xPosAll,1), spdLT_[jj], labels = round2(spdLT_[jj]), xpd = TRUE, cex=.75*cex_, pos=4, col=colVec[jj])	# LT Spread label
		}
	}
	if(!oneCol) {
		legend(ifelse(legendRight, "topright", "topleft"), 
				legend 	= names(yPlot), 
				pch 	= rep(15, ncol(yPlot)), 
				col 	= colVec[1:ncol(yPlot)], 
				horiz 	= F, 
				bg 		= "white", 
				bty		='n',
				cex		= cex_)
	}
    
    axis(1, at = xPos, labels = F, cex = 0.7*cex_, crt = 45)	# Blank label to draw tick marks
	usr = par('usr')
	text(xPos, usr[3] - 0.02*(usr[4] - usr[3]), labels = xVal, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.7*cex_)
    axis(2, at = yPos, labels = yVal, cex = 0.7*cex_, las = 2, cex.axis = cex_)
    box(bty = "l")		# 'l' means two-sided box
	if(!is.null(fullChartName)) { device = dev.off() }
	
}

plotDf_Bar = function(	dfIn, 
						xVar, 
						yVar,
						xLab			= xVar,
						yLab			= yVar,
						labVar			= NULL,
						orderVar		= NULL,
						textAdd			= NULL,
						graphTitle		= NULL,
						fullChartName	= NULL,
						legendBool		= !is.null(labVar),
						beside			= FALSE,
						solidFill		= FALSE,
						horiz			= FALSE,
						sepLines		= FALSE
						) {
# plotDf_Bar
# A function to produce a barplot from a data.frame object
# Notes:
# - labVar is the variable that defines distinct groups to be plotted on each points
#	on x-axis. These groups are included in the legend
# - solidFill=F results in textured fill for bars 
# - beside and horiz are as defined in barPlot

# To test...
# xVar = 'DUR_CELL'
# yVar = 'DC'
# labVar = 'FE_SECTOR'
# orderVar = 'DUR_CELL'
# textAdd = NULL
# fullChartName = NULL
# graphTitle = NULL
# beside = T
# solidFill=TRUE
# horiz = F
# sepLines = FALSE

	if(is.null(labVar)) {
		dfIn$LAB_VAR	= 'ALL'
		labVar			= 'LAB_VAR'
		if(legendBool) {
			legendBool = FALSE
			warning('**plotDf_Bar: Must supply labVar in order to display legend\n')
		}
	}

	nSeries = length(unique(dfIn[[labVar]]))
	# buckets	= as.data.frame(apply(unique(dfIn[c(orderVar,xVar)]), 2, as.character))
	buckets	= unique(dfIn[c(orderVar,xVar)])
	for(kk in 1:ncol(buckets)) {
		buckets[,kk] = tryCatch(as.numeric(buckets[,kk]), warning = function(w) {as.character(buckets[,kk])})
	}
	if(!is.null(orderVar)) {
		buckets = buckets[order(buckets[[orderVar]]),]
	}
	
	plotDf			= reshape(dfIn, idvar=labVar, timevar=xVar, direction='wide', drop=setdiff(names(dfIn), c(xVar, yVar, labVar)))	# c('MW','LWR_YC')
	names(plotDf)	= gsub(paste0(yVar, '.') ,'', names(plotDf))
	rownames(plotDf) = plotDf[[labVar]]
	plotDf[is.na(plotDf)] = 0
	plotDf			= as.matrix(plotDf[buckets[[xVar]]])
	
	plotMatPos = plotMatNeg = plotDf
	plotMatPos[plotDf<0] = 0
	plotMatNeg[plotDf>0] = 0
	
	# Target duration text
	durTxt = ''
	if(!is.null(textAdd)) {
		durTxt = textAdd
	}
	
	# Colour and texture of plotted bars
	# lightCols = c('#FCBBA1,#FED976,#FEE391,#D9F0A3,#C6DBEF,#CCECE6,#D0D1E6,#C7E9C0,#C7E9B4,
					# # FCC5C0,#FDD49E,#DADAEB,#FDD0A2,#BFD3E6,#CCEBC5,#D4B9DA,#D0D1E6,#D9D9D9')
	# # lightCols from:
	# seqNames = rownames(subset(brewer.pal.info, category %in% 'seq'))
	# # ind1	= sample(1:length(seqNames), length(seqNames), replace = FALSE)
	# ind1	= c(14,18,17,15,1,2,10,5,16,13,8,12,7,3,4,11,9,6)
	# colVec = sapply(seqNames[ind1], function(x) {brewer.pal(9, x)[3]})
	
	colVecAll	= getColourPalette()
	colVec		= colVecAll[1:nSeries]
	
	angleVec	= rep(c(0,45,90,135), ceiling(nSeries/4))[1:nSeries]
	densityVec	= 100*c(rep(1, 4), rep(0.5, 4), rep(0.24, 4))[1:nSeries]
	if(solidFill) {
		densityVec = NULL	# Solid coloured bars
	}

	# Y-axis
	yMax		= max(dfIn[[yVar]], na.rm = T)
	yMin		= min(dfIn[[yVar]], 0, na.rm = T)
	spaceVar	= c(0,2)	# Default is c(0,1)
	if(!beside) {
		yMax = max(colSums(plotMatPos))	# Max and min of stacked bars
		yMin = min(colSums(plotMatNeg))
		spaceVar = 0.2
	}
	sciExp	= floor(log10(yMax - yMin))
	# yPos	= seq(10^(sciExp)*floor(yMin/10^(sciExp)), 10^(sciExp)*ceiling(yMax/10^(sciExp)), by = 10^(sciExp))
	fact1	= 10^(sciExp)
	yPos	= 1
	yPosNew	= 1
	while(length(yPosNew) < 10) {
		yPos	= yPosNew
		yPosNew	= seq(fact1*floor(yMin/fact1), fact1*ceiling(yMax/fact1), by = fact1)
		fact1	= fact1/2
	}
	if(length(yPos) == 1) {
		yPos = yPosNew	# Guard against case where length(yPosNew(1)) >= 10
	}
	yLim	= c(min(yPos), yPos[min(which(yPos >= yMax))])
	# yVal	= formatC(yPos, format = "f", digits = max(0,-sciExp))
	yVal	= paste(yPos)
	
	# X-axis
	xLabLine	= 2
	cexXtick	= 1
	xTickLen	= nchar(buckets[[xVar]])
	# xMarFact	= 1
	xLineTot	= 5	# Default value
	yMarFact	= 1
	yLineTot	= 4	# Default value
	cpl			= 3	# Characters per line
	if(horiz) {
		# Need to increase left margin!
		# To do...
		yMarFact = max(1, max(xTickLen)/12)	# Left margin expansion factor (since default left margin holds about 12 chars)
		yLineTot = max(4, floor(max(xTickLen)/3))
	} else {
		# Sort out spacing between xlab and x tick marks (bar labels)
		stInd		= floor(nrow(buckets)/2)-1
		maxLenMid	= max(xTickLen[stInd:min((stInd+3), nrow(buckets))])
		xLabLine	= max(2, ceiling(maxLenMid/cpl))
		# if(maxLen > 5) {
			# xLine = 2	# Shift xlab down a line
		# }
		if(stInd > 5) {
			cexXtick = 0.75	# Decrease font size of bar labels
		}
		xLineTot = max(5, ceiling(max(xTickLen)/cpl)+1, xLabLine+1)
		# xMarFact = max(1, max(xTickLen)/18)	# Bottom margin expansion factor
	}
	
	# Begin plot
	if(!is.null(fullChartName)) { pdf(fullChartName, width=12, height=7) }
	# par(mar=c(xMarFact*5,4*yMarFact,4,1)+0.1)
	par(mar=c(xLineTot,yLineTot,4,1)+0.1)
	barTmp = barplot(plotMatPos,
			axes		= F,
			col			= colVec,
			ylim		= if(horiz) {NULL} else {yLim},
			xlim		= if(horiz) {yLim} else {NULL}, #ifelse(horiz, yLim, NULL),
			main		= graphTitle,
			xlab		= NULL, 
			ylab		= NULL,
			axisnames	= FALSE,	# Suppress bar labels
			space		= spaceVar,
			angle		= angleVec,
			density		= densityVec,
			beside		= beside,
			horiz 		= horiz
	)
	barplot(plotMatNeg,
			add			= T,
			axes		= F,
			col			= colVec,
			angle		= angleVec,
			density		= densityVec,
			space		= spaceVar,
			xlab		= NULL,
			axisnames = FALSE,
			beside		= beside,
			horiz 		= horiz
	)

	# Add text
	rngPlot = range(plotDf[,ncol(plotDf)], na.rm=T)	# Range of last bars
	usrPlot = par('usr')
	lwdPlot	= strheight('')
	# lwdPlot	= par('cxy')[2]
	indY = 3
	adjY = -1
	if(usrPlot[4] - rngPlot[2] > rngPlot[1] - usrPlot[3]) {
		indY = 4
		adjY = 1
	}
	text(usrPlot[2], usrPlot[indY], durTxt, adj=c(1, adjY))
	
	# Add axes
	cexAx = 0.75
	cexPar = par('cex')
	xPos = barTmp
	if(beside) {
		xPos = colMeans(barTmp)
	}
	
	# X- and Y-axis labels
	mtext(ifelse(horiz, yLab, xLab), side=1, outer=F, line=xLabLine, cex=cexAx*cexPar, font=2)
	mtext(ifelse(horiz, xLab, yLab), side=3, at=(usrPlot[1] - yMarFact*0.05*(usrPlot[2] - usrPlot[1])), line=ifelse(horiz, 0, 1), cex=cexAx*cexPar, font=2)
	
	# X and Y axes tick labels
	if(horiz) {
		text(usrPlot[1], xPos, labels = buckets[[xVar]], srt = 0, adj = c(1,0.5), xpd = TRUE, cex=cexXtick*cexAx)
	} else {
		# text(xPos, usrPlot[3] - 0.04*(usrPlot[4] - usrPlot[3]), labels = buckets[[xVar]], srt = 45, pos=NULL, adj=c(0.95,1), xpd = TRUE, cex=cexXtick*cexAx)
		text(xPos, usrPlot[3] - lwdPlot, labels = buckets[[xVar]], srt = 45, pos=NULL, adj=c(0.95,1), xpd = TRUE, cex=cexXtick*cexAx)
	}
	axis(ifelse(horiz, 1, 2), at = yPos, labels = yVal, las = ifelse(horiz, 1, 2), cex.axis=cexAx)			# Y-axis
	if(horiz) {
		abline(v=0)	# Fill in y-axis
	} else {
		abline(0,0)	# Fill in x-axis
	}
	
	# Add faint separating lines
	if(sepLines) {
		nBars = length(xPos)
		midPts = colMeans(rbind(xPos[1:(nBars-1)], xPos[2:nBars]))
		if(horiz) {
			abline(h=midPts, col='lightgray', lty=2)	# Fill in x-axis
		} else {
			abline(v=midPts, col='lightgray', lty=2)	# Fill in y-axis
		}
	}

	if(legendBool) {
		# Calculate legend position
		plotUsr		= par('usr')
		posVec		= c('topleft','topright','bottomleft','bottomright')
		bar_x		= as.vector(barTmp)
		bar_y		= as.vector(plotDf)		#apply(plotDf, 2, range)
		if(!beside) {
			bar_x = as.vector(c(barTmp, barTmp))
			bar_y = c(colSums(plotMatPos), colSums(plotMatNeg))
		}
		bar_x_all = c(bar_x-0.5, bar_x+0.5)	# Since bar width = 1
		bar_y_all = c(bar_y, bar_y)
		
		legendFitsPlotBool		= FALSE
		legendOverlapsBarsBool	= TRUE
		legendCrossesAxisBool	= TRUE
		cexLeg					= cexAx/0.9
		while(!(legendFitsPlotBool & !legendOverlapsBarsBool)) {
			cexLeg = 0.9*cexLeg
			for(kk in 1:nrow(plotDf)) {	# Increase number of legend columns
				for(ll in 1:length(posVec)) {
					legTmp = legend(posVec[ll], legend = rownames(plotDf), 
								ncol = kk, 
								cex = cexLeg,
								plot=F
					)
					rectCoord_x = c(legTmp$rect$left, legTmp$rect$left + legTmp$rect$w)
					rectCoord_y = c(legTmp$rect$top - legTmp$rect$h, legTmp$rect$top)
					if(horiz) {
						yPts = bar_x_all[between(bar_y_all, rectCoord_x[1], rectCoord_x[2])]
					} else {
						yPts = bar_y_all[between(bar_x_all, rectCoord_x[1], rectCoord_x[2])]
					}
					
					legendFitsPlotBool		= (plotUsr[1] <= rectCoord_x[1]) & (plotUsr[2] >= rectCoord_x[2]) &
												(plotUsr[3] <= rectCoord_y[1]) & (plotUsr[4] >= rectCoord_y[2])
					legendOverlapsBarsBool	= min(abs(rectCoord_y)) <= max(sign(rectCoord_y[1])*yPts)
					legendCrossesAxisBool	= sign(ifelse(horiz, rectCoord_x[1], rectCoord_y[1])) != 
												sign(ifelse(horiz, rectCoord_x[2], rectCoord_y[2]))
					if(legendFitsPlotBool & !(legendOverlapsBarsBool | legendCrossesAxisBool)) {
						# If legend meets criteria
						break
					}
				}
				if(legendFitsPlotBool & !(legendOverlapsBarsBool | legendCrossesAxisBool)) {
					# If legend meets criteria
					break
				}
			}
		}
		
		legTmp = legend(posVec[ll], legend = rownames(plotDf), 
					fill = colVec, 
					bg = "white", bty='n',
					angle = angleVec, density = densityVec,
					cex = cexLeg, ncol=kk
		)
	}
	if(!is.null(fullChartName)) { device=dev.off() }

}

# DO WE NEED THIS ONE?!
plotDf_Table = function(dfIn, 
						graphTitle		= NULL,
						fullChartName	= NULL,
						digits			= 0
) {
# plotDf_Table
# A function to display a dataframe in a figure
# Notes:
# - 

# To do:
# - add variable telling where to draw lines

# To test...


	# if(!is.null(fullGraphName)) { pdf(fullGraphName) }
	# plot.new()
	# # par(mar = c(5,0,5,0), oma = c(0,0,0,0), new=T)
	# if(!is.null(fullGraphName)) { plot.new() }
	
    # # par(oma = c(bottom,omaAll,omaAll,omaAll),xpd=NA, mar=c(5.1, 4.1, 4.1, 3))
    # coordLims	= par('usr')
	# xCoord		= mean(coordLims[1:2])
	# yCoord		= mean(coordLims[3:4])
	# addtable2plot(xCoord, yCoord, dfIn, display.colnames=T, 	#round2(dfIn,digits)
					# display.rownames=T, xjust=0.5, yjust=1, cex=1,bty="o", hlines=T, box.col='gray')
	# title(graphTitle, cex.main=2)

	
	library('gridExtra')
	grid.table(tevAgg2)
	
	aa = tableGrob(tevAgg2)
	grid.draw(aa)
	
	tablePlot1 = tableGrob(tevAggTrans[c('MW','DC','SPRD_DC'), -1], show.csep=TRUE, show.rsep=TRUE, show.box=TRUE, separator="grey", name='test')
	tablePlot2 = tableGrob(tevAggTrans[c('TOTAL',factorGroupVec), -1], show.csep=TRUE, show.rsep=TRUE, show.box=TRUE, separator="grey", name='test2')	# , cols=NULL
	# gtable_add_grob(tablePlot1, tablePlot2, t=3, l=1, b=3, r=1)
	grid.arrange(tablePlot1, tablePlot2, nrow=2, heights = c(1,2))
	grid.edit("test",gp=gpar(fontsize=8, lwd=2), equal.width=F, grep=TRUE, global=TRUE)
	grid.edit("test2",gp=gpar(fontsize=8, lwd=2), equal.width=TRUE, grep=TRUE, global=TRUE)
	
	
	
	vp1 <- viewport(width = 0.75, height = 1, x = 0.375, y = .5)
	
	grid.newpage()
	print(tablePlot1, vp = vp1)
	
	grid.draw(tablePlot)
	grid.lines()
	grid.lines(x=c(0.1, 0.9), y=0.5)
	
	if(!is.null(fullGraphName)) { device = dev.off() }


	
	
	
	library('PerformanceAnalytics')
	PerformanceAnalytics::textplot(tevAggTrans, halign='left', hadj=0.5, wrap.colnames=10, rmar=0, col.data=colData)
	
	colData = matrix('black', nrow(tevAggTrans), ncol(tevAggTrans))
	colData[row.names(tevAggTrans) %in% 'TOTAL', ] = 'red'
	colData[row.names(tevAggTrans) %in% factorGroupVec, ] = 'blue'
	aa = gplots::textplot(tevAggTrans, halign='left', hadj=0.5, fixed.width=F, col.data=colData, col.main='grey')
	
	
	



}

getColourPalette = function(paletteNum=1) {
	if(round2(paletteNum)!=paletteNum) {
		stop('**getColourPalette: paletteNum must be an integer!\n')
	}
	palList = list(
		c(brewer.pal(8, 'Set1')[c(2,4)], brewer.pal(8, 'Paired')[c(3,4,6)], brewer.pal(8, 'Pastel1')[c(1,2)], brewer.pal(8, 'Dark2'),
		brewer.pal(8, 'Accent'), brewer.pal(8, 'Set2'), brewer.pal(8, 'Set3'))
	)
	return(palList[[paletteNum]])
}

# ------------------------------------------------------------------------------------------------------------------------
# Portfolio specific functions
applyOverrides = function(port, overridesDf) {
# applyOverrides
# A general function to apply overrides in overridesDf to portfolio holdings in port
	if(nrow(overridesDf) > 0) {
		namesOr		= names(overridesDf)
		namesPort	= names(port)
		
		if('CUSIP8' %in% names(port) & 'CUSIP' %in% names(overridesDf) & !('CUSIP8' %in% names(overridesDf))) {
			overridesDf$CUSIP8 = substr(overridesDf$CUSIP, 1, 8)
		}
		if('CUSIP8' %in% names(overridesDf) & 'CUSIP' %in% names(port) & !('CUSIP8' %in% names(port))) {
			port$CUSIP8 = substr(port$CUSIP, 1, 8)
		}

		idColsAll	= c('SECURITY_ALIAS','CUSIP','CUSIP8','TICKER')	# Columns used to identify entries in port, hierarchical order
		ignoreCols	= c('NOTES')									# Columns to ignoreCols
		idCols		= idColsAll[idColsAll %in% namesOr]
		
		replaceCols = setdiff(namesOr[namesOr %in% namesPort], c(idCols, ignoreCols))
		
		# Loop over idCols
		for(ii in 1:length(idCols)) {
			idColLoop = idCols[ii]
			
			rowKeep = rep(TRUE, nrow(overridesDf))
			if(ii > 1) {
				# Determine which (if any) rows identified by subsequent IDs requires overriding
				rowKeepTmp	= sapply(1:(ii-1), function(x) {is.na(overridesDf[idCols[x]])}, simplify=T)
				rowKeep		= as.logical(apply(rowKeepTmp, 1, prod))
			}
			
			# Loop over fields to replace
			for(jj in 1:length(replaceCols)) {
				repColLoop	= replaceCols[jj]
				replaceRows = which(port[[idColLoop]] %in% subset(overridesDf, rowKeep & !is.na(eval(parse(text=repColLoop))), idColLoop, drop=T))	# & !is.na(overridesDf[[repColLoop]]))
				if(length(replaceRows)) {
					orRows		= match(port[replaceRows, idColLoop], overridesDf[[idColLoop]], nomatch=0)
					port[replaceRows, repColLoop] = overridesDf[orRows, repColLoop]
				}
			}
		}
	}
	
	return(port)
}

getAccountMap = function(accountVec, dtEnd=NULL) {
# getAccountMap
# A function that encodes the query to get the ENTITY_ID given the account SHORT_NAME
# Notes:
# - This function encodes all choices made with regard to combined accounts and especially SANMATEO
# - dtEnd is used to inform choices on benchmark changes

# To do:
# - NEED TO MAKE QUERY DATE-DEPENDENT!!
# - Add to constructBM_Taxable_New and Muni equivalent function

# To test...
# AcctsCashBM = ENV_Utilities$AcctsCashBM

	if(is.null(dtEnd)) {
		dtEnd = getCurrentDate()
	}

	qry_accountMap = paste0('select a.ENTITY_ID, a.ENTITY_SHORT_NAME, a.ENTITY_NAME, a.MANDATE, b.INDEX_ALIAS, im.SRC_INTFC_INST, b.INDEX_NAME, b.IDX_SHORT_NAME, b.ORACLE_VIEW ',
							'from ipgadm.fe_account_map a ',
							'	left join ipgadm.fe_index_map b ',
							"		on replace(coalesce(a.idx_id_tfe, a.idx_id_mfe), '_Citi', '') = b.entity_id ", # Replaces Agg (Citi model) with regular Agg
							'	left join INDEXDBO.INDEX_MASTER im ', 
							'		on b.INDEX_ALIAS = im.INDEX_ALIAS ', 
							'where a.ENTITY_SHORT_NAME in ',
							"('", paste(accountVec, collapse="','"), "')")
	# qry_accountMap = paste0('select a.ENTITY_ID, a.ENTITY_SHORT_NAME, a.MANDATE, b.IDX_SHORT_NAME, b.ORACLE_VIEW ',
							# 'from ipgadm.fe_account_map a ',
							# 'left join ipgadm.fe_index_map b ',
							# 'on a.idx_id_tfe = b.entity_id ',
							# 'where a.ENTITY_SHORT_NAME in ',
							# "('", paste(accountVec, collapse="','"), "')")

	channel = getConnectionODBC()
		accountMap = sqlQuery(channel, qry_accountMap)
	odbcClose(channel)
	
	# Fix index names to include "IPGADM."
	tmp1					= accountMap$ORACLE_VIEW
	accountMap$ORACLE_VIEW	= ifelse(invWhich(grep('\\.', tmp1), length(tmp1)), tmp1, paste0('IPGADM.', tmp1))
	
	# Benchmark changes
	# - Need to add others here...
	accountMap$INDEX_ALIAS[which(accountMap$ENTITY_SHORT_NAME %in% 'SAND')] = 109825	# Manual correction for SAND
	
	# Cash BM
	cashInd = which(accountMap$ENTITY_SHORT_NAME %in% AcctsCashBM)
	if(length(cashInd) > 0) {
		accountMap[cashInd, c('INDEX_NAME','IDX_SHORT_NAME')] = c('CASHUSD','Cash')
	}
	
	# Exception for San Mateo (due to benchmark change)
	indSM = accountVec %in% c('SANMATEO', 'SANMACMB')
	if(sum(indSM) > 0 & dtEnd <= as.Date('2013-07-31')) {
		accountMap$ORACLE_VIEW[indSM]		= 'merr.g0qi_view'
		accountMap$IDX_SHORT_NAME[indSM]	= 'ML TIPS'
	}
	indLeumi = accountVec %in% c('LEUMI')	# Manaul change for Leumi
	if(sum(indLeumi) > 0 & dtEnd <= as.Date('2016-01-31')) {
		accountMap$ORACLE_VIEW[indLeumi]	= 'merr.cf5x_view'
		accountMap$INDEX_ALIAS[indLeumi]	= 105299
		accountMap$IDX_SHORT_NAME[indLeumi]	= 'ML CORP 1-10yr xFIN'
	}
	
	return(accountMap)
}
environment(getAccountMap) = ENV_Utilities

getAccountPositionMap = function(accountVec, dtVec=NULL) {
# getAccountPositionMap
# A function to return the Account-Security mapping for a given set of dates

# To test...
# dtVec = as.Date('2016-01-31','2016-04-30')
# accountVec = c('BRUNEI','MCKESSON')

	if(is.null(dtVec)) {
		dtVec = getCurrentDate()
	}
	
	dtVecQry	= paste(as.character(dtVec, format="'%d-%b-%Y'"), collapse=',')
	acctVecQry	= paste0("'", accountVec, "'", collapse=',')
	
	acctSecQry = paste('select A.Effdate, B.ENTITY_SHORT_NAME as SNAME, A.SECURITY_ALIAS, A.CUSIP, A.ISFUTUREOFFSET, A.MV',
						'from ipgadm.fe_holdings_hist A',
						'  left join ipgadm.fe_account_map B',
						'    on A.ENTITY_ID = B.ENTITY_ID',
						' where A.EFFDATE in (', dtVecQry, ')',
						'   and B.ENTITY_SHORT_NAME in (', acctVecQry, ')')
	channel	= getConnectionODBC()
		acctSecMap = sqlQuery(channel, acctSecQry)
	odbcClose(channel)
	
	# Add MW
	tmv				= aggregate(acctSecMap['MV'], acctSecMap[c('EFFDATE','SNAME')], sum)
	acctSecMap$TMV	= mapDf(acctSecMap[c('EFFDATE','SNAME')], tmv, noMatch=NA)
	acctSecMap$MW	= acctSecMap$MV/acctSecMap$TMV
	
	return(acctSecMap)
}

# TO DO!!!!....
queryAccount = function(accountVec=NULL, dateVec, colNames=NULL, subErrorDates=TRUE) {
# queryAccount
# A function to query account info
# This function encodes the queries used to extract position-level account information from
# the old system (pre July 2012) and the current one (Front End). It also corrects for all 
# known issues, such as missing month-end dates and incorrect sector mappings in the old system.
#
# To be used in attribution, security valuation and any other scripts that require account holdings

# dateVec is a vector of unique dates
# colNames is a vector of all columns to be queried
# If accountVec = NULL, query all accounts

# To do
# - Implement colNames
# - Research queries to include all relevant variables

feDate = as.Date('2012/07/01')	# First date we can use FE_ tables (the 'new' front end) - NEED TO PUT INTO ENVIRONMENT
	accountMap = getAccountMap(accountVec)
	
	# ----------------------------------------
	# Substitute dates with incorrect FE data
	indDtFix = numeric(0)
	if(subErrorDates) {
		# List of all currently known dates that contain errors
		dateFixDf = data.frame(	ErrorDates = as.Date(c('2011-08-31','2013-08-31')),
								FixDates = as.Date(c('2011-08-30','2013-08-29'))
		)
		indDtFix = which(dateVec %in% dateFixDf$ErrorDates)
		if(length(indDtFix) > 0) {
			for(ii in 1:length(indDtFix)) {
				indFixLoop = which(dateFixDf$ErrorDates == dateVec[indDtFix[ii]])
				dateVec[indDtFix[ii]] = dateFixDf$FixDates[indFixLoop]
			}
		}
	}
	# ----------------------------------------
	
	channel	= getConnectionODBC()
		dateVecOld	= dateVec[dateVec < as.Date("2012-07-31")]
		dateVecNew	= as.Date(setdiff(dateVec, dateVecOld))
		# if(min(dateVec) < as.Date("2012-07-31")){
		if(length(dateVecOld) > 0) {
		
			# Correct Account name
			accountVecOld = gsub('ALCATELCMB', 'ALCACMB', accountVec)
			
			# Correct for dates that are absent from DWE_TAXABLE_DB
			dtMissing = data.frame(	dtEom	= as.Date(c('2007-09-30','2007-06-30','2008-05-31','2009-05-31','2010-01-31','2010-02-28','2011-12-31')),
									dtQry	= as.Date(c('2007-09-28','2007-06-29','2008-05-30','2009-05-29','2010-01-28','2010-02-26','2011-12-30')))
			dateVecOldMod				= dateVecOld
			indMissing					= dateVecOldMod %in% dtMissing$dtEom
			dateVecOldMod[indMissing]	= dtMissing$dtQry[match(dateVecOldMod[indMissing], dtMissing$dtEom)]
		
			dtQryOld	= paste0("('", paste(as.character(dateVecOldMod, format = '%d-%b-%Y'), collapse="','"), "')")
			acctQryOld	= paste0("('", paste(accountVecOld, collapse = "','"), "')")
		
			hldHist1 = sqlQuery(channel, paste(' SELECT D.EFFDATE',
											   ' ,D.CUSIP',
											   ' ,D.TICKER',
											   ' ,D.CURRENCY',
											   ' ,D.MV',
											   ' ,coalesce(D.OAS, S.OAS) as OAS',
											   ' ,coalesce(D.YTM, S.YTM) as YTM',
											   # ' ,S.YTM',
											   # ' ,D.EFFDUR as EFFDUR_ALT',
											   # ' ,S.EFFDUR',
											   ' ,coalesce(D.EFFDUR, S.EFFDUR) as EFFDUR',
											   ' ,S.SPRDDUR AS SDUR',
											   ' ,D.BBH_SEC AS FE_SECTOR',
											   ' ,D.PORTCOMP AS SNAME',
											   ' ,D.SECTYPE',
											   ' ,C2.CLASS_1',
											   ' ,C2.CLASS_2',
											   ' ,C2.CLASS_3',
											   ' ,C2.CLASS_4',
											   ' FROM ipgadm.DWE_TAXABLE_DB D',
											   '	left join ipgadm.SYB_ANALYTICS S',
											   '		on SUBSTR(D.CUSIP, 1, 8) = SUBSTR(S.CUSIP9, 1, 8)',
											   '		and D.EFFDATE = S.EFFDATE',
											   '	left join ipgadm.BBH_SEC_CLASS C1',
											   '		on SUBSTR(D.CUSIP, 1, 8) = SUBSTR(C1.CUSIP, 1, 8)',
											   '	left join ipgadm.BBH_CLASS C2',
											   '		on C1.BBH_CLASS_ID = C2.BBH_CLASS_ID',
											   '  where D.EFFDATE in', dtQryOld,
											   ' AND D.PORTCOMP in ', acctQryOld))
			hldHist1$EFFDATE	= as.Date(hldHist1$EFFDATE)
			
			# Add back missing dates
			indAddBack						= hldHist1$EFFDATE %in% dtMissing$dtQry & !(hldHist1$EFFDATE %in% dateVecOld)
			hldHist1$EFFDATE[indAddBack]	= dtMissing$dtEom[match(hldHist1$EFFDATE[indAddBack], dtMissing$dtQry)]
			
			hldHist1$ENTITY_ID	= mapDf(hldHist1['SNAME'], accountMap[c('ENTITY_SHORT_NAME','ENTITY_ID')], colMain='SNAME', colMap='ENTITY_SHORT_NAME', noMatch=NA)
			
			# Below is the official definition (as of 8th June 2015) of FE_SECTOR for DWE_TAXABLE_DB
			hldHist1$FE_SECTOR = replace(hldHist1$FE_SECTOR, hldHist1$CLASS_1 %in% 'CORPORATES', 'CORP')
			hldHist1$FE_SECTOR = replace(hldHist1$FE_SECTOR, hldHist1$SECTYPE %in% 'CMBS', 'CMBS')			# Takes account of UNITOW - This is the only instance where Class_1 = Corporate is wrong!
			hldHist1$FE_SECTOR = replace(hldHist1$FE_SECTOR, hldHist1$CLASS_2 %in% 'MBS_PASSTHROUGH' & hldHist1$SECTYPE %in% 'CMO', 'MBS')	# Takes account of 36176F2J
			hldHist1$FE_SECTOR = replace(hldHist1$FE_SECTOR, hldHist1$FE_SECTOR %in% c('MTG15YR', 'MTG30GNMA', 'MTG30CONV', 'MTGOTHER'), 'MBS')
			hldHist1$FE_SECTOR = replace(hldHist1$FE_SECTOR, hldHist1$TICKER %in% c('TIPS'), 'TIPS')
			hldHist1$FE_SECTOR = replace(hldHist1$FE_SECTOR, hldHist1$FE_SECTOR %in% c('ILSTSY','ILSNOND','TSY') & !(hldHist1$CURRENCY %in% 'USD'), 'ILS')	# Foreign linkers
			hldHist1$FE_SECTOR = replace(hldHist1$FE_SECTOR, hldHist1$FE_SECTOR %in% c('FUTTSY'), 'FT')	# Futures
			
			hldHist1$CURRENCY = NULL	# No longer needed
			
			# Take account of SBAC, GTPCEL (final two cell tower CMBS in the old FE. In new FE they are classified correctly)
			hldHist1$FE_SECTOR = replace(hldHist1$FE_SECTOR, hldHist1$CUSIP %in% c('78403DAC','36192EAA'), 'CMBS')
			
			# Adjust Futures Offset duration
			indOffset = (hldHist1$FE_SECTOR %in% 'FT') & (substr(hldHist1$TICKER, nchar(hldHist1$TICKER)-3, nchar(hldHist1$TICKER)) %in% 'CASH')
			hldHist1$EFFDUR[indOffset] = 0
			
			# hldHist1$EFFDUR		= ifelse(is.na(hldHist1$EFFDUR), hldHist1$EFFDUR_ALT, hldHist1$EFFDUR)		# If SYB_ANALYTICS is not populated for a particular date, use analytics in DWE_TAXABLE_DB
			# hldHist1$SECTYPE   	= NULL
			# hldHist1$EFFDUR_ALT	= NULL
			hldHist1$SECURITY_ALIAS = NA	# To be able to concat with new FE
		}
		if(length(dateVecNew) > 0) {
			dtQryNew	= paste0("('", paste(as.character(dateVecNew, format = '%d-%b-%Y'), collapse="','"), "')")
			acctQryNew	= paste0("('", paste(accountMap$ENTITY_ID, collapse = "','"), "')")
			hldHist2	= sqlQuery(channel, paste(' SELECT H.EFFDATE',
													' ,H.SECURITY_ALIAS',
													' ,H.CUSIP',
													' ,SM.TICKER',
													' ,H.MV', 
													' ,A.OAS', 
													' ,A.YTM', 
													# ' ,A.EFFDUR', 
													# ' ,A.SDUR', 
													' ,DECODE(H.ISFUTUREOFFSET, 1, 0, A.EFFDUR) AS EFFDUR',
													' ,DECODE(H.ISFUTUREOFFSET, 1, 0, A.SDUR) AS SDUR',
													' ,S.FE_SECTOR',
													' ,LC.CLASS_1',
													' ,LC.CLASS_2',
													' ,LC.CLASS_3',
													' ,LC.CLASS_4',
													' ,H.ENTITY_ID',
													# ' ,H.ISFUTUREOFFSET',
													' FROM  ipgadm.FE_HOLDINGS_HIST  H',
													'	left join ipgadm.FE_ANALYTICS_HIST A',
													'		on H.SECURITY_ALIAS = A.SECURITY_ALIAS',
													'  		and H.EFFDATE = A.EFFDATE',
													'	left join ipgadm.FE_SMF SM',
													'  		on H.SECURITY_ALIAS = SM.SECURITY_ALIAS',
													'	left join ipgadm.FE_SECTOR_SORT S',
													'  		on SM.FE_SECTOR = S.FE_SECTOR',
													'   left join IPGADM.LEHM_CLASS LC',
													'    on SM.BARC_CLASS_4 = LC.CLASS_4',
													'  where H.EFFDATE in ', dtQryNew,
													'  AND H.ENTITY_ID in ', acctQryNew))
			hldHist2$EFFDATE	= as.Date(hldHist2$EFFDATE)
			hldHist2$SNAME		= mapDf(hldHist2['ENTITY_ID'], accountMap[c('ENTITY_SHORT_NAME','ENTITY_ID')])
			# hldHist2 = merge(hldHist2, accountsList, by = "ENTITY_ID", all.x = T)
			hldHist2$SECTYPE   	= NA
			
		} else {
			hldHist2 = hldHist1[0,]	# Define with zero rows but all the same columns
		}
		if(length(dateVecOld) == 0) {
			hldHist1 = hldHist2[0,]	# Do same with hldHist1
		}
	odbcClose(channel)
	
	namesAll	= intersect(names(hldHist1), names(hldHist2))
	hldAll		= rbind(hldHist1[namesAll], hldHist2[namesAll])
	tmv			= aggregate(hldAll['MV'], hldAll[c('EFFDATE','SNAME')], sum, na.rm = T)

	hldAll			= merge(hldAll, tmv, by = c('EFFDATE', 'SNAME'), all.x = T, suffix = c('', '_TMV'))
	hldAll$MV_TMV	= mapDf(hldAll[c('EFFDATE', 'SNAME')], tmv)
	hldAll$MW		= hldAll$MV/hldAll$MV_TMV
	hldAll$DC		= hldAll$MW*hldAll$EFFDUR
	hldAll$SDC		= hldAll$MW*hldAll$SDUR
	
	# ----------------------------------------
	# Revert modified dates
	if(subErrorDates & length(indDtFix) > 0) {
		dateFixDf = data.frame(	ErrorDates = as.Date(c('2011-08-31','2013-08-31')),
								FixDates = as.Date(c('2011-08-30','2013-08-29'))
		)
		for(ii in 1:length(indDtFix)) {
			indFixLoop = which(dateFixDf$FixDates == dateVec[indDtFix[ii]])
			hldAll[hldAll$EFFDATE %in% dateFixDf$FixDates[indFixLoop], 'EFFDATE'] = dateFixDf$ErrorDates[indFixLoop]
		}
	}
	# ----------------------------------------

	return(hldAll)
}



# ------------------------------------------------------------------------------------------------------------------------
# Bond pricing functions
bondPrice = function(bond1, dayCountBasis=0, isClean=TRUE) {
# Calculate clean price of a bond
# Note: Yield and coupon must be in % units
# bond1 = data.frame(EFFDATE = as.Date('2016-04-11'), MATDATE = as.Date('2026-04-11'), COUPON=0.05, YTM=0.01, FACE = 100,  FREQ = 2)
    sett		= as.Date(bond1$EFFDATE)
    mat			= as.Date(bond1$MATDATE)
    cpnRate		= as.numeric(bond1$COUPON)
    ytm			= as.numeric(bond1$YTM)
    face		= as.numeric(bond1$FACE)
    f			= as.numeric(bond1$FREQ)
    couponDts	= as.Date( sort(seq.Date(mat, sett, by = paste0('-', 12/f,' month'))) )
    couponDts	= couponDts[couponDts > sett]
	nCpns		= length(couponDts)
    prevCpn		= seq.Date(couponDts[1], len=2, by = paste0('-', 12/f,' month'))[2]
	
    # ai          = (1 - periods[1])*(cpnRate/f)
    ai				= yearFrac(prevCpn, sett, dayCountBasis)*cpnRate*face
    cash			= rep((cpnRate/f)*face, nCpns)
    cash[nCpns]		= face + cash[nCpns]
    dfVec			= (1 + ytm/f)^(-f*yearFrac(sett, couponDts, dayCountBasis))	# Vector of discount factors
    pv1				= sum(dfVec*cash)
    return(pv1 - ifelse(isClean, ai, 0))
}

bondYield = function(bond1, dayCountBasis=0) {
	p1 = as.numeric(bond1$PRICE)
	secant(fun = function(yld) {
				bond1$YTM = yld
				return((bondPrice(bond1, dayCountBasis) - p1))
			}, 
			x0 = 0, 
			x1 = bond1$COUPON + 0.01
	)
	
}

duration = function(bond1) {
	bond_dwn = bond1
	bond_up  = bond1
	rt_shift = .002
	
	bond_dwn$YTM = bond1$YTM - rt_shift
	bond_up$YTM = bond1$YTM + rt_shift
	
	price1 		= bondPrice(bond1)
	price_up 	= bondPrice(bond_up)
	price_dwn 	= bondPrice(bond_dwn)
	dur 		= (price_dwn - price_up)/(2*price1*rt_shift)
	return(dur)
}

macaulayDuration = function(bond1, dayCountBasis=0) {

    sett		= as.Date(bond1$EFFDATE)
    mat			= as.Date(bond1$MATDATE)
    cpnRate		= as.numeric(bond1$COUPON)
    ytm			= as.numeric(bond1$YTM)
    face		= as.numeric(bond1$FACE)
    f			= as.numeric(bond1$FREQ)
    couponDts	= as.Date( sort(seq.Date(mat, sett, by = paste0('-', 12/f,' month'))) )
    couponDts	= couponDts[couponDts > sett]
	nCpns		= length(couponDts)
	
    cash			= rep((cpnRate/f)*(face/100), nCpns)
    cash[nCpns]		= face + cash[nCpns]
	timeToCoupon	= yearFrac(sett, couponDts, dayCountBasis)
    dfVec			= (1 + ytm/f/100)^(-f*timeToCoupon)
    return( sum(dfVec*cash*timeToCoupon) / sum(dfVec*cash) )
}

modifiedDuration = function(bond1, dayCountBasis=0) {
	return( macaulayDuration(bond1, dayCountBasis=0) / (1 + as.numeric(bond1$YTM)/(100*as.numeric(bond1$FREQ))) )
}

getDiscountFactors = function(effdate) {
# A function to get (Treasury) discount factors from Bloomberg
# using zero-coupon bond prices

# effdate = as.Date('2014/09/11')
	library(RBloomberg)

	matVec = c(1:10, 15, 20, 30)
	tickersAll = paste(paste('GS', matVec), 'Index')
    conn = blpConnect()
        zcYield = bdh(conn, tickersAll, 'PX_LAST', as.character(effdate, format='%Y%m%d'), as.character(effdate, format='%Y%m%d'))
    blpDisconnect(conn)
	names(zcYield) = gsub('PX_LAST', 'Yield', names(zcYield))
	zcYield$Maturity		= matVec
	zcYield$DiscountFactor	= (1 + zcYield$Yield/(2*100))^(-2*matVec)	# Factor of 2 due to semiannual yield quoting convention
	
	return(zcYield)
}


# ------------------------------------------------------------------------------------------------------------------------
# Option pricing functions
forwardBondPrice = function(bond1, expiry, discountFactorDF, dayCountBasis=0, isClean=FALSE) {
# Function to calculate the forward price of a coupon-bearing bond
# See Section 3.3 in Hull (4th edition)

	sett		= as.Date(bond1$EFFDATE)
    mat			= as.Date(bond1$MATDATE)
	if(expiry > mat) {
		stop('Expiry date of forward > maturity of underlying!\n')
	}
    cpnRate		= as.numeric(bond1$COUPON)
    face		= as.numeric(bond1$FACE)
    f			= as.numeric(bond1$FREQ)
    couponDts	= as.Date( sort(seq.Date(mat, sett, by = paste0('-', 12/f,' month'))) )
    couponDts	= couponDts[couponDts > sett & couponDts <= expiry]
	nCpns		= length(couponDts)
	
    cash			= rep((cpnRate/f)*(face/100), nCpns)
    cash[nCpns]		= cash[nCpns] + ifelse(expiry==mat, face, 0)
    dfVec			= approxExtrap(discountFactorDF$Maturity, discountFactorDF$DiscountFactor, yearFrac(sett, couponDts, dayCountBasis),  rule=2)$y
	discountToExp	= approxExtrap(discountFactorDF$Maturity, discountFactorDF$DiscountFactor, yearFrac(sett, expiry, dayCountBasis),  rule=2)$y
	return( (bondPrice(bond1, dayCountBasis, isClean) - sum(dfVec*cash)) / discountToExp )
}

blackCall = function(bond1, strike, vol, expiry, discountFactorDF, dayCountBasis=0) {
# Function to calculate the price of a European call option on a coupon-bearing bond
# using Black's model (assuming the distribution of the forward price of the underlying
# follows a lognormal distribution)
# Notes:
# - discountFactorDF is a dataframe with EFFDATE and DiscountFactor columns

	sett			= as.Date(bond1$EFFDATE)
	discountToExp	= approxExtrap(discountFactorDF$Maturity, discountFactorDF$DiscountFactor, yearFrac(sett, expiry, dayCountBasis),  rule=2)$y
	fwdPrice		= forwardBondPrice(bond1, expiry, discountFactorDF, dayCountBasis, isClean=TRUE)
	# fwdPrice		= bond1$PRICE/discountToExp
	# fwdPrice		= bond1$PRICE
	volSqrtT		= vol*sqrt(yearFrac(sett, expiry, dayCountBasis))
	d1				= (log(fwdPrice/strike) + volSqrtT*volSqrtT/2) / volSqrtT
	
	return( discountToExp*( fwdPrice*pnorm(d1) - strike*pnorm(d1 - volSqrtT) ) )
}

blackPut = function(bond1, strike, vol, expiry, discountFactorDF, dayCountBasis=0) {
# Function to calculate the price of a European put option on a coupon-bearing bond
# using Black's model (assuming the distribution of the forward price of the underlying
# follows a lognormal distribution)
# Notes:
# - discountFactorDF is a dataframe with EFFDATE and DiscountFactor columns

	sett			= as.Date(bond1$EFFDATE)
	discountToExp	= approxExtrap(discountFactorDF$Maturity, discountFactorDF$DiscountFactor, expiry,  rule=2)$y
	fwdPrice		= forwardBondPrice(bond1, expiry, discountFactorDF, dayCountBasis)
	volSqrtT		= vol*sqrt(yearFrac(sett, expiry, dayCountBasis))
	d1				= (log(fwdPrice/strike) + volSqrtT*volSqrtT/2) / (volSqrtT)
	
	return( discountToExp*( strike*pnorm(-(d1 - volSqrtT)) - fwdPrice*pnorm(-d1) ) )
}

bondDCF = function(bond1, discountFactorDF, dayCountBasis=0) {
# A function to calculate the present value of future cash flows of
# a bond using the risk-free discount curve

    sett		= as.Date(bond1$EFFDATE)
    mat			= as.Date(bond1$MATDATE)
    cpnRate		= as.numeric(bond1$COUPON)
    ytm			= as.numeric(bond1$YTM)
    face		= as.numeric(bond1$FACE)
    f			= as.numeric(bond1$FREQ)
    couponDts	= as.Date( sort(seq.Date(mat, sett, by = paste0('-', 12/f,' month'))) )
    couponDts	= couponDts[couponDts > sett]
	nCpns		= length(couponDts)
    prevCpn		= seq.Date(couponDts[1], len=2, by = paste0('-', 12/f,' month'))[2]
	
    ai				= yearFrac(prevCpn, sett, dayCountBasis)*cpnRate
    cash			= rep((cpnRate/f)*(face/100), nCpns)
    cash[nCpns]		= face + cash[nCpns]
    dfVec			= approxExtrap(discountFactorDF$Maturity, discountFactorDF$DiscountFactor, yearFrac(sett, couponDts, dayCountBasis),  rule=2)$y
    pv1				= sum(dfVec*cash)
	
	# return(pv1 - ai)	# NOT SURE IF ai IS NEEDED HERE!!!
	return(pv1)
}

parallelSpread = function(bond1, price, discountFactorDF, dayCountBasis=0) {
# A function to calculate the parallel shift to a reference (zero) curve required to 
# match the discounted cash flows with the price
# Used to approximate OAS by inputting the Option-Free price


	secant(fun = function(shift) {
	
				# Change discount curve
				dfLoop					= discountFactorDF
				dfLoop$Yield			= discountFactorDF$Yield + shift
				dfLoop$DiscountFactor	= (1 + dfLoop$Yield/(2*100))^(-2*dfLoop$Maturity)
				
				return(abs(price - bondDCF(bond1, dfLoop, dayCountBasis)))
			}, 
			x0 = 0.1, 
			x1 = 0.2
	)

}


# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------

testFunction1 = function() {
# A function to house all the testing code

	# Test code for bondPrice
	# CUSIP = '452152XX4'
	bond1 = data.frame(	EFFDATE	= as.Date('2014/09/22'),
						MATDATE	= as.Date('2028/05/01'),
						COUPON	= 5.0,
						YTM		= 4.298,
						FACE	= 100,
						FREQ	= 2)
	bond1$PRICE = 107.158
	bond2=bond1

	# bondYield(bond2, 0)
	# macaulayDuration(bond1)
	# modifiedDuration(bond1)

	# Test option pricing functions
	discountFactorDF	= getDiscountFactors(bond1$EFFDATE-3)
	# expiry				= bond1$MATDATE
	expiry = as.Date('2028/02/01')	# Assume expiry 3M before bond matures
	forwardBondPrice(bond1, expiry, discountFactorDF)

	bond3			= bond1
	bond3$EFFDATE	= expiry
	bond3$PRICE		= forwardBondPrice(bond1, expiry, discountFactorDF, isClean=T)

	strike		= 100
	fwdYield	= 100*bond3$FREQ*log(1 + bond3$YTM/bond3$FREQ/100)
	vol			= (26/100)*modifiedDuration(bond1)*fwdYield/100
	blackCall(bond1, strike, vol, expiry, discountFactorDF)

	blackCall(bond1, strike, vol, as.Date('2024/02/01'), discountFactorDF)



	bondDCF(bond1, discountFactorDF)
	parallelSpread(bond1, bond2$PRICE, discountFactorDF, dayCountBasis=0)


	aa		= seq(bond1$EFFDATE, bond1$MATDATE, length.out=20)
	bond4	= coredata(bond1)[rep(seq(nrow(bond1)), length(aa)),]
	bond4$EFFDATE = as.Date(aa)
	bond4$ModDur = 0

	for(ii in 1:nrow(bond4)) {
		bond4$ModDur[ii] = modifiedDuration(bond4[ii, ])
	}

}


missingTest = function(inVar1, inVar2=1) {
# A function to test the missing() function

	if(missing(inVar1)) {
		message(paste('**missingTest: inVar1 is missing\n'))
	}
	if(missing(inVar2)) {
		message(paste('**missingTest: inVar2 is missing\n'))
	}
	message(paste('**missingTest: inVar1 = and inVar2 =', inVar2,'\n'))
}

testFunction2 = function() {

	cat(paste0('Running missingTest function with no inputs:\n'))
	missingTest()
	cat(paste0('Running missingTest function with all inputs:\n'))
	missingTest(1, 2)
}








