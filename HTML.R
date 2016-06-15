# HTML.R
# Version 1
# Written by George McWilliams
# Version started on 4th of Apr, 2016
# Taken from AttributionHTML.R, version 3.

# General functions and routines to create html files to be used in attribution,
# risk and anywhere else where html presentation is required

# Changes since last version
#	- 

# source('//NJFILE02//GFIQSHR//QUANT_RESEARCH//R_FUNCTIONS//HTML.R')


source('//NJFILE02//GFIQSHR//QUANT_RESEARCH//R_FUNCTIONS//Utilities.R') # For round2 and htmlDir

#--------------------------------------------------------------------
# HTML functions...

# TO DO
# - 


cleanHtml = function(x, digits=0) {
# cleanHtml
# A function to format 
    if (is.na(x)){
        result = " "
    } else if (is.numeric(x)) {
        if (x==0)   {
            result = "-"
        } else {
            # d = ifelse(abs(x) > 110, 0, 0)
            result = formatC(round2(x),digits=digits,big.mark=",",format="f",drop0trailing=FALSE)
        }
    } else{
		result = x
	}
    return(result)
}
vCleanHtml = Vectorize(cleanHtml)

htmlPage = function(titleIn, pgHeading, tableIn, backLink) {
# htmlPage
# A simple HTML page with a single table and a link back to the previous page
	# link = gsub('//NJFILE02//GFIQSHR', 'J:', backLink)
	link = paste0('\\', gsub('//', '\\\\', backLink))	# Need extra "\\" at start for HTML links
	txtOut	= paste('<!DOCTYPE html>',
					'<html>',
					paste0('<title>', titleIn, '</title>'),
					'<body>',
					'<h1 style="font-size: 20px;">',pgHeading,'</h1>',
					paste('<p><a href="',link,'">Back</a></p>', sep=''),
					paste('<p>', tableIn,'</p><br>', sep=''),
					# paste('<p><a href="',link,'">Back</a></p>', sep=''),
					'</body>',
					'</html>', sep='\n')
}

htmlTable = function(dfIn, styleNum=1, highlightRow = c(3,4,12,13,16), highlightStyle='background-color:#89B8FF;font-weight:bold;border-bottom: 1px solid #000000;') {
	txtOut 	= paste(tableStyles(styleNum), '\n')
	txtOut 	= paste(txtOut,'<table class="tableizer-table"', sep='')
	emptyRow= rep('',,ncol(dfIn))
	
	dfOut = dfIn	# Needed?
	style = ''
	# txtOut = paste(txtOut, htmlLine(names(dfOut[i,]), style, header=T))
	txtOut = paste(txtOut, htmlLine(names(dfOut), style, header=T))
	txtOut = paste(txtOut, htmlLine(emptyRow, 'background-color:#000;'))
	for( i in 1:nrow(dfOut)) {
		nDigit = ifelse(i<3, 0, 0)		# Change here for more decimal places
		if(i %in% highlightRow) {
			styleLoop 	  = paste(style, highlightStyle, sep='')
			# elemStyleLoop = paste(style, 'border: 1px solid #ccc;', sep='')
			elemStyleLoop = paste(style, 'border-bottom: 1px solid #000;border-top: 1px solid #FFF;',  sep='')
			elemStyleLoop = ''
			txtOut = paste(txtOut, htmlLine(emptyRow, 'background-color:#FFFFFF;'))	# Add empty line before highlighted col
			txtOut = paste(txtOut, htmlLine(dfOut[i,], styleLoop, elemStyleLoop, header=F, digits=nDigit))
		} else {
			styleLoop = paste(style, ifelse(i%%2>0, 'background-color:#EEFFFF;', ''), sep='')
			txtOut = paste(txtOut, htmlLine(dfOut[i,], styleLoop, header=F, digits=nDigit))
		}
	}
	txtOut = paste(txtOut, '<table class="tableizer-table">', sep='')
}

htmlTable_Link = function(dfIn, styleNum=1, highlightRow = c(3,4,12,13,16), highlightStyle='background-color:#89B8FF;font-weight:bold;border-bottom: 1px solid #000000;') {
# htmlTable_Link
# A function to create an html table from a data frame and include
# hyperlinks for row elements 

# To test...
# dfIn = hldVolSec_out

	# Preprocess dfIn to identify '_LINK' columns
	linkCol 	= grep('_LINK', names(dfIn))
	dfOut		= dfIn[setdiff(1:ncol(dfIn), linkCol)]
	nLink		= length(linkCol)
	linkedCols	= rep(0, nLink)
	
	# Modify 'Linked' columns to embed link
	if(nLink > 0) {
		dfInNames = names(dfIn)
		dfOutNames = names(dfOut)
		for(jj in 1:nLink) {
			linkedCols[jj] = grep(gsub('_LINK','',dfInNames[linkCol[jj]]), dfOutNames)
			
			linkedColIn = grep(paste0(gsub('_LINK','',dfInNames[linkCol[jj]]), '$'), dfInNames)
			linkedColOut = grep(paste0(gsub('_LINK','',dfInNames[linkCol[jj]]), '$'), dfOutNames)
			
			# Covert linked column into hyperlink element
			listIn	= split(dfIn[c(linkedColIn, linkCol[jj])], row.names(dfIn))
			listOut	= lapply(listIn, function(x) {
				htmlLink(vCleanHtml(x[1], digits=0), x[2])	# Need digits argument...
			})
			
			dfOut[linkedColOut] = unsplit(listOut, row.names(dfIn))
		}
	}
	
	txtOut 	= paste(tableStyles(styleNum), '\n')
	txtOut 	= paste(txtOut,'<table class="tableizer-table"', sep='')
	emptyRow= rep('',,ncol(dfOut))
	
	style = ''
	# txtOut = paste(txtOut, htmlLine(names(dfOut[jj,]), style, header=T))
	txtOut = paste(txtOut, htmlLine(names(dfOut), style, header=T))
	txtOut = paste(txtOut, htmlLine(emptyRow, 'background-color:#000;'))
	for( jj in 1:nrow(dfOut)) {
		nDigit = ifelse(jj<3, 0, 0)		# Change here for more decimal places
		if(jj %in% highlightRow) {
			styleLoop	= highlightStyle
			txtOut		= paste(txtOut, htmlLine(emptyRow, 'background-color:#FFFFFF;'))	# Add empty line before highlighted col
		} else {
			styleLoop = ifelse(jj%%2>0, 'background-color:#EEFFFF;', '')
		}
		txtOut = paste(txtOut, htmlLine(dfOut[jj,], rowStyle=styleLoop, elemStyle='', header=F, digits=nDigit))
	}
	txtOut = paste(txtOut, '<table class="tableizer-table">', sep='')
}

htmlLine = function(rowIn, rowStyle='', elemStyle='', header=F, digits=0) {
	elemH	= ifelse(header, 'th', 'td')
	rowOut 	= vCleanHtml(rowIn, digits=digits)	# Convert dfIn into characters, round numbers etc
	txtOut 	= paste('<tr style="',rowStyle,'"><',elemH,' style="',elemStyle,'">', 
					paste(rowOut, collapse=paste('</',elemH,'><',elemH,' style="',elemStyle,'text-align: center;">', sep='')), 
					'</',elemH,'></tr> \n', sep='') # NOTE: WE USE \n FOR NEWLINES
}

htmlImage = function(fileName, width=600, height=600) {
	fileTag  = ifelse(length(grep('pdf', fileName)) > 0, 'embed', 'img')
	paste0('<',fileTag,' src="', fileName,'" width="',width,'" height="',height,'"> ')
}

# # Function for differing styles in each cell - probably do not need just yet...
# htmlElement = function(dataEl, style='') {
	# txtOut = paste('<td style="',style,'">', dataEl, '</td>', sep='')
# }

htmlLink = function(textIn, linkIn) {
	link = paste0('\\', gsub('//', '\\\\', linkIn))	# Need extra "\\" at start for HTML links
	paste0('<a href="',link,'">',textIn,'</a>')
}

htmlParagraph = function(textIn, styleIn='') {
# Regular text - not in a table
	paste0('<p style="',styleIn,'">',textIn,'</p>')
}

tableStyles = function(styleNum = 1) {
	txtOut = ''
	if(styleNum == 1) {
	# Attribution table style
		txtOut = paste('<style type="text/css">',
						'table.tableizer-table {',
						'	border: 1px solid #000; font-family: Arial, Helvetica, sans-serif;',
						'	font-size: 12px;',
						'	border-collapse: collapse;',
						'} ',
						'.tableizer-table td {',
						'	padding: 2px;',
						'	margin: 3px;',		# Doesnt do anything!
						#'	border: 1px solid #ccc;',
						'	border-left: 1px dotted #D8D5D4;',
						'}',
						'.tableizer-table th {',
						'	padding: 2px;',
						'	background-color: #4D80A6; ',	# background-color: 85bde7, 4D80A6-Orig, 91d0ff-lightest blue
						'	color: #000;',
						'	font-weight: bold;',
						'	border-bottom: 1px solid #000;',
						'	border-left: 1px dotted #D8D5D4;',
						'}',
						'</style>', sep='\n')
	} else if(styleNum == 2) {
	# MVP style
		txtOut = paste('<style type="text/css">',
						'table.tableizer-table {',
						'	border: 1px solid #000; font-family: Arial, Helvetica, sans-serif;',
						'	font-size: 12px;',
						'	border-collapse: collapse;',
						'} ',
						'.tableizer-table td {',
						'	padding: 2px;',
						'	margin: 3px;',		# Doesnt do anything!
						#'	border: 1px solid #ccc;',
						'	border-left: 1px dotted #D8D5D4;',
						'}',
						'.tableizer-table th {',
						'	padding: 2px;',
						'	background-color: #527098; ',	# background-color: 85bde7, 4D80A6-Orig, 91d0ff-lightest blue
						'	color: #f3fdff;',				# Off-white typeface
						'	font-weight: bold;',
						'	border-bottom: 1px solid #000;',
						'	border-left: 1px dotted #D8D5D4;',
						'}',
						'</style>', sep='\n')
	} else {
	}
	txtOut
}


# Need to incorporate Scrolling table from: http://www.imaputz.com/cssStuff/bigFourVersion.html#






# End functions
#--------------------------------------------------------------------









