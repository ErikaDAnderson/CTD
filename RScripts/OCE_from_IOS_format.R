# OCE_from_IOS_format.R
# from MS Teams R gurus chat
# https://teams.microsoft.com/l/message/19:1726bebbe684476ab56d6892a2bd1676@thread.skype/1600722648630?tenantId=1594fdae-a1d9-4405-915d-011467234338&groupId=50a32c0d-d5fe-4368-b95b-4beaaa1ba1a1&parentMessageId=1600722648630&teamName=Science%20PL%20Community%20-%20Communaut%C3%A9%20Scientifique%20LP&channelName=R%20-%20Ask%20a%20Guru&createdTime=1600722648630
# 2020-09

# R code from Emily Chisholm

read.ctd.ios <- function(file){
  
  rawlines <- readLines(file)
  # find end of header
  eoh <- grep(rawlines, pattern = '*END OF HEADER')
  dataStart <- eoh+1
  # split data and header
  #rawdata <- rawlines[eoh +1:length(rawlines)]
  rawheader <- rawlines[1:eoh]
  
  # get variable name lines
  varnamelinestart <- grep(rawheader, pattern = '-1-')
  varnames <- rawheader[varnamelinestart:(eoh-1)]
  # parse out variable names
  varnums <- strsplit(varnames[1], split = ' ')
  
  finalvarnames <- list()
  for (i in 1:length(varnums[[1]])){
    fullvarnum <- varnums[[1]][i]
    
    strlocation <- gregexpr(pattern =fullvarnum,varnames[1])
    enstring <- strlocation[[1]][1]+nchar(fullvarnum)
    rawvarname <- substr(varnames, start = strlocation[[1]][1], stop = enstring)
    rawvarname <- paste0(rawvarname, collapse = '' )
    varname <- gsub(rawvarname, pattern = fullvarnum, replacement = '')
    varname <- gsub(varname, pattern = '!', replacement = '')
    varname <- gsub(varname, pattern = '-', replacement = '')
    varname <- gsub(varname, pattern = ' ', replacement = '')
    finalvarnames[[i]] <- varname
  }
  
  # get data into table (from oce::read.ODF)
  data <- scan(file, what="character", skip=dataStart, quiet=TRUE)
  data <- matrix(data, ncol=length(finalvarnames), byrow=TRUE)
  data <- as.data.frame(data, stringsAsFactors=FALSE)
  # name data columns
  names(data) <- finalvarnames
  # find salintiy and temperature vars if they have unique names
  sal <- grep(finalvarnames, pattern = 'salinity', ignore.case = TRUE, value = TRUE)
  temp <- grep(finalvarnames, pattern = 'temperature', ignore.case = TRUE, value = TRUE)
  pres <- grep(finalvarnames, pattern = 'pressure', ignore.case = TRUE, value = TRUE)
  if(length(sal) ==0 | length(temp) == 0 | length(pres) ==0){
    stop('Could not identify temperature, salinity and pressure variables to convert to oce-ctd!')
  }
  # get into oce format
  
  ctd <- as.ctd(data, salinity = data[[sal]], temperature = data[[temp]], pressure = data[[pres]], conductivity = NA, scan = NA, time = NA)
  # solution for when 'other' argument of as.ctd is deprecated (WARNING this creates duplicate data columns and may need ot be adjusted)
  # otherVars<-  finalvarnames[!finalvarnames %in% c(sal, temp, pres)]
  # for ( o in otherVars){
  #   eval(parse(text = paste0("ctd <- oceSetData(ctd, name = '",o,"', value = data[['",o,"']])")))
  # }
  
  # remove duplicate data?
  ctd@data <- ctd@data[!names(ctd@data) %in% c(sal, temp, pres)]
  
  # put header info into oce-ctd
  
  ctd <-  oceSetMetadata(ctd, name = 'header', value = rawheader)
  # TODO: Parse header into metadata slots
  
  return(ctd)
}
