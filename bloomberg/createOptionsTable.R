createOptionsTable <- function(sInputFile){
  
    # Loading an RData file
    dat.name  = load(sInputFile);
    opt.dat   = get(dat.name);
    all.names = names(opt.dat);
    nNum      = length(all.names);
    
    # Base data frame"
    df        = data.frame(matrix("", ncol=nNum*2, nrow=200), stringsAsFactors=FALSE);
    nMax      = 0;
    nLmt      = nNum;
    nSkip     = 0;
    dtExp     = NULL;
    
    for(i in 1:nLmt){
        
        dtCol  = ((i-1-nSkip) * 2) + 1;          # Current date column
        prcCol = dtCol + 1;                      # and price col
        sym    = all.names[i];                   # symbol/strike
        srs    = opt.dat[[i]];                   # Actual data [date,prc]
        
        nRows  = nrow(srs);
        if(nRows == 0){
            nSkip = nSkip + 1;
            next;
        }
        nMax   = ifelse(nRows > nMax, nRows, nMax);
        
        # Fill out data frame
        df[1:nRows, dtCol]  = format(srs[,1], "%m/%d/%Y");
        df[1:nRows, prcCol] = srs[,2];
        if(is.null(dtExp)){
            dtExp = srs[nRows, 1];
        }
        curExp = srs[nRows, 1];
        if(dtExp != curExp){
            #message(sprintf("%s[%s] has different expiration from main[%s]", sym, curExp, dtExp));
            dtExp = max(dtExp, curExp);
        }
        
        if(!exists("hdr")){
            hdr = c(sym, "PX_LAST");
        } else {
            hdr = c(hdr, sym, "PX_LAST");
        }
    }
    
    # Trim appropriately
    nLmt = nLmt - nSkip;
    if(nLmt == 0){
        
        # Empty dataset, so return NULLs
        df    = NULL;
        dtExp = NULL;
        message(sprintf("No options data in file: %s", sInputFile));
    }
    else {
        df           = df[1:nMax, 1:(nLmt * 2)]
        colnames(df) = hdr
    }
    return(list(dat=df, exp=dtExp));
}