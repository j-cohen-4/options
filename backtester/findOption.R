findOption <- function(dtCurrent, dte, dDelta, result.var=0.05, product=NULL){
  
    if(is.null(product)){
        stop("findOption: 'product' not specified!");
    }
  
    # On date dtCurrent, find an option that most clearly resembles an option that has 'dte'
    # days to expiration and a delta of dDelta
  
    g_env   = globalenv();
    dte.var = sprintf("%s.dte.table", product);
    
    # Empty return object
    opt = list(contract=NULL, DTE=NA);
    
    if(!exists(dte.var, envir=g_env)){
        message(sprintf("findOption: Unable to continue because '%s' not loaded!", dte.var));
        return(opt);
    }
    prod.dte.table = get(dte.var, envir=g_env);
    
    # Get subset at/near dtCurrent
    dte.sub = prod.dte.table[index(prod.dte.table) <= dtCurrent, ];
    dte.sub = tail(dte.sub, 1);
    theDate = index(dte.sub);
    
    # Get option closest to dte that is wanted
    days     = abs(dte.sub - dte);
    idx      = which(days == min(days[1, !is.na(days)]));
    contract = colnames(dte.sub)[idx];
    
    # Finding option -- if positive look at calls, negative is puts
    dat = get(contract, envir=g_env)[theDate, ];
    tbl = generateOptionDatTbl(dat);
    
    # Return option
    if(dDelta > 0){
        tbl = tbl[tbl$delta > 0, ];
    } else {
        tbl = tbl[tbl$delta < 0, ];
    }
    lrg = tbl[tbl$delta > dDelta, ];
    sml = tbl[tbl$delta < dDelta, ];
    
    if(nrow(lrg) <= 0) {
        val = sml[1,];
    } else if(nrow(sml) <= 0){
        val = tail(lrg, 1);
    } else {
        dLrg = as.numeric(tail(lrg[,'delta'], 1));
        dSml = as.numeric(sml[1,'delta']);
        if(abs(dLrg - dDelta) < abs(dDelta - dSml)){
            val = tail(lrg, 1);
        } else {
            val = sml[1,];
        }
    }
    return(val);
}

getStrikeForDelta <- function(delta, listStrikes){
  
    nLen = length(listStrikes);
    if(nLen == 1){
        return(listStrikes[1]);
    } else if(nLen == 2) {
      
        if(abs(listStrikes[1] - delta) < abs(listStrikes[2] - delta)){
            return(listStrikes[1]);
        } else {
            return(listStrikes[2]);
        }
      
    } else {
        idx = round(nLen/2, 0);
        val = listStrikes[idx];
        if(val == delta){
            return(val);
        } else {
            if(listStrikes[idx] > delta){
                return(getStrikeForDelta(delta, listStrikes[1:idx]));
            } else {
                return(getStrikeForDelta(delta, listStrikes[idx:length(listStrikes)]));
            }
        }
    }
}

getStrikeForDelta_ <- function(delta, listStrikes, result.var=0.5){
  
    nBottom = 1;
    nTop    = length(listStrikes);
    bCont   = TRUE;
    
    while(bCont == TRUE){
      
        nTryIdx = round((nBottom + nTop) / 2, 0);
        dGuess  = listStrikes[nTryIdx];
        
        if(abs(dGuess - delta) < result.var){
            bCont = FALSE;
        } else {
          
            if(dGuess > delta){
                nTop = nTryIdx;
            } else {
                nBottom = nTryIdx;
            }
        }
        
        if(nTop - nBottom == 1){
            if(abs(listStrikes[nTop] - delta) < abs(listStrikes[nBottom] - delta)){
                dGuess = listStrikes[nTop];
            } else {
                dGuess = listStrikes[nBottom];
            }
            bCont = FALSE;
        }
    }
    return(dGuess);
}