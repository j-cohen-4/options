getOptionPrice <- function(contract, dt, futures.contract, opt.type, k, incl.delta=TRUE){
  
    # Get the option price for the given date. If 'incl.delta' is TRUE also return the
    # option's delta for that day. Return object is a list with named variables: opt.prc,
    # fut.prc, delta
    return.obj = list(opt.prc=NULL, fut.prc=NULL, delta=NULL);
    
    # Start with futures price
    fut.prc = getFuturesPrice(futures.contract, dt);
    if(is.na(fut.prc) || is.null(fut.prc) || fut.prc == 0){
        message("getOptionPrice: Unable to retrieve futures prices so cannot continue!");
        return(NULL);
    }
    
    # Check for valid option price
    dat                = get(futures.contract, envir=globalenv());
    curRow             = dat[dt, ];
    if(is.na(curRow[1, contract])){
        message(sprintf("getOptionPrice: Unable to retrieve options price on %s so cannot continue!", dt));
        return(NULL);
    }
    
    # We know data is good, so collect option info
    return.obj$opt.prc = as.numeric(curRow[1, contract]);
    return.obj$fut.prc = fut.prc;
    
    if(incl.delta){
        dte = as.numeric(curRow[1, "DTE"]) / 360;
        if(opt.type == "C"){
            iv         = bscallimpvol(fut.prc, k=k, r=0, tt=dte, d=0, price=return.obj$opt.prc);
            dlt        = bsopt(s=fut.prc, k=k, v=iv, r=0, tt=dte, d=0)$Call['Delta', 'bscall'];
        } else {
            iv         = bsputimpvol(fut.prc, k=k, r=0, tt=dte, d=0, price=return.obj$opt.prc);
            dlt        = bsopt(s=fut.prc, k=k, v=iv, r=0, tt=dte, d=0)$Put['Delta', 'bsput'];
        }
        return.obj$delta = dlt;
    }
    return(return.obj);
}

getFuturesPrice <- function(futures.contract, dt){
  
    g_env = globalenv();
    if(!exists(futures.contract, envir=g_env)){
        message(sprintf("getFuturesPrice: Returning NULL because %s not in global environment", 
                        futures.contract));
        return(NULL);
    }
  
    prc.tbl = get(futures.contract, envir=g_env);
    prc     = as.numeric(prc.tbl[dt, futures.contract]);
    if(length(prc) == 0){
        message(sprintf("getFuturesPrice: No data for date of %s", dt));
        prc = 0;
    }
    return(prc);
}