buildFuturesExpTable <- function(product="crude"){
  
    g_env            = globalenv();
    prod.futures.var = sprintf("%s.futures", product);
    
    # Need to have crude.futures loaded
    if(!exists(prod.futures.var, envir=g_env)){
        stop("buildFuturesExpTable: Unable to build table because futures not loaded!");
    }
    prod.futures = get(prod.futures.var, envir=g_env);
  
    nCols     = ncol(prod.futures);
    lastDate  = index(tail(prod.futures, 1));
    col.names = colnames(prod.futures);
    c.f.e     = data.frame(symbol=character(), date=as.Date(character()), stringsAsFactors=FALSE);
    for(i in 1:nCols){
      
        sym = col.names[i];
        dat = prod.futures[!is.na(prod.futures[,i]), i];
        c.f.e[i, 1] = sym;
        c.f.e[i, 2] = index(tail(dat, 1));
    }
    
    # Last date is not really expiration
    c.f.e = c.f.e[c.f.e[,2] != lastDate,];
    
    # Set in global enviroment
    prod.futures.exp.var = sprintf("%s.futures.exp", product);
    assign(prod.futures.exp.var, value=c.f.e, envir=g_env);
}