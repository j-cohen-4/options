buildCrudePriceTable <- function(futuresSymbol, data.dir="data/crude/", product="crude") {
  
    g_env            = globalenv();
    prod.option.var  = sprintf("%s.option.exps", product);
    prod.futures.var = sprintf("%s.futures", product);
    
    # Bring together loadFuturesPrices and loadOptionPrices
    sOptionFile = sprintf("%s%s Options.csv", data.dir, futuresSymbol);
    
    # Futures and expirations should be in global environment
    if(!exists(prod.futures.var, envir=g_env)){
        prod.futures = loadFuturesPrices(sprintf("%s%sFuturesPrices.csv", data.dir, product));
        assign(prod.futures.var, prod.futures, envir=g_env);
    } else {
        prod.futures = get(prod.futures.var, envir=g_env);
    }
    
    if(!exists(prod.option.var, envir=g_env)){
        prod.option.exps = loadCrudeOptionsExpirations(data.dir=data.dir, product=product);
    } else {
        prod.option.exps = get(prod.option.var, envir=g_env);
    }
    
    # Load and merge
    exp.date = prod.option.exps[prod.option.exps[,1] == futuresSymbol, 2];
    opt      = loadOptionPrices(sOptionFile, expiration=exp.date);
    tbl      = cbind(prod.futures[,futuresSymbol], opt);
    
    # Put in global env too
    assign(x=futuresSymbol, value=tbl, envir=g_env);
    
    return(tbl);
}