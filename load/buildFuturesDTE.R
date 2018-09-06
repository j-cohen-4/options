buildFuturesDTE <- function(product="crude"){
  
    g_env                = globalenv();
    prod.futures.exp.var = sprintf("%s.futures.exp", product);
    prod.futures.var     = sprintf("%s.futures", product);
    
    # Need to have crude.futures.exp loaded
    if(!exists(prod.futures.exp.var, envir=g_env)){
        stop(sprintf("buildFuturesDTE: Unable to calc because %s not loaded!", prod.futures.exp.var));
    }
    prod.futures.exp = get(prod.futures.exp.var, envir=g_env);
    
    # Need to have crude.futures loaded
    if(!exists(prod.futures.var, envir=g_env)){
        stop(sprintf("buildFuturesDTE: Unable to calc because %s not loaded!", prod.futures.var));
    }
    prod.futures = get(prod.futures.var, env=g_env);
  
    nCols = nrow(prod.futures.exp);
    for(i in 1:nCols){
      
        exp = prod.futures.exp[i, 'date'];
        sym = prod.futures.exp[i, 'symbol'];
        
        if(i == 1){
            dte = xts(exp - index(prod.futures), order.by=index(prod.futures));
        } else {
            dte = cbind(dte, xts(exp - index(prod.futures), order.by=index(prod.futures)));
        }
    }
    dte[dte < 0]  = NA;
    colnames(dte) = prod.futures.exp$symbol;
    prod.dte.var  = sprintf("%s.dte.futures", product);
    assign(x=prod.dte.var, value=dte, envir=globalenv());
}