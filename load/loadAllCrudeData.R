loadAllCrudeData <- function(data.dir="data/crude/", product="crude"){
    
    g_env = globalenv();
    prod.option.var = sprintf("%s.option.exps", product);
    
    # This function assumes nothing and will load all futures and options contracts
    # as indicated in the OptionsExpirations.csv
    if(!exists(prod.option.var, envir=g_env)){
        prod.option.exps = loadCrudeOptionsExpirations(data.dir=data.dir, product=product);
    } else {
        prod.option.exps = get(prod.option.var, envir=g_env);
    }
    
    # Iterate each symbol to load futures/options
    nLen = nrow(prod.option.exps);
    for(i in 1:nLen){
        sym = prod.option.exps[i, 'symbol'];
        tmp = buildCrudePriceTable(sym, data.dir=data.dir, product=product);
        
        if(i == 1){
            all.syms = sym;
            dte.tbl  = tmp[,'DTE'];
        } else {
            all.syms = c(all.syms, sym);
            dte.tbl  = cbind(dte.tbl, tmp[,'DTE']);
        }
    }
    
    # Fill out header for DTE
    #dte.tbl[is.na(dte.tbl)] = -1;
    colnames(dte.tbl) = all.syms;
    prefix            = substr(prod.option.exps[1, 'symbol'], 1, 2);
    dte.tbl           = sortDTETable(dte.tbl, symbolPrefix=prefix);
    prod.dte.var      = sprintf("%s.dte.table", product);
    assign(prod.dte.var, dte.tbl, envir=g_env);
    
    # Futures expiration and DTE
    buildFuturesExpTable(product=product);
    buildFuturesDTE(product=product);

    # Let user know what and where
    message(sprintf("All data loaded. Main futures table is in '%s.futures'.", product));
    message("Options data is stored in the global environment by ticker name, and");
    message(sprintf("DTE table can be found in '%s.dte.table'.", product));
    message(sprintf("Futures DTE stored in '%s.dte.futures'", product));
}