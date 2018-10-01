clearProductData <- function(prod.name){
  
    # Clear all product specific data that is loaded in the global environment. The list includes
    # tables/data like PROD.futures, PROD.dte.futures, etc as well as the individual elements
    # that hold futures and option data for matching expirations, ie CNH12, which hold prices
    # for corn futures and options that expire in Mar 2012.
    g_env = globalenv();
    
    # Handle individual products
    futures.var = sprintf("%s.futures", prod.name);
    if(exists(futures.var, envir=g_env)){
        futures.dat      = get(futures.var, envir=g_env);
        individual.prods = colnames(futures.dat);
        
        # Get valid names for deletion
        valid.names      = individual.prods[individual.prods %in% ls(envir=g_env)];
        if(length(valid.names) > 0){
            rm(list=valid.names, envir=g_env);
        }
        
    } else {
        message(sprintf("clearProductData: No data for %s, unable to delete contract data!", prod.name));
    }


    # Handle prod specific tables
    prod.vars = c(".futures.exp", ".dte.futures", ".dte.table", ".option.exps", ".futures");
    nLen      = length(prod.vars);
    for(i in 1:nLen){
        var.name = sprintf("%s%s", prod.name, prod.vars[i]);
        if(exists(var.name, envir=g_env)){
            rm(list=var.name, envir=g_env);
        } else {
            message(sprintf("clearProductData: Unable to delete %s because it does not exist in global envir.", var.name));
        }
    }
}