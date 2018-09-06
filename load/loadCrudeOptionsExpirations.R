loadCrudeOptionsExpirations <- function(sFile    = "OptionExpirations.csv", 
                                        data.dir = "data/crude/", 
                                        product  = "crude"){
  
    tmp     = read.csv(sprintf("%s%s", data.dir, sFile), header=FALSE, stringsAsFactors=FALSE);
    tmp[,2] = as.Date(tmp[,2], format="%m/%d/%Y");
    colnames(tmp) = c("symbol","date");
    
    # Create name according to product
    prod.option.var = sprintf("%s.option.exps", product);
    assign(prod.option.var, tmp, envir=globalenv());
    
    message(sprintf("%s option expirations loaded in '%s.option.exps' variable",
                      product, product));
    
    return(tmp);
}