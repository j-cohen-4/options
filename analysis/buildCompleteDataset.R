buildCompleteDataset <- function(list.params){
    
    final.params = validateParamList(list.params);
    if(is.null(final.params)){
        message("buildCompleteDataset: param list is invalid. Unable to run!");
        return(NULL);
    }
    
    message("Building implied volatility for an option series with a constant maturity...", appendLF=FALSE);
    const.iv = findConstantImpliedVol(final.params$iv.days, 
                                      nRealizedDays        = 30, 
                                      r                    = 0, 
                                      product              = final.params$product, 
                                      opt.inc              = final.params$k.inc, 
                                      near.days            = final.params$near.days);
    message("DONE", appendLF=TRUE);

    message("Building a constant maturity future contract...", appendLF=FALSE);    
    const.fut = createConstantMaturityCrudeFuture(nDays   = final.params$cmf.days, 
                                                  product = final.params$product);
    # Remove NAs
    const.fut = const.fut[!is.na(const.fut)];
    message("DONE", appendLF=TRUE);
    
    message("Compiling dataset");
    message("\tBase data...", appendLF=FALSE);
    new.data  = buildDatasetForImpliedVol(cmf.ret     = const.fut, 
                                          iv          = const.iv$xts[,1], 
                                          lb          = final.params$cone.len,
                                          period.days = final.params$look.fwd);
    message("DONE", appendLF=TRUE);
    
    message("\tAppending Moving Averages...", appendLF=FALSE);
    new.data = appendToIVDataset(new.data, ma.length=final.params$iv.ma.len);
    message("DONE", appendLF=TRUE);
    
    message("\tAppending futures and futures moving average...", appendLF=FALSE);
    new.data = addFuturesMA(new.data, const.fut, ma_length=final.params$cmf.ma.len);
    message("DONE", appendLF=TRUE);
    
    return(new.data);
}

createDatasetParamList <- function(iv.days    = NULL,
                                   cmf.days   = NULL,
                                   product    = NULL,
                                   k.inc      = NULL,
                                   near.days  = NULL,
                                   cone.len   = (252 * 5),
                                   look.fwd   = 30,
                                   iv.ma.len  = 50,
                                   cmf.ma.len = 120){
  
    return(list(type       = "data.param.list",
                iv.days    = iv.days,
                cmf.days   = cmf.days,
                product    = product,
                k.inc      = k.inc,
                near.days  = near.days,
                cone.len   = cone.len,
                look.fwd   = look.fwd,
                iv.ma.len  = iv.ma.len,
                cmf.ma.len = cmf.ma.len));
}

validateParamList <- function(data.param.list){
  
    # Get names
    param.names = names(data.param.list);
    
    if(!("type" %in% param.names)){
        message("Param list is improper class -- does not have 'type'!");
        return(NULL);
    }
    
    if(data.param.list$type != "data.param.list"){
        message("Param list 'type' indicates incorrect value!");
        return(NULL);
    }
    
    params = c("iv.days","cmf.days","product","k.inc","near.days","cone.len","look.fwd","iv.ma.len","cmf.ma.len");
    if(sum(params %in% names(data.param.list)) < length(params)){
        message("Param list is missing some parameters!");
        return(NULL);
    }
    
    if(is.null(data.param.list$iv.days)){
        message("iv.days needs to have a value!");
        return(NULL);
    }
    if(is.null(data.param.list$cmf.days)){
        data.param.list$cmf.days = data.param.list$iv.days;
    }
    if(is.null(data.param.list$product)){
        message("Param list needs to specify a product!");
        return(NULL);
    }
    if(is.null(data.param.list$k.inc)){
        message("Param list needs to specify 'k.inc'!");
        return(NULL);
    }
    if(is.null(data.param.list$near.days)){
        message("Param list needs to specify 'near.days'!");
        return(NULL);
    }
    if(is.null(data.param.list$cone.len)){
        message("Param list has empty 'cone.len. Using 5 year default!");
        data.param.list$cone.len = 5 * 252;
    }
    if(is.null(data.param.list$look.fwd)){
        message("Param list has empty 'look.fwd. Using 30 days!");
        data.param.list$look.fwd = 30;
    }
    if(is.null(data.param.list$iv.ma.len)){
        message("Param list has empty 'iv.ma.len. Using 50 days!");
        data.param.list$iv.ma.len = 50;
    }
    if(is.null(data.param.list$cmf.ma.len)){
        message("Param list has empty 'cmf.ma.len'. Using 'iv.ma.len' value!");
        data.param.list$cmf.ma.len = data.param.list$iv.ma.len;
    }
    
    return(data.param.list);
}