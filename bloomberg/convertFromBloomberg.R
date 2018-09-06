convertFromBloomberg <- function(product.dir, output.dir, prod.name=NULL){
  
    if(is.null(prod.name)){
        stop("convertFromBloomberg: prod.name must be set for function to be called successfully!");
    }
    # The data has already been downloaded from bloomberg and resides locally. This function
    # will take the raw 'RData' files and convert them to csvs to be used in data analysis
    # further down the line. Raw data must exist in the following layout:
    #             product\
    #                 options    ------ this is where options data is
    #                 futures    ------ the futures prices, should be single file
    #                 tickers    ------ the tickers that correspond with one to one with futures
    #                                   This directory will have possibly 2 files. One file
    #                                   is the raw tickers (usually tickers.RData). The other,
    #                                   optional file is for conversion of the ticker. The file
    #                                   format will be:
    #                                         bbstart, 1
    #                                         bbend,2
    #                                         newticker,CN
    #                                   where bbstart, bbend define where current ticker is and
    #                                   newticker defines how it should be replaced
  
    # Read in tickers
    tick.path      = sprintf("%stickers/", product.dir);
    tick.files     = list.files(tick.path);
    tick.file.idx  = grep("Rdata", tick.files);
    tick.var.name  = load(sprintf("%stickers/%s", product.dir, tick.files[tick.file.idx]));
    local.tickers  = get(tick.var.name);
    
    # Create key/table for possible conversions
    base.ticker= gsub(" Comdty", "", local.tickers);
    ticker.key = data.frame(original=base.ticker, converted=base.ticker, stringsAsFactors=FALSE);
    
    if(length(tick.files) > 1){
        
          # Need to do a conversion. Read conversion file.
          df.conversion = read.csv(sprintf("%stickers/Conversion.txt", product.dir), 
                                   stringsAsFactors=FALSE, header=FALSE);
          start         = as.numeric(df.conversion[df.conversion[,1] == "bbstart", 2]);
          end           = as.numeric(df.conversion[df.conversion[,1] == "bbend", 2]);
          replacement   = df.conversion[df.conversion[,1] == "newticker", 2];
          oldticker     = substr(local.tickers[1], start=start, stop=end);
          local.tickers = gsub(oldticker, replacement, local.tickers);
          
          # Update key
          ticker.key[,2]= gsub(" Comdty", "", local.tickers);
          assign("ticker.key", value=ticker.key, envir=globalenv());
    }
    
    # Now read in futures
    futures.path   = sprintf("%sfutures/", product.dir);
    futures.files  = list.files(futures.path);
    if(length(futures.files) > 1){
        stop("convertFromBloomberg: Unable to continue because too many files in futures directory!");
    }
    futures.data = local(get(load(sprintf("%sfutures/%s", product.dir, futures.files[1]))));
    
    # Make sure futures and ticker match in length
    if(length(futures.data) != length(local.tickers)){
        stop("convertFromBloomberg: Unable to continue because tickers and futures have number mismatch!");
    }
    
    # Create futures table, and save
    futures.table = createFuturesTable(local.tickers, futures.data);
    futures.file  = sprintf("%s%sFuturesPrices.csv", output.dir, prod.name);
    write.table(futures.table, file=futures.file, sep=",", col.names=TRUE, row.names=FALSE);
    
    # Now convert the option files
    option.dir    = sprintf("%soptions/", product.dir);
    tmp.expiry    = prepOptionCSV(option.dir, output.dir);
    
    # Remove ticker key
    if(exists("ticker.key", envir=globalenv())){
        rm(ticker.key, envir=globalenv());
    }
    return(tmp.expiry);
}