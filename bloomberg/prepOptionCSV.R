prepOptionCSV <- function(sInputDir, sOutputDir){
  
    # Read in RData files, create csv
#    input.dir  = crude.option.directory;
#    output.dir = "BloombergData/csv2/";
    input.dir  = sInputDir;
    output.dir = sOutputDir;
    
    # Get files
    opt.files = list.files(input.dir);
    nNum      = length(opt.files);
    df.exp    = data.frame("", stringsAsFactors=FALSE);
    nRow      = 1;
    
    for(i in 1:nNum){
        
        sym = strsplit(opt.files[i], "[.]")[[1]][1];
        sym = getCommonTicker(sym);
        if(nchar(sym) == 4){
            sym = paste(substring(sym, 1, 3), "1", substring(sym, 4, 4), sep="");
        }
        
        # Create table info
        tbl = createOptionsTable(sprintf("%s%s", input.dir, opt.files[i]));
        if(!is.null(tbl$dat)){
            
            df.exp[nRow,1] = sym;
            df.exp[nRow,2] = format(tbl$exp, "%m/%d/%Y");
            nRow           = nRow + 1;
            
            # Save file
            write.table(tbl$dat, file=sprintf("%s%s Options.csv", output.dir, sym), 
                 col.names=TRUE, row.names=FALSE, sep=",");
        }
    }
    
    # Save expirations
    write.table(df.exp, file=sprintf("%sOptionExpirations.csv", output.dir), col.names=FALSE,
                row.names=FALSE, sep=',');
    
    return(df.exp);
}