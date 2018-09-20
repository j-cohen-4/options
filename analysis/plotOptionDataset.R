plotOptionDataset <- function(opt.ds){
  
    par(mfrow=c(2,1));
  
    # Plot futures with moving average (last 2 columns)
    nCols = ncol(opt.ds);
    plotMultiple(xts(opt.ds[,(nCols-1):nCols], order.by=opt.ds[,1]));
    
    # Plot IV within cone info
    plotMultiple(xts(opt.ds[,c("iv","bottom","two.five","five.zero","seven.five","top")], 
                     order.by=opt.ds[,1]));
    
    # Return to regular params
    par(mfrow=c(1,1));
}