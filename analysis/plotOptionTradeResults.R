plotOptionTradeResults <- function(trade.data){
  
    # What needs to be passed in is the output from 'runOptionBacktest'. The object is a list
    # that has 2 elements: total, trades
  
    if(class(trade.data) != "list"){
        stop("plotOptionTradeResults: trade.data is not a list object. Unable to run!");
    }
  
    if(sum(c("total","trades") %in% names(trade.data)) < 2){
        stop("plotOptionTradeResults: trade.data has wrong elements. Unable to run!");
    }
    
    # PL
    par(mfrow=c(2,1));
    if(ncol(trade.data$total) == 1){
        plot(cumprod(trade.data$total+1)-1, main="Total PL");
    } else {
        plotMultiple(xts(apply(trade.data$total+1, 2, cumprod)-1, order.by=index(trade.data$total)),
                     theLegend="topleft",
                     theTitle ="Call and Put PL");
    }
    # Trades in a box plot
    boxplot(t(trade.data$trades[,2:4]));
    par(mfrow=c(1,1));
}