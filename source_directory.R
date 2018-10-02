source.directory <- function(directory){
  
    all.files = list.files(directory);
    num.files = length(all.files);
    
    for(i in 1:num.files){
      
        sourced.file = sprintf("%s%s", directory, all.files[i]);
        file.length  = nchar(sourced.file);
        extension    = substr(sourced.file, file.length-1, file.length);
        if(extension != ".R"){
            message(sprintf("Not sourcing %s because is is not a '.R' file.", sourced.file));
        } else {
            source(sourced.file);
        }
    }
}