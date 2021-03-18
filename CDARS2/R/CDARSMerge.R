#' @title Quick merge CDARS files
#' @description Merge CDARS raw xlsx files of the same types (column names) under the same folder automatically, save to SPSS format
#' @param class classes of files to be combined
#' @param to_csv convert to csv if TRUE, or to SPSS
#' @export
#' @import readxl
#' @seealso \code{\link[readxl]{read_xlsx}}
#' @seealso \code{\link[readxl]{excel_sheets}}
#' @keywords
#' @return NULL
#' @details supports xlsx only
#' @examples \dontrun{
#' # CDARSMerge(class = c("Dx","Px"))
#' # CDARSMerge(class = c("alt","DeAth"),TRUE)
#' }
#'
CDARSMerge<-function(class,to_csv=FALSE){
  for(i in 1:length(class)){
    #get all relevant file names
    file<-dir()[regexpr(tolower(class[i]),tolower(dir()))>0&regexpr("xlsx",tolower(dir()))>0]
    print(paste("Number of",class[i],"files:",length(file)))
    dat<-data.frame()
    for(j in 1:length(file)){
      #get worksheets' name
      sheetName<-readxl::excel_sheets(file[j])
      for(k in 1:length(sheetName)){
        #loop through worksheets
        tmp_dat<-as.data.frame(readxl::read_xlsx(file[j],sheet=sheetName[k]))
        if(nrow(tmp_dat)>0){
          tmp_dat %>%
            dplyr::mutate_all(as.character)
          if(length(grep("Reference Key",tmp_dat[,1]))>0){
            tmp_dat_name<-gsub(" ","",paste(tmp_dat[grep("Reference Key",tmp_dat[,1]),]))
            tmp_dat <- tmp_dat[(grep("Reference Key", tmp_dat[,1]) + 1):dim(tmp_dat)[1],]
            names(tmp_dat) <- tmp_dat_name
          }
          if(length(grep("No. of Records",tmp_dat[,1]))>0){
            tmp_dat<-tmp_dat[1:(grep("No. of Records",tmp_dat[,1])-1),]
          }
          names(tmp_dat)<-gsub("[.():,-]","",names(tmp_dat))
          dat<-bind_rows(dat,tmp_dat)
          print(paste("File",j,"processed:",file[j],"worksheet:",sheetName[k]))
        }
      }
    }
    print(paste("Name of columns:",names(dat)))
    if(to_csv){
      write.csv(dat,file=paste(class[i],".csv",sep=""),row.names=FALSE,na="")
    }else{
      names(dat)<-gsub(" ","",names(dat))
      haven::write_sav(dat,paste(class[i],".sav",sep=""))
      print(paste("Done with",length(file),"file of",class[i]))
    }
  }
}


