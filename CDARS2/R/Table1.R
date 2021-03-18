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
Table1<-function(){
  options(pillar.sigfig=5)
  cat("Pick the Table1Par.csv file\n")
  flush.console()
  par<-read.csv(file.choose())
  par<<-par%>%
    filter(!is.na(order))%>%
    arrange(order)
  cat("Pick the data file [sav/xlsx]\n")
  flush.console()
  dat_name<-file.choose()
  if(regexpr("xlsx",tolower(dat_name))>0){
    dat<-as.data.frame(read_xlsx(dat_name))
  }else if(regexpr("sav",tolower(dat_name))>0){
    dat<-as.data.frame(read_sav(dat_name))
  }
  dat<<-dat[,names(dat)%in%par[,"par"]]

  #mean_sd
  summary1<-dat%>%
    group_by(dat[,par[par$"order"==0,"par"]])%>%
    select(starts_with(c(par[!is.na(par$"average_par")|!is.na(par$"StandardDeviation_par"),"par"])))%>%
    summarise(across(.cols=where(is.numeric),.fns=list(Mean=mean,SD=sd),na.rm=TRUE,.names="{col}_{fn}"))%>%
    mutate(across(.cols=where(is.numeric),round,2))

  ###Median_IQR
  summary2<-dat%>%
    group_by(dat[,par[par$"order"==0,"par"]])%>%
    select(starts_with(c(par[!is.na(par$"Q2_par")|!is.na(par$"InterQuartileRange_par"),"par"])))%>%
    summarise(across(.cols=where(is.numeric),.fns=list(median=median, Q1=~quantile(., probs = 0.25,na.rm=TRUE),Q3=~quantile(., probs = 0.75,na.rm=TRUE)),na.rm=TRUE,.names="{col}_{fn}"))%>%
    mutate(across(.cols=where(is.numeric),round,2))

  #count_percent
  summary3<-dat%>%
    select(starts_with(c(par[!is.na(par$"Count_par")|!is.na(par$"percentage_par"),"par"])))%>%
    group_by(dat[,par[par$"order"==0,"par"]])%>%
    count(dat[,c(par[!is.na(par$"Count_par")|!is.na(par$"percentage_par"),"par"])])



}


