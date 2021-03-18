#' @title Select Parameters for Table1
#' @description
#' @export
#' @import readxl
#' @import haven
#' @seealso \code{\link[readxl]{read_xlsx}}
#' @seealso \code{\link[haven]{read_sav}}
#' @keywords
#' @return NULL
#' @details supports xlsx or spss files only
#' @examples \dontrun{
#' # Table1Par()
#' }
Table1Par<-function(){
  dat_name<-file.choose()
  if(regexpr("xlsx",tolower(dat_name))>0){
    dat<-as.data.frame(read_xlsx(dat_name))
  }else if(regexpr("sav",tolower(dat_name))>0){
    dat<-as.data.frame(read_sav(dat_name))
    }
  form<-as.data.frame(cbind(rep(NA,length(names(dat))),names(dat),matrix(nrow=length(dat),ncol=11)))
  names(form)<-c("order",
                 "par",
                 "Count_par",
                 "CountMissing_par",
                 "percentage_par",
                 "average_par",
                 "StandardDeviation_par",
                 "MissingPercentage_par",
                 "Q2_par",
                 "InterQuartileRange_par",
                 "Fisher_par",
                 "StudentT_par",
                 "MannWhitney_par")
 form[is.na(form)]<-""
 write.csv(form,file="Table1Par.csv",row.names=FALSE)
}
