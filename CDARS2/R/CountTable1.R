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
