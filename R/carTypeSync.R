#' 获取车型信息
#'
#' @return 返回值
#' @export
#'
#' @examples
#' getCarType()
getCarType <- function(){
  conn <- conn_rds_nsic()
  sql <- "select FBrandName,FCartypeName  from t_md_carType"
  mydata  <- sql_select(conn,sql)


  mydata_split <- split(mydata,mydata$FBrandName)

  mydata_res <- lapply(mydata_split,function(item){
    cartype <- item$FCartypeName
    res <- tsdo::vect_as_list(cartype)
    return(res)
  })
  return(mydata_res)
}
