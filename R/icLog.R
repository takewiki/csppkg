#' 写入客服操作日志
#'
#' @param conn 数据库连接
#' @param FNickName 客服名称，默认为登录名
#' @param FQuesText 原始查询问题
#' @param answ  问题回复数据包
#' @param index 可中部分
#' @param type 问题序号
#' @param forHelp 是否内部支持
#'
#' @return 返回值
#' @export
#'
#' @examples
#' icLogUpload()
icLogUpload <- function(conn=conn_rds_nsic(),FNickName='RDS',FQuesText='发现运动版多少钱',answ,index=1,type='A',forHelp=0) {

  if (forHelp ==0){
    value_answNumber <- index
  }else{
    value_answNumber <- 0
  }

  ic_log <-data.frame(FNickName=FNickName,
                        FCreateTime=getTime(),
                        FQuesText = FQuesText,
                        FQuesMatch = answ$FQues[index],
                        FScore = answ$FScore[index],
                        FSysMatch = answ$FQuesMatch[index],
                        FAnswerText = answ$FAnsw[index],
                        FAnswerNumber = value_answNumber,
                        FAnswerType = type,
                        stringsAsFactors = F)
  if (type =='C'){
    ic_log$FAnswerNumber <- 0
  }

  #tsda::upload_data(conn,'t_ic_log',ic_log)
  tsda::db_writeTable(conn=conn,table_name = 't_ic_log',r_object = ic_log,append = T)
}




#' 写入AI操作日志功能
#'
#' @param conn 数据库连接
#' @param FNickName 客服名称
#' @param FQuesText 问题名
#' @param answ  答案
#'
#' @return 返回值
#' @export
#'
#' @examples
#' queryLog_upload()
queryLog_upload <- function(conn=conn_rds_nsic(),FNickName='RDS',FQuesText='发现运动版多少钱',answ) {

  ncount <- nrow(answ)
  aiLog <- data.frame(FNickName = FNickName,
                      FCreateTime=getTime(ncount),
                      FQuesText = rep(FQuesText,ncount),
                      FQuesMatch = answ$FQues,
                      FScore = answ$FScore,
                      FSysMatch = answ$FQuesMatch,
                      FAnswerText = answ$FAnsw,
                      FAnswerNumber = answ$FIndex,
                      stringsAsFactors = F)
   #tsda::upload_data(conn,'t_query_log',aiLog)
   tsda::db_writeTable(conn=conn,table_name = 't_query_log',r_object = aiLog,append = T)

}
