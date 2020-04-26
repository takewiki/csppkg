

#conn <- tsda::conn_rds('nsic')
#'  获取新的ID
#'
#' @return 返回值
#' @import tsda
#' @export
#'
#' @examples
#' ques_newId()
ques_newId <- function(){
  conn <- conn_rds_nsic()
  sql <-'select max(fid)+1  as fid  from t_tsp_ques'
  r <- sql_select(conn,sql)
  res <- as.character(r$fid)
  return(res)

}


#' 问题提交
#'
#' @param FCspName 客服名称
#' @param FQues 问题
#' @param FAnsw 答案
#' @param FTspName 支持人员
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_commit()
ques_commit <- function(FCspName='bot19',FQues='多少钱',FAnsw='请内部支持',FTspName='badou'){
  conn <- conn_rds_nsic()

  sql <- paste0(  "insert into t_tsp_ques values(",
                  ques_newId(),
                  " , '",as.character(Sys.Date()),"' ,  '",
                  as.character(Sys.time()),"','",
                  FCspName,"','",FQues,"','",FAnsw,"','",FTspName,"',","'1970-1-1',0,0,0,0)")

  #print(sql)
  sql_update(conn,sql)

}


#' 获取问题1
#'
#' @param table 表名
#'
#' @return 返回值
#' @import tsda
#' @import DTedit
#' @export
#'
#' @examples
#' getBooks()
getBooks <- function(table='t_tsp_ques') {
  conn <- conn_rds_nsic()
  sql_header <- sql_gen_select(conn,table = table)
  sql_tail <- ' where FPushStatus=1 and FPullStatus =0 '

  sql <- paste0(sql_header,sql_tail)
  #print(sql)
  books <-sql_select(conn,sql)
  #print(books)
  #针对进行格式化处理
  #如果出来新的数据类型，需要添加格式化函数
  #请修改format_to_dtedit  --formatter.R
  fieldList <-sql_fieldInfo(conn,table)
  print(fieldList)
  for (i in 1:ncol(books)){
    type <-fieldList[i,'FTypeName']
    print(type)
    books[,i] <-format_to_dtedit(type)(books[,i])

  }

  return(books)
}



#' 提交内部支持
#'
#' @param table  表名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' getBooks2()
getBooks2 <- function(table='t_tsp_ques') {
  conn <- conn_rds_nsic()
  sql_header <- sql_gen_select(conn,table = table)
  sql_tail <- ' where FPushStatus = 0 order by FPriorCount desc  '

  sql <- paste0(sql_header,sql_tail)
  #print(sql)
  books <-sql_select(conn,sql)
  #print(books)
  #针对进行格式化处理
  #如果出来新的数据类型，需要添加格式化函数
  #请修改format_to_dtedit  --formatter.R
  fieldList <-sql_fieldInfo(conn,table)
  print(fieldList)
  for (i in 1:ncol(books)){
    type <-fieldList[i,'FTypeName']
    print(type)
    books[,i] <-format_to_dtedit(type)(books[,i])

  }

  return(books)
}



#' 获取最大ID
#'
#' @param conn 连接
#' @param table 表名
#' @param id_var 内码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' getMax_id()
getMax_id <-function(conn,table='t_tsp_ques',id_var='FId'){
  conn <- conn_rds_nsic()
  sql <- sql_gen_select(conn,table,id_var)
  #print(sql)
  r <-sql_select(conn,sql)
  res <- max(as.integer(r[,id_var]))+1
  return(res)
}



#' 新增
#'
#' @param data 数据
#' @param row 行
#' @param table 列表
#' @param f 函数
#' @param id_var 内码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' books.insert.callback()
books.insert.callback <- function(data, row ,table='t_tsp_ques',f=getBooks,id_var='FId') {
  conn <- conn_rds_nsic()
  sql_header <- sql_gen_insert(conn,table)
  fieldList <-sql_fieldInfo(conn,table)
  ncount <-nrow(fieldList)
  res <- character(ncount)
  for (i in 1:ncount){
    col_Insert <-fieldList[i,'FFieldName']
    type <-fieldList[i,'FTypeName']
    if(col_Insert == id_var){
      res[i] <-paste0(' ',getMax_id(conn,table,id_var),' ')
    }else{
      res[i] <- format_to_sqlInsert(type)(data[row,col_Insert])
    }

  }
  sql_body <- paste0(res,collapse = ',')
  query <-paste0(sql_header,sql_body,")")

  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}



#' 更新数据
#'
#' @param data 新数据
#' @param olddata  原数据
#' @param row 行
#' @param table 表
#' @param f 函数
#' @param edit.cols 列名
#' @param id_var 内码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' books.update.callback()
books.update.callback <- function(data, olddata, row,
                                  table='t_tsp_ques',
                                  f=getBooks,
                                  edit.cols = c('FQues','FAnsw'),
                                  id_var='FId')
{
  conn <- conn_rds_nsic()

  sql_body <-'update  t_tsp_ques  set  FPullStatus = 1  '
  print(sql_body)
  sql_tail <-paste0(' where ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_body,sql_tail)

  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}


#' 更校报2
#'
#' @param data 新数据
#' @param olddata 原数据
#' @param row 行
#' @param table 表
#' @param f 函数
#' @param edit.cols 列
#' @param id_var 内码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' books.update.callback2()
books.update.callback2 <- function(data, olddata, row,
                                   table='t_tsp_ques',
                                   f=getBooks2,
                                   edit.cols = c('FQues','FAnsw'),
                                   id_var='FId')
{
  conn <- conn_rds_nsic()
  sql_body <-'update  t_tsp_ques  set  FPriorCount = FPriorCount +  1  '
  print(sql_body)
  sql_tail <-paste0(' where ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_body,sql_tail)

  print(query) # For debugging
  sql_update(conn, query)
  return(f())
}




#' 删除
#'
#' @param data 数据
#' @param row 行
#' @param table 表
#' @param f 函数
#' @param id_var 内码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' books.delete.callback()
books.delete.callback <- function(data, row ,table ='t_tsp_ques',f=getBooks,id_var='FId') {
  conn <- conn_rds_nsic()
  sql_header <- sql_gen_delete(table);
  sql_tail <-paste0('  ',id_var,' = ',data[row,id_var])
  query <- paste0(sql_header,sql_tail)

  #query <- paste0("DELETE FROM  ",table,"  WHERE id = ", data[row,]$id)
  print(query)
  sql_update(conn, query)
  return(f())
}
