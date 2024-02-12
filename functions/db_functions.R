get_data = function(table_name){
  
  conn = DBI::dbConnect(RSQLite::SQLite(), "data/raw_data.sqlite")
  
  if (table_name == "tables"){
    data = DBI::dbListTables(conn)
    
    DBI::dbDisconnect(conn)
  } else {
    data = dplyr::tbl(conn, table_name) %>% 
      dplyr::collect()
    
    DBI::dbDisconnect(conn)
  }
  
  return(data)
}

get_tables = function(){
  conn = DBI::dbConnect(RSQLite::SQLite(), "data/raw_data.sqlite")
  
  tables = DBI::dbListTables(conn)
  
  DBI::dbDisconnect(conn)
  
  return(data)
}
