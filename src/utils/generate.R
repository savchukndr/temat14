# Function for genereting data frame
generate.generate_empty_df = function(unique_user_id, unique_item){
  df = data.frame(user_id=c(unique_user_id))
  
  # Filling data frame with empty rating
  for (column in unique_item){
    df[[toString(column)]] = 0
  }
  return(df)
}


# Function for filling data frame
generate.generate_complete_df = function(df, user_id, item, rating){
  
  # browser()
  # Write book ratings into data frame
  for (i in seq(length(rating))){
    col = toString(item[[i]])
    row = which(df$user_id == user_id[[i]])
    m = mean(rating)
    
    df[[col]][[row]] = rating[[i]]
  }
  return(df)
}