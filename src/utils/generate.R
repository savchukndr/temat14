# Function for genereting data frame
generate.generate_empty_df = function(unique_user_id, unique_isbn){
  df = data.frame(user_id=c(unique_user_id))
  
  # Filling data frame with empty rating
  for (column in unique_isbn){
    df[[toString(column)]] = 0
  }
  return(df)
}


# Function for filling data frame
generate.generate_complete_df = function(df, user_id, isbn, rating){
  
  # browser()
  # Write book ratings into data frame
  for (i in seq(length(rating))){
    cols = toString(isbn[[i]])
    row = which(df$user_id == user_id[[i]])
    
    # Binarize ratings
    if (rating[[i]] < 5){
      df[[cols]][[row]] = 0
    } else{
      df[[cols]][[row]] = 1
    }
  }
  return(df)
}