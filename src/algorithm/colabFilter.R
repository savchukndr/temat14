source('~/Documents/Temat14/src/utils/help.r')

colabFilter.colaborative_filtering = function(df){
  
  # Start counting function time execution
  ptm = proc.time()
  
  # ---- Item-Based part ----
  
  # Drop any column named "user_id"
  df.ibs = (df[,!(names(df) %in% c("user_id"))])
  
  # Create a placeholder dataframe listing item vs.item
  df.ibs.similarity  = matrix(NA, nrow=ncol(df.ibs),ncol=ncol(df.ibs),dimnames=list(colnames(df.ibs),colnames(df.ibs)))

  # Loop through the columns
  for(i in 1:ncol(df.ibs)) {
    # Loop through the columns for each column
    for(j in 1:ncol(df.ibs)) {
      # Fill in placeholder with cosine similarities
      df.ibs.similarity[i,j] = help.get_cosine(as.matrix(df.ibs[i]),as.matrix(df.ibs[j]))
    }
  }
  
  # Back to dataframe
  df.ibs.similarity = as.data.frame(df.ibs.similarity)
  
  # Get the top 10 neighbours for each
  df.neighbours = matrix(NA, nrow=ncol(df.ibs.similarity),ncol=11,dimnames=list(colnames(df.ibs.similarity)))
  
  for(i in 1:ncol(df.ibs)) 
  {
    df.neighbours[i,] = (t(head(n=11,rownames(df.ibs.similarity[order(df.ibs.similarity[,i],decreasing=TRUE),][i]))))
  }
  
  # ---- User-Based part ----
  
  # A placeholder matrix
  holder = matrix(NA, nrow=nrow(df),ncol=ncol(df)-1,dimnames=list((df$user_id),colnames(df[-1])))
  
  # Loop through the users (rows)
  for(i in 1:nrow(holder)) 
  {
    # Loops through the products (columns)
    for(j in 1:ncol(holder)) 
    {
      # Get the user's name and th product's name
      user = rownames(holder)[i]
      product = colnames(holder)[j]
      
      # Remove already consumed products
      if(as.integer(df[df$user_id==user,product]) == 1)
      { 
        holder[i,j]=""
      } else {
        
        # Top 10 neighbours sorted by similarity
        topN=((head(n=11,(df.ibs.similarity[order(df.ibs.similarity[,product],decreasing=TRUE),][product]))))
        topN.names = as.character(rownames(topN))
        topN.similarities = as.numeric(topN[,1])
        
        # Drop first because it will be the same product
        topN.similarities=topN.similarities[-1]
        topN.names=topN.names[-1]
        
        # Get user rating history for those 10 items
        topN.purchases= df[,c("user_id",topN.names)]
        topN.userPurchases=topN.purchases[topN.purchases$user_id==user,]
        topN.userPurchases = as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("user_id"))])
        
        # Calculate the score for that product and that user
        holder[i,j]=help.get_score(similarities=topN.similarities,history=topN.userPurchases)
        
      }
    }   
  } 
  
  df.user.scores = holder
  
  # Filling recommendation with names
  df.user.scores.holder = matrix(NA, nrow=nrow(df.user.scores),ncol=100,dimnames=list(rownames(df.user.scores)))
  for(i in 1:nrow(df.user.scores)) 
  {
    df.user.scores.holder[i,] = names(head(n=100,(df.user.scores[,order(df.user.scores[i,],decreasing=TRUE)])[i,]))
  }
  
  # End counting function time execution
  time_res = proc.time() - ptm
  
  # Returns list with three items (item_based results, user_based results, time execution)
  return(list(df.neighbours, df.user.scores.holder, time_res))
}