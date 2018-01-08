##' Recommendation functions.
##'
##' @name recommendation-functions

# Create a helper function to calculate the cosine between two vectors
help.get_cosine = function(x,y)
{
  this.cosine = sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  if (!is.nan(this.cosine))
  {
    return(this.cosine)
  }
  else
  {
    return(0)
  }
}

# A helper function to calculate the scores
help.get_score = function(history, similarities)
{
  x = sum(history*similarities)/sum(similarities)
  x
}

# TODO
#' @export
ub_collaborative_filtering <- function(df, n_recommendations = 10)
{
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
  df.user.scores.holder = matrix(NA, nrow=nrow(df.user.scores),ncol=n_recommendations,dimnames=list(rownames(df.user.scores)))
  for(i in 1:nrow(df.user.scores))
  {
    df.user.scores.holder[i,] = names(head(n=n_recommendations,(df.user.scores[,order(df.user.scores[i,],decreasing=TRUE)])[i,]))
  }
  return(df.user.scores.holder)
}

#' @export
ib_collaborative_filtering <- function(df, n_recommendations = 10, n_neighbours = 5)
{
  df.ibs = (df[,!(names(df) %in% c("user_id"))])

  # Create a placeholder dataframe listing item vs.item
  df.ibs.similarity  = matrix(0L, nrow=ncol(df.ibs),ncol=ncol(df.ibs),dimnames=list(colnames(df.ibs),colnames(df.ibs)))

  # Loop through the columns
  for(i in 1:ncol(df.ibs)) {
    # Loop through the columns for each column
    for(j in 1:ncol(df.ibs)) {
      # Fill in placeholder with cosine similarities
      if (i != j)
      {
        sim = help.get_cosine(as.matrix(df.ibs[i]),as.matrix(df.ibs[j]))
        df.ibs.similarity[i,j] <- sim
      }
    }
  }

  # Back to dataframe
  df.ibs.similarity = as.data.frame(df.ibs.similarity)

  # Get the top neighbours for each
  df.neighbours = matrix(NA, nrow=ncol(df.ibs.similarity),ncol=n_neighbours,dimnames=list(colnames(df.ibs.similarity)))
  neighbours_similarities = matrix(NA, nrow=ncol(df.ibs.similarity),ncol=n_neighbours,dimnames=list(colnames(df.ibs.similarity)))

  for(i in 1:ncol(df.ibs))
  {
    df.neighbours[i,] = (t(head(n=n_neighbours,rownames(df.ibs.similarity[order(df.ibs.similarity[,i],decreasing=TRUE),][i]))))
    neighbours_similarities[i,] = (t(head(n=n_neighbours,df.ibs.similarity[order(df.ibs.similarity[,i],decreasing=TRUE),][i])))
  }

  # Matrix of predicted ratings
  predictions = matrix(0L, nrow=nrow(df.ibs), ncol=ncol(df.ibs))

  rownames(predictions) <-df[["user_id"]]
  colnames(predictions) <- colnames(df.ibs)
  for(i in 1:nrow(predictions))
  {
    for(j in 1:ncol(predictions))
    {
      if (df.ibs[i, j] != 0)
      {
        cols <- colnames(predictions) %in% df.neighbours[j,]
        items <- df.ibs[i, cols]
        similarities <- neighbours_similarities[j,]
        if (sum(similarities) > 0)
        {
          prediction <- (sum(items * similarities) / sum(similarities))
          predictions[i, j] <- prediction
        }
      }
    }
  }

  recommendations = matrix(NA, nrow=nrow(predictions), ncol=n_recommendations)
  rownames(recommendations) <- rownames(predictions)
  for(i in 1:nrow(recommendations))
  {
    cols <- head(n = n_recommendations, order(predictions[i,], decreasing = TRUE))
    recommendations[i,] <- colnames(predictions[,cols])
  }
  return(recommendations)
}
