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

#' @export
ub_collaborative_filtering <- function(df, n_recommendations = 10, n_neighbours = 5)
{
  df.ubs = (df[,!(names(df) %in% c("user_id"))])
  rownames(df.ubs) <- df$user_id

  # Create a placeholder dataframe listing user vs.user
  df.ubs.similarity  = matrix(0L, nrow=nrow(df.ubs),ncol=nrow(df.ubs),dimnames=list(rownames(df.ubs),rownames(df.ubs)))

  # Loop through the rows
  for(i in 1:nrow(df.ubs)) {
    # Loop through the row for each row
    for(j in 1:nrow(df.ubs)) {
      # Fill in placeholder with cosine similarities
      if (i != j)
      {
        sim = help.get_cosine(as.matrix(df.ubs[i]),as.matrix(df.ubs[j]))
        df.ubs.similarity[i,j] <- sim
      }
    }
  }

  # Back to dataframe
  df.ubs.similarity = as.data.frame(df.ubs.similarity)

  # Get the top neighbours for each user
  df.neighbours = matrix(NA, nrow=nrow(df.ubs.similarity),ncol=n_neighbours,dimnames=list(rownames(df.ubs.similarity)))
  neighbours_similarities = matrix(NA, nrow=ncol(df.ubs.similarity),ncol=n_neighbours,dimnames=list(rownames(df.ubs.similarity)))

  for(i in 1:nrow(df.ubs))
  {
    df.neighbours[i,] = (t(head(n=n_neighbours,rownames(df.ubs.similarity[order(df.ubs.similarity[,i],decreasing=TRUE),][i]))))
    neighbours_similarities[i,] = (t(head(n=n_neighbours,df.ubs.similarity[order(df.ubs.similarity[,i],decreasing=TRUE),][i])))
  }

  # Matrix of predicted ratings
  predictions = matrix(0L, nrow=nrow(df.ubs), ncol=ncol(df.ubs))

  rownames(predictions) <-df[["user_id"]]
  colnames(predictions) <- colnames(df.ubs)
  for(i in 1:nrow(predictions))
  {
    for(j in 1:ncol(predictions))
    {
      if (df.ubs[i, j] == 0)
      {
        rows <- rownames(predictions) %in% df.neighbours[i,]
        m <- mean(df.ubs[rows, j][df.ubs[rows, j] != 0])
        if (!is.nan(m))
        {
          predictions[i, j] <- m

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

#'@export
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

  # Get the top neighbours for each item
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
      if (df.ibs[i, j] == 0)
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
