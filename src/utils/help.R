# Create a helper function to calculate the cosine between two vectors
help.get_cosine = function(x,y) 
{
  this.cosine = sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

# A helper function to calculate the scores
help.get_score = function(history, similarities)
{
  x = sum(history*similarities)/sum(similarities)
  x
}