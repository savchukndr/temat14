# Loads a dataset from the file
read.read_csv = function(fl, rowLen){
  return(read.csv(file=fl,nrows=rowLen, sep=";"))
}