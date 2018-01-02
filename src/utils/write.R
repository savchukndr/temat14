# Write results into csv file
write.write_csv = function(fl, df){
  write.table(df, file=fl, sep=";")
}