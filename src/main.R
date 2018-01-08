source('~/Documents/Temat14/src/utils/read.R')
source('~/Documents/Temat14/src/utils/write.R')
source('~/Documents/Temat14/src/utils/generate.R')

setwd('~/Documents/Temat14/src')
install.packages('recommender', repo=NULL, type='source')
library(recommender)

main.main <- function(){
  
  
  print('Begin')
  
  #import data from csv files
  source('~/Documents/Temat14/src/utils/read.R')
  source('~/Documents/Temat14/src/utils/write.R')
  source('~/Documents/Temat14/src/utils/generate.R')
  
  print("Load data")
  # Load rating data
  books_rating = read.read_csv(fl="~/Documents/BX-CSV-Dump 2/BX-Book-Ratings.csv", rowLen = 150)
  #books_rating = read.read_csv(fl="~/Documents/MOWproj/BX-Book-Ratings.csv", rowLen = 35)
  
  # get specified columns from imported data
  col_br_user = books_rating[[1]]
  col_br_isbn = books_rating[[2]]
  col_br_rating = books_rating[[3]]
  
  # Unique users and book's isbn
  unique_user_id = sort(unique(col_br_user))
  unique_book_id = sort(unique(col_br_isbn))
  
  print("Generate empty data frame")
  # Generating empty data frame
  df.item.holder = generate.generate_empty_df(unique_user_id, unique_book_id)
  
  print("Filling data frame")
  # Filling df.item.complete with ratings
  df.item.complete <- generate.generate_complete_df(df.item.holder, col_br_user, col_br_isbn, col_br_rating)
  
  # Writing results to a csv file
  # write_csv(fl="~/Documents/Mow_project/res.csv", df=df.item.complete)
  
  print("Filtering starts")
  
  ub_results <- ub_collaborative_filtering(df.item.complete, 5)
  ib_results <- ib_collaborative_filtering(df.item.complete, 5)

  # ----- Writing results to a csv file -----
  print("Filtering ends")
  
  print("Write results into .csv files")
  # Item-Based results
  write.write_csv(ib_results, fl="~/Documents/Mow_project/item.csv")
  # User-Based results
  write.write_csv(fl="~/Documents/Mow_project/user.csv", df=ub_results)
  
  print('End')
}

main.main()

