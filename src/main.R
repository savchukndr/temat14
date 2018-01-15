install.packages('recommender', repo=NULL, type='source')
library(recommender)

main.main <- function(){
  
  row_length = 300
  
  print('Begin')
  
  #import data from csv files
  source('utils/read.R')
  source('utils/write.R')
  source('utils/generate.R')
  
  print("Load data")
  # Load rating data
  books_rating = read.read_csv(fl="../BX-CSV-Dump 2/BX-Book-Ratings.csv", rowLen = row_length)
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
  
  print("Filtering starts")
  
  print("User-based algorythm starts:")
  print(system.time(ub_results <- ub_collaborative_filtering(df.item.complete, 10, 4)))
  print("User based algorythm ends, Item-based starts:")
  print(system.time(ib_results <- ib_collaborative_filtering(df.item.complete, 10, 4)))
  print("Item-based algotythm ends.")
  
  print(object.size(ub_results))
  print(object.size(ib_results))
  
  # ----- Writing results to a csv file -----
  print("Filtering ends")
  
  print("Write results into .csv files")
  # Item-Based results
  write.write_csv(ib_results, fl="../item.csv")
  # User-Based results
  write.write_csv(fl="../user.csv", df=ub_results)
  
  print('End')
}

main.main()

