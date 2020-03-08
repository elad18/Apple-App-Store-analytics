# This file will extract aditional features for clusetring and perform a k-means alanysis.

pacman::p_load(c("sentimentr","readr","cluster","e1071","purrr","rgl","jpeg","mltools","data.table",
                 "dplyr","ggplot2","Rtsne","BBmisc","rpart","caTools","rpart.plot","randomForest",
                 "stringr","pROC","caret", "formattable"),install=TRUE,character.only=TRUE)

library(BBmisc)
cat("\014")
set.seed(1234)


library(formattable)

# Change working Directory Automaticaly
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)



LOAD_IMAGES <- "../Data/AppIcons/"
CSV_WRITE_DIRECTORY <- "../Data/"

DATAFILE <- "../Data/mobileAppData.csv"

apps <- read.csv(DATAFILE)


# Print first five indexes
print(head(apps$Icon.URL))

# Print size of the column
print(nrow(data))


##################### Function to retreive images and obtain RGB Values #############################################
# INPUT ARGUMENTS: Dataset, directory                                                                               #
#                                                                                                                   #
# RETURNS: Input dataset vertically concatenated with RGB channel features                                          #
#                                                                                                                   #
#####################################################################################################################


image_get <- function(final){
  app_id <- final$ID
  color_matrix = matrix(0,length(app_id),3)
  empty_idx = matrix(0,1,nrow(final))
  
  count_fails<-0
  i <- 0
  
  for(id in app_id){
    i <- i + 1
    img <- readJPEG((paste0(LOAD_IMAGES,id,'.jpg')), native = FALSE)
    img_r <- try(img[,,1])
    if(is(img_r,"try-error")){
      count_fails <- count_fails + 1
      empty_idx[count_fails] <- i
      cat("Failed to convert image", i, "\n")

    }
    
    else{
      
      img_r <- img[,,1]
      img_g <- img[,,2]
      img_b <- img[,,3]
      
      r_flat <- as.vector(img_r)
      g_flat <- as.vector(img_g)
      b_flat <- as.vector(img_b)
      
      mean_r <- mean(r_flat)
      mean_g <- mean(g_flat)
      mean_b <- mean(b_flat)
      
      color_matrix[i,1] <- mean_r
      color_matrix[i,2] <- mean_g
      color_matrix[i,3] <- mean_b
      cat("Image", i, "has been successfully converted out of", length(app_id),"\n")
    }
  }
  print(paste0('Failed to read: ',count_fails, ' values'))
  
  # Now we can remove the indexes of the non-successful game icons
  
  final <- final[-empty_idx,]
  color_matrix <- color_matrix[-empty_idx,]
  
  colnames(color_matrix) <- c("r","g","b")
  
  return (cbind(final,color_matrix))
  
}

##################### Function to retreive word count and language count ############################################
# INPUT ARGUMENTS: Dataset                                                                                          #
#                                                                                                                   #
# RETURNS: Input dataset vertically concatenated with number of words in a description and the number of languages  #
#                                                                                                                   #
#####################################################################################################################


getwordcount <- function(data){
  
  apps_descript <- data$Description
  
  word_count = matrix(0,length(apps_descript),1)
  language_count = matrix(0,length(apps_descript),1)
  
  fail_capture <- 0
  empty_idx = matrix(0,1,length(apps_descript))
  
  
  i <- 0
  
  for (language in data$Languages){
    
    i <- i + 1
    
    num_languages <- length(unlist(strsplit(toString(language), "\\,")))
    
    if(is(num_languages,"try-error")){
      
      fail_capture <- fail_capture + 1
      
      empty_idx[fail_capture] = 1
      print("Field is currently empty")
    }
    
    else{
      language_count[i] = num_languages
    }
  }
  
  apps_descript <- data$Description
  word_count = matrix(0,length(apps_descript),1)
  
  i <- 0 
  for (descript in apps_descript){
    i <- i + 1
    count <- sapply(strsplit(descript, " "), length)
    word_count[i] <- count
  }
  
  
  data <- cbind(data,word_count,language_count)
  
  return(data)
}


##################### Function to obtain sentiment score of each app ################################################
# INPUT ARGUMENTS: Dataset                                                                                          #
#                                                                                                                   #
# RETURNS: Input dataset vertically concatenated with sentiment score of each app                                   #
#                                                                                                                   #
#####################################################################################################################

getsentiment <- function(data){
  
  AppDescriptions<-data$Description
  AppDescriptionsS<-as.character(AppDescriptions)
  
  sentences<-sentimentr::get_sentences(AppDescriptionsS, as_vector = TRUE)
  sentimentAnal<-sentimentr::sentiment_by(sentences)
  sentiment_score <- sentimentAnal$ave_sentiment
  names(sentiment_score) <- "sentiment"
  return(cbind(data,sentiment_score))
}

get_genre_count <- function(data){
  apps_descript <- data$Description
  
  genre_count= matrix(0,length(apps_descript),1)
  language_count = matrix(0,length(apps_descript),1)
  empty_idx = matrix(0,1,length(apps_descript))
  
  
  i <- 0
  
  for (genre in data$Genres){
    
    i <- i + 1
    print(genre)
    
    num_genres <- length(unlist(strsplit(genre, "\\,")))
    
    print(num_genres)
    genre_count[i] = num_genres
  }
  
  return(cbind(data,genre_count))
  
}


##################### Function to run and evaluate K-means ##########################################################
# INPUT ARGUMENTS: Dataset                                                                                          #
#                                                                                                                   #
# RETURNS: Input dataset vertically concatenated with corresponding cluster indices                                 #
#                                                                                                                   #
#####################################################################################################################

k_means_clustering <- function(data){
  
  
  x <- data$User.Rating.Count
  skew_time_diff <- skewness(x)
  skew_time_diff_new <- skewness(x^(1/3))
  print(skew_time_diff_new)
  x <- x^0.5
  x <- BBmisc::normalize(x, method = "standardize", range = c(0, 1)) # Overcome masking issues
  
  y <- data$Size
  skew_usercount <- skewness(y)
  skew_usercount <- skewness(log10(y)^(1/2))
  y <- y
  y <- BBmisc::normalize(y, method = "standardize", range = c(0, 1))
  
  plot(x = x, y = y, xlab = "User Review Count", ylab = "Memory Size (Bytes)", main = "Scatter plot of User review count vs. Memory Size(Bytes)",)
  
  ########
  #while(TRUE){
  user.input <- readline(prompt="Enter the number of anomalous datasets you would like to consider: ")

    #if(print(all.equal(test, as.integer(test))) & user.input > 0){
      #break
    #}
    #else{
     # print("Try a positive integer")
   # }
 # }
  
  anomalies <- order(x, decreasing=TRUE)[1:user.input ]
  
  df <- cbind(y,x)
  df <- df[-anomalies,]
  data <- data[-anomalies,]
  
  avg_sil <- function(k) {
    km.res <- kmeans(df, centers = k, iter.max = 50)
    ss <- silhouette(km.res$cluster, dist(df))
    mean(ss[, 3])
  }
  
  # Compute and plot wss for k = 2 to k = 15
  k.values <- 2:15
  
  # extract avg silhouette for 2-15 clusters
  avg_sil_values <- map_dbl(k.values, avg_sil)
  plot(k.values, avg_sil_values,
       type = "b", pch = 19, frame = FALSE,
       xlab = "Number of clusters K",
       ylab = "Average Silhouettes")
  
  # Add one to account for k starting at 2
  final <- kmeans(df, centers = 3,iter.max = 50 )
  plot(x = df[,1], y = df[,2], col = final$cluster, xlab = "User Review Count", ylab = "Memory Size (Bytes)", main = "K-Means Clustering, K=3",)
  legend("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3"), pch = 1 , col = c("red","black","green"))
  return(cbind(data,final$cluster))
}

##################### Function to run and evaluate K-means ##########################################################
# INPUT ARGUMENTS: Dataset                                                                                          #
#                                                                                                                   #
# RETURNS: Input dataset vertically concatenated with corresponding cluster indices                                 #
#                                                                                                                   #
#####################################################################################################################

clust_rating <- function(data){
  
  
  cluster_idx <- data[,26] # Column index of cluster indices
  
  mobileAppData <- data[,1:25] # Rest of dataset
  

  
  cluster_1_data <- mobileAppData[which(cluster_idx == 1),]
  cluster_2_data <- mobileAppData[which(cluster_idx == 2),]
  cluster_3_data  <- mobileAppData[which(cluster_idx == 3),]
  
  
  cluster_1_avgrate <- mean(cluster_1_data$Average.User.Rating)
  cluster_2_avgrate <- mean(cluster_2_data$Average.User.Rating)
  cluster_3_avgrate <- mean(cluster_3_data$Average.User.Rating)
  user_review_count <- c(cluster_1_avgrate,cluster_2_avgrate,cluster_3_avgrate)
  
  
  cluster_1_count <- mean(cluster_1_data$User.Rating.Count)
  cluster_2_count <- mean(cluster_2_data$User.Rating.Count)
  cluster_3_count <- mean(cluster_3_data$User.Rating.Count)
  avg_rating <- c(cluster_1_count,cluster_2_count,cluster_3_count)
  
  
  
  cluster_1_r <- mean(cluster_1_data[,19])
  cluster_2_r <- mean(cluster_2_data[,19])
  cluster_3_r <- mean(cluster_3_data[,19])
  red <- c(cluster_1_r,cluster_2_r,cluster_3_r)
  
  
  cluster_1_g <- mean(cluster_1_data[,20])
  cluster_2_g <- mean(cluster_2_data[,20])
  cluster_3_g <- mean(cluster_3_data[,20])
  green <- c(cluster_1_g,cluster_2_g,cluster_3_g)
  
  
  cluster_1_b <- mean(cluster_1_data[,21])
  cluster_2_b <- mean(cluster_2_data[,21])
  cluster_3_b <- mean(cluster_3_data[,21])
  blue <- c(cluster_1_b,cluster_2_b,cluster_3_b)
  
  
  cluster_1_size <- mean(cluster_1_data$Size)
  cluster_2_size <- mean(cluster_2_data$Size)
  cluster_3_size <- mean(cluster_3_data$Size)
  size_MB <- c(cluster_1_size,cluster_2_size,cluster_3_size)
  
  cluster_1_sentiment <- mean(cluster_1_data[,25]) 
  cluster_2_sentiment <- mean(cluster_2_data[,25]) 
  cluster_3_sentiment <- mean(cluster_3_data[,25]) 
  sentiment <- c(cluster_1_sentiment,cluster_2_sentiment,cluster_3_sentiment)
  
  
  cluster_1_lang_count <- mean(cluster_1_data$language_count)
  cluster_2_lang_count <- mean(cluster_2_data$language_count)
  cluster_3_lang_count <- mean(cluster_3_data$language_count)
  lang_count <- c(cluster_1_lang_count,cluster_2_lang_count,cluster_3_lang_count)
  
  
  cluster_1_genre_count <- mean(cluster_1_data$genre_count)
  cluster_2_genre_count <- mean(cluster_2_data$genre_count)
  cluster_3_genre_count <- mean(cluster_3_data$genre_count)
  genre_count <- c(cluster_1_genre_count,cluster_2_genre_count,cluster_3_genre_count)
  
  
  cluster_1_word_count <- mean(cluster_1_data$word_count)
  cluster_2_word_count <- mean(cluster_2_data$word_count)
  cluster_3_word_count <- mean(cluster_3_data$word_count)
  word_count <- c(cluster_1_word_count,cluster_2_word_count,cluster_3_word_count)
  
  cluster_1_price <- mean(cluster_1_data$Price)
  cluster_2_price <- mean(cluster_2_data$Price)
  cluster_3_price <- mean(cluster_3_data$Price)
  price <- c(cluster_1_price,cluster_2_price,cluster_3_price)

  cluster.data <- data.frame(avg_rating,user_review_count,price,red,green,blue,size_MB,word_count,sentiment,lang_count,genre_count)
  row.names(cluster.data) <- c("Cluster 1","Cluster 2", "Cluster 3")
  print(formattable(cluster.data, align = c("c", rep("r", NCOL(cluster.data) - 1))))
  
}


# Our main function
main <- function(){
  
  # Remove NaN values
  apps <- apps[!(is.na(apps$Average.User.Rating)) | !(is.na(apps$User.Rating.Count)),]
  
  # Remove duplicate values
  apps <- apps[!duplicated(apps),]
  
  # Obtain pixel value features of images
  data_processed <- image_get(apps)
  
  # Add word count feature
  data_processed <- getwordcount(data_processed)
  
  data_processed <- get_genre_count(data_processed)
  
  data_processed <- getsentiment(data_processed)
  
  data_clustered <- k_means_clustering(data_processed)
  
  clust_rating(data = data_clustered)

  # Write data to a CSV file
  write.csv(data_clustered, "../Data/data_complete.csv" ,row.names = FALSE)
  
}

# Make a call to main
main()


