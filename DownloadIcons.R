# This file downloads app icons that will to be used for clustering


# Change working Directory Automaticaly
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

SAVE_IMAGES <- "../Data/AppIcons/~.jpg"

if (!dir.exists("../Data/AppIcons")){
  dir.create("../Data/AppIcons")
}

# Import Data
data <- read.csv("../Data/mobileAppData.csv")

# Remove NaN values
data <- data[!(is.na(data$Average.User.Rating)) | !(is.na(data$User.Rating.Count)),]

# Remove duplicate values
data <- data[!duplicated(data),]

print("extracting images....")
i <- 0
fail_capture <- 0
# We don't want any repeated samples when downloading the images
data_thumb <- data$Icon.URL 

# Make a note of empty indexes and remove them ()
# so we create a vector of zeros with one row and n_samples long
empty_idx = matrix(0,1,nrow(data))

# Loop over data to extract images
for(url in data_thumb){
  i <- i + 1
  istr <- toString(data$ID[i])
  url <- toString(url)
  
  # Try to download the files
  img_try <- try(download.file(url = url, destfile = gsub('~',istr, SAVE_IMAGES), mode='wb'))
  
  # Catch any files which can't be downloaded
  if(is(img_try,"try-error")){
    print(paste0("404 Errror when trying to capture URL: ",url))
    
    fail_capture <- fail_capture + 1
    
    # Note the index of the non-existent files
    empty_idx[fail_capture] = i
  } 
  else{
    print(paste0("Successfully Downloaded: ", url))
  }
  
  cat(i, "/", length(data_thumb), "\n")
  
}
print(fail_capture)

data <- data[-empty_idx,]
