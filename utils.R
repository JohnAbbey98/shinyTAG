
# Assuming you have a function `getIonImage` that takes an m/z value and returns an image

# utility function for making matrix from flattened dataframe
.curateMatrix <- function(dataframe, channel, coords){
  Matrix_image = matrix(0, ncol = max(coords$y), nrow=max(coords$x)) # initialise matrix of correct shape
  x = dataframe[,channel] # isolate single channel
  Matrix_image[as.matrix(coords)] = x # input channel values into matrix
  return(Matrix_image)
}

# function for imaging channels
.imageChannel <- function(x, coords = NA, channel_number = 1, method = "targeted", quantile_lim = 0.99, interpolate = FALSE, axes = FALSE, colna = "black") {
  
  # adjust input dataframe according to whether targeted or untargeted
  if (method == "targeted"){
    df <- x$IntensityDF
    coords <- x$SpatialCoords
  } else if(method == "untargeted"){
    df <- x$Untargeted$UntargetedIntensity
    coords <- x$SpatialCoords
  } else{
    stop("Method must be either 'targeted' or 'untargeted'.")
  }

  # create matrix image
  matrix_image <- matrix(NA, ncol = max(coords$y), nrow = max(coords$x))
  x <- df[, channel_number]
  x_max <- quantile(x, probs = quantile_lim, na.rm = TRUE)
  x[x > x_max] <- x_max
  matrix_image[as.matrix(coords)] <- x
  matrix_image <- imager::as.cimg(matrix_image - min(matrix_image, na.rm = TRUE))
  matrix_image <- imager::add.color(matrix_image, simple = TRUE)
  
  # adjust colours according to whether targeted or untargeted image
  if (method == "targeted"){
    R(matrix_image) <- 0
    B(matrix_image) <- 0
  }else if (method == "untargeted"){
    B(matrix_image) <- 0
  }

  if(colna == "black"){
    bg <- 1
  }
  if(colna == "white"){
    bg <- 0
  }
  
  # adjust title according to whether targeted or untargeted image
  if (method == "targeted"){
    title <- colnames(df)[channel_number]
  } else if(method == "untargeted"){
    title <- "Untargeted peak"
  } 
  
  # plot the image
  img <- plot(matrix_image, main = title,
       interpolate = interpolate,
       xlim = c(1, max(coords$x)), ylim = c(max(coords$y), 1),
       axes = axes,
       col.na = rgb(0, 0, 0, bg)) # black background for NA
  
  return(img)
  
}




# function for plotting metapeaks
.createMetapeaksPlot <- function(processed, metapeaks) {
  
  # get metapeak data
  counts <- metapeaks$count_df
  smooth_counts <- metapeaks$count_smooth_df
  
  # get targeted metapeak locations
  names <- processed$CorrespondenceMatrix$marker
  targeted_metapeak_location <- processed$CorrespondenceMatrix$mz_location
  targeted_expected_location <- processed$CorrespondenceMatrix$expected_mz_location
  targeted_metapeaks <- data.frame(matrix(0, nrow = length(names), ncol = 3))
  colnames(targeted_metapeaks) <- c("name", "metapeak_mz", "expected_mz")
  targeted_metapeaks$name <- names
  targeted_metapeaks$metapeak_mz <- targeted_metapeak_location
  targeted_metapeaks$expected_mz <- targeted_expected_location
  
  # get untargeted metapeak locations
  untargeted_metapeak_location <- processed$Untargeted$UntargetedCorrespondence$mz_location
  untargeted_metapeak_location <- untargeted_metapeak_location[is.na(processed$Untargeted$UntargetedCorrespondence$marker)] # get only truly untargeted peaks
  
  untargeted_metapeaks <- data.frame(matrix(0, nrow = length(untargeted_metapeak_location), ncol = 1))
  colnames(untargeted_metapeaks) <- c("untargeted_mz")
  untargeted_metapeaks$untargeted_mz <- untargeted_metapeak_location
  
  # Plot
  mz_range <- c(800, 2000) 
  r <- ggplot() +
    theme_minimal() +
    ggtitle("Metapeaks") +
    geom_line(data = counts, aes(x = mz, y = count), color = 'black', alpha = 1) +
    geom_vline(data = targeted_metapeaks, aes(xintercept = expected_mz, text = name), color = 'firebrick2', size = 0.5) +
    geom_vline(data = untargeted_metapeaks, aes(xintercept = untargeted_mz), color = 'orange', linetype = 'dashed', size = 0.5) +
    geom_vline(data = targeted_metapeaks, aes(xintercept = metapeak_mz), color = 'springgreen3', linetype = 'dashed', size = 0.5) +
    geom_area(data = smooth_counts, aes(x = mz, y = count), fill = '#6062eb', alpha = 0.5) +
    coord_cartesian(xlim = mz_range) +
    ylab('Peak Counts')
  
  return(r)
}


