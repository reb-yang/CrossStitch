library(imager)
library(tidyverse)
library(tidymodels)
library(sp)
library(scales)
library(cowplot)
library(dmc)


# 1. Process Image function 
process_image <- function(image_file_name, k_list){
  ## process_image returns a list of 2 elements necessary for the next steps of selecting colors
  ## and creating final pattern
  ##
  ## Input:
  ## - image_file_name: an image path 
  ## - k_list: a vector of values for the number of clusters (colors) desired
  ## Output:
  ## - cluster_info: A list of 2 elements
  ##                1) a data frame containing the x,y coordinates of the original image and colors
  ##                2) a nested tibble containing the k-means clusterings, variance within clusters,
  ##                  color assignments and corresponding dmc colors for different values of k
  ## Notes:
  ## The k-means clusterings are calculated with nstart = 20 and max iterations = 30. If colors are too 
  ## similar or too many clusters are specified in k_list, the algorithm may fail to converge within 30
  ## iterations and produce warnings. 
  ##
  ## Example:
  ##   cluster_info <- process_image("HarryStyles.jpeg", c(2,4,6,8))

  
  if(!require(imager)) {
    stop("The imager packages must be installed. Run install.packages(\"imager\") and then try again.")
  }
  if(!require(dplyr)) {
    stop("The dplyr packages must be installed. Run install.packages(\"dplyr\") and then try again.")
  }
  if(!require(tidymodels)) {
    stop("The tidymodels packages must be installed. Run install.packages(\"tidymodels\") and then try again.")
  }
  if(!require(dmc)) {
    stop("The dmc packages must be installed. Run install.packages(\"dmc)\") and then try again.")
  }
  if(!require(tidyverse)) {
    stop("The tidyverse packages must be installed. Run install.packages(\"tidyverse\") and then try again.")
  }
  img <- imager::load.image(image_file_name) # load the image 
  tidy_dat <- as.data.frame(img, wide = "c") %>% rename(R = c.1, G = c.2, B = c.3) # rename colors to RGB
  colors_dat <- tidy_dat %>% select(R, G, B) # selecting colors 
  
  # function to get vector of dmc colors 
  get_dmc <- function(tidied){
    ## get_dmc returns a list of two vectors of the closest dmc color hex codes and names
    ## for each cluster
    ##
    ## Input:
    ## - tidied: a tibble containing the cluster color assignments and hex codes
    ## Output:
    ## - a list with 2 elements
    ##    1) a vector of the dmc thread color hex codes
    ##    2) a vector of the dmc thread color names
    
    ## Example:
    ##   cluster_info <- process_image("HarryStyles.jpeg", c(2,4,6,8))
    ##   cluster_colors <- cluster_info[[2]] %>% filter(k = 6) %>% pluck("tidied", 1)
    ##   get_dmc(cluster_colors)
    
    dmc_cols = vector()
    dmc_names = vector()
    for(i in 1:nrow(tidied)){
      dmc_cols[i] <- dmc(tidied$colours[i], visualize = FALSE)$hex
      dmc_names[i] <- paste0(
        dmc(tidied$colours[i], visualize = FALSE)$name, " (",
        dmc(tidied$colours[i], visualize = FALSE)$dmc, ")")
    }
    return(list(dmc_cols, dmc_names))
  }
  
  # getting kmeans clusters based on k values from 1:k_list for scree plot
  kclusts <- tibble(k = k_list) %>%
    mutate(
      kclust = map(k, ~kmeans(x = colors_dat, centers = .x, nstart = 20, iter.max = 30)), # kmeans for each k
      glanced = map(kclust, glance), # applying glance function to each k
      tidied = map(kclust, tidy), # saving tidied cluster summary to tided for each k
      tidied = map(tidied, ~{.x %>% mutate(colours = rgb(R, G, B))}), # hex codes for colors 
      tidied = map(tidied, ~{mutate(.x, dmc_cols = get_dmc(.x)[[1]])}), # hex codes for dmc thread color 
      tidied = map(tidied, ~{mutate(.x, dmc_names = get_dmc(.x)[[2]])}) # color names for dmc threads
      )
  return(list(tidy_dat, kclusts))
}


# 2. Scree plot 

scree_plot <- function(cluster_info){
  ## scree_plot(cluster_info) produces a scree plot of the clusters 
  ##
  ## Input:
  ## - cluster_info: A list of 2 elements
  ##                1) a data frame containing the x,y coordinates of the original image and colors
  ##                2) a nested tibble containing the kmeans clusterings and color assignments
  ##                   for several values of k
  ##
  ## Output:
  ## - scree plot with the total sum of squares within each cluster on the y axis and number of clusters on the x axis
  ## - number of clusters on x axis according to the k_list specified in process_image
  ##    
  ##
  ## Example:
  ##   cluster_info <- process_image(image_path, c(2,4,6,8))
  ##   scree_plot(cluster_info)
  
  plot <- cluster_info[[2]] %>% unnest(cols = c(glanced)) %>% # getting tot.withnss for each k
    ggplot(aes(k, tot.withinss)) + 
    geom_line() +
    geom_point() + 
    scale_x_continuous(breaks = seq(2, 12, 1)) + 
    labs(x = "Number of Clusters",
         y = "Total Within Group Sum of Squares")
   return(plot)
}


color_strips <- function(cluster_info){
  ## color_strips(cluster_info) produces color strips for each value of k to help visualize the similarity
  ##  and difference between clusters 
  ##
  ## Input:
  ## - cluster_info: A list of 2 elements
  ##                1) a data frame containing the x,y coordinates of the original image and colors
  ##                2) a nested tibble containing the kmeans clusterings and color assignments
  ##                   for several values of k
  ##
  ## Output:
  ## - color strips with the DMC thread colors corresponding to the clusters for each k
  ##
  ## Example:
  ##   cluster_info <- process_image("HarryStyles.jpeg", c(2,4,6,8))
  ##   color_strips(cluster_info)
  
  colors_info <- cluster_info[[2]]
  k_list <-  cluster_info[[2]]$k
  colors <- vector()
  x <- vector()
  y <- vector()
  for(i in 1:nrow(colors_info)){
    k_colors <- colors_info %>% pluck("tidied", i)
    colors <- c(k_colors$dmc_cols, rep(NA, max(k_list) - k_list[i]), colors)
    y <- c(y, rep(i, max(k_list)))
    x <- c(1:max(k_list), x)
  }
  color_dat <- tibble(x, y, colors) %>%
    mutate(x1 = x-1, x2 = x, y1 = y-1, y2 = y)
  n_col = max(k_list)
  color_dat %>% 
    ggplot() + 
    coord_fixed() + 
    geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              color="black", 
              fill = colors) +
    geom_label(aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, 
                   label=colors), 
               size=24/n_col) + 
  scale_fill_manual(values = color_dat$colors) + theme_void() 

}


# function to change resolution of image
# this function was supplied by Dan Simpson as part of Assignment 2 for STA314
change_resolution <- function(image_df, x_size)
{
  ## change_resolution(image_df, x_size) subsamples an image to produce
  ## a lower resolution image. Any non-coordinate columns in the data
  ## frame are summarized with their most common value in the larger
  ## grid cell.
  ##
  ## Input:
  ## - image_df: A data frame in wide format. The x-coordinate column MUST
  ##             be named 'x' and the y-coordinate column MUST be named 'y'.
  ##             Further columns have no naming restrictions.
  ## - x_size:   The number of cells in the x-direction. The number of cells
  ##             in the vertical direction will be computed to maintain the 
  ##             perspective. There is no guarantee that the exact number
  ##             of cells in the x-direction is x_size
  ##
  ## Output:
  ## - A data frame with the same column names as image_df, but with fewer 
  ##   entries that corresponds to the reduced resolution image.
  ##
  ## Example:
  ##   library(imager)
  ##   library(dplyr)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager') 
  ##   im <- load.image(fpath)
  ##   image_df <- as.data.frame(im,wide = "c") %>% rename(R = c.1, G = c.2, B = c.3) %>%
  ##            select(x,y,R,G,B)
  ##   agg_image <- change_resolution(image_df, 50)
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  if(!require(dplyr)) {
    stop("The dplyr packages must be installed. Run install.packages(\"dplyr\") and then try again.")
  }
  
  sp_dat <- image_df
  gridded(sp_dat) = ~x+y
  
  persp = (gridparameters(sp_dat)$cells.dim[2]/gridparameters(sp_dat)$cells.dim[1])
  y_size = floor(x_size*persp)
  orig_x_size = gridparameters(sp_dat)$cells.dim[1]
  orig_y_size = gridparameters(sp_dat)$cells.dim[2]
  
  x_res = ceiling(orig_x_size/x_size)
  y_res = ceiling(orig_y_size/y_size)
  
  gt = GridTopology(c(0.5,0.5), c(x_res, y_res),
                    c(floor(orig_x_size/x_res), floor(orig_y_size/y_res)))
  SG = SpatialGrid(gt)
  agg = aggregate(sp_dat, SG, function(x) names(which.max(table(x)))[1] )
  agg@grid@cellsize <- c(1,1)
  df <- agg %>% as.data.frame %>% rename(x = s1, y = s2)  %>% select(colnames(image_df))
  
  return(df)
  
}




make_pattern <- function(cluster_info, k, x_size, background_color = NULL, black_white = FALSE){
  ## make_pattern(cluster_info, k , x_size, background_color = NULL, black_white = FALSE) creates 
  ## a cross stitch pattern using k-means clustering.
  ## 
  ## Input:
  ## - cluster_info: A list of 2 elements
  ##                1) a data frame containing the x,y coordinates of the original image and colors
  ##                2) a nested tibble containing the kmeans clusterings and color assignments
  ##                   for several values of k
  ## - k: An integer specifying the number of colors desired in the pattern
  ##      The scree plot and color strips should help decided this - colors that are too similar are
  ##      advised against because they may not show up well 
  ## - x_size:   An integer specifying the stitches desired in the horizontal direction. The number of cells
  ##             in the vertical direction is proportionally determined by the change_resolution function
  ## - background_color: A character in the format of a hex code "#FFFFFF" of the background color that the 
  ##                    user does not wish to be included in the final pattern. The default setting is NULL.
  ## - black_white: A logical argument on whether or not the pattern should be in black in white. The default
  ##                The default is FALSE. 
  ## Output:
  ## - A graph of the pattern, with thread colors coded by shapes and colors (optional). The legend
  ## provides DMC thread names as well as id numbers. 
  ##
  ## Example:
  ##   cluster_info <- process_image(image_path, 5)
  ##   pattern <- make_pattern(cluster_info, 6, 40, background_color = "#FFFFFF", black_white = TRUE)


  
  if(!require(cowplot)) {
    stop("The cowplot packages must be installed. Run install.packages(\"cowplot\") and then try again.")
  }
  
  k_num <- k
  k_clust <- cluster_info[[2]] %>% 
    filter(k == k_num) %>% pluck("kclust", 1)
  
  cluster_colors <- cluster_info[[2]] %>% 
    filter(k == k_num) %>% pluck("tidied", 1)
  
  img <- cluster_info[[1]]
  if(!is.null(background_color)){
    cluster_colors <- cluster_colors %>% filter(dmc_cols != background_color)
  }
  
  clustered_img <- augment(k_clust, img) %>%
    rename(cluster = .cluster)
  clustered_img <- inner_join(clustered_img, cluster_colors, by = "cluster")
  lower_res_img <- change_resolution(clustered_img, x_size)

  if(black_white){
    pattern <- ggplot(lower_res_img, aes(x=x, y=y)) + 
      geom_point(aes(shape = cluster)) 
    }
  else{
    pattern <- ggplot(lower_res_img, aes(x=x, y=y)) + 
      geom_point(aes(shape = cluster, color = cluster))  + 
      scale_color_manual(values = cluster_colors$dmc_cols, 
                         name = "DMC Thread Color", 
                         labels = cluster_colors$dmc_names) 
  }
  
  pattern <- pattern  + 
    scale_shape_manual(values = c(0:(k-1), size = 1), 
                       name = "DMC Thread Color", 
                       labels = cluster_colors$dmc_names) + 
    scale_y_reverse(minor_breaks = seq(1, x_size, 1)) + 
    theme_void() + 
    scale_x_continuous(minor_breaks = seq(1, x_size, 1)) + 
    background_grid(major = "xy", 
                    minor = "xy", 
                    size.major = 0.5, 
                    color.major = "black",
                    size.minor = 0.5) +
    theme(legend.position = "bottom",
          legend.direction = "vertical"
          ) +
    coord_fixed()
  return(pattern)
}



