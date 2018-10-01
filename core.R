# Ubuntu install libtiff5-dev
#install.packages("tiff")
require(tiff)

# Custom f(x)
# for filling zeros (destFile auto labeling)
fill_zeros <- function(n, max_digs = 4){
  n1 <- as.character(n)
  nl <- nchar(n1)
  if (nl < max_digs) {
    n1 <- paste(paste(rep("0", (max_digs - nl)), sep = "", collapse = ""),
                n1, sep = "", collapse = "")
  }
  return(n1)
}

# Core function
# Explode a TIFF file into multiple sub-images
clip_tiff <- function(tiff_file, px=300, prefix="sub_pic_", show = TRUE) {
  my_pic <- tiff::readTIFF(tiff_file, convert = TRUE)
  
  #my_pic_mat <- matrix(my_pic[1:(759*715)], ncol = 715, nrow=759)
  #my_pic_mat[1:5,1:5]
  if(show) {
    tryCatch({
      #XX <- matrix(my_pic[1:(dim(my_pic)[1]*dim(my_pic)[2])], 
      #             ncol = dim(my_pic)[2], nrow=dim(my_pic)[1])
      #plot(as.raster(XX))
      #
      #image(t(XX[nrow(XX):1, ]), 
      #      col = colorRampPalette(c("black", "white"))(100),
      #      main = "Full TIFF preview", axes = FALSE)
      
      plot(as.raster(my_pic))
      title("Full TIFF preview")
      
    }, error = function(e) NULL)

  }
  # dim(my_pic)
  # length(my_pic)
  # 11264 * 8192 * 3
  
  # Get original dimensions
  num_layers <- ifelse(is.na(dim(my_pic)[3]), 1, dim(my_pic)[3])
  dim_1 <- dim(my_pic)[1]
  dim_2 <- dim(my_pic)[2]
  
  # Define edge_len
  edge_len <- px
  
  # Based on edge, define seqs
  dim_1_seq <- seq(1, dim_1, by = edge_len)
  dim_1_seq <- dim_1_seq[-length(dim_1_seq)]
  dim_2_seq <- seq(1, dim_2, by = edge_len)
  dim_2_seq <- dim_2_seq[-length(dim_2_seq)]
  
  zeros_to_add <- nchar(as.character(as.integer(length(dim_1_seq) * length(dim_2_seq))*10))
  # Initialize counter
  cnt <- 1
  
  # Proceed with sub-sampling
  for (nn1 in dim_1_seq) {
    for (nn2 in dim_2_seq) {
      d1_i <- nn1
      d1_e <- nn1 + edge_len - 1
      d2_i <- nn2
      d2_e <- nn2 + edge_len - 1
      mult_x <- dim_1 * dim_2
      
      # Debugging lines
      #image(my_pic_mat)
      #image(my_pic_mat[1:8,1:8])
      #my_pic_mat[1:8,1:8]
      
      sub_pic <- do.call(c, lapply(1:num_layers, function(jl) {
        out <- do.call(rbind, lapply(seq(d1_i, d1_e), function(jj) {
          z_i <-  (mult_x * (jl - 1)) + (dim_1 * (jj - 1)) + d2_i
          z_e <-  (mult_x * (jl - 1)) + (dim_1 * (jj - 1)) + d2_e
          my_pic[z_i:z_e]
        }))
        # Debugging lines
        #image(t(out))
        t(out)
      }))
      
      file_nm <- paste(prefix, fill_zeros(cnt, zeros_to_add), ".tiff", sep = "")
      sub_pic <- array(data = sub_pic,
                       dim = c((d1_e - d1_i + 1), (d2_e - d2_i + 1), num_layers),
                       dimnames = NULL)
      
      # Visualize result
      if (show){
        tryCatch({
          plot(as.raster(sub_pic))
          title(file_nm)
        }, error = function(e) NULL)
      }
      
      # Write TIFF
      tiff::writeTIFF(sub_pic, where = file_nm, compression = "none")
      cnt <- cnt + 1
      sub_pic <- NULL
    } 
  }
  my_pic <- NULL

  message("Done!")
  return(NULL)
}
