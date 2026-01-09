#' Estimation of new daily precipitation values for a single day and gridpoint
#' 
#' @description This function uses the neighbouring observations to estimate new precipitation values in a single day.
#' @param can SpatVector. Candidate gridpoint
#' @param ref SpatVector. Observations
#' @param model_fun function. A function that integrates the statistical hybrid model (classification and regression)
#' @param thres numeric. Distance threshold to find neighbors.
#' @param neibs number of nearest neighbours that will be used to estimate new values
#' @param covars formula. Names of predictors.
#' @importFrom terra distance
#' @importFrom stats as.formula glm binomial predict
#' @noRd
#' 

predpoint <- function(can, ref, model_fun, thres, neibs, covars, coords, crs){
  #set nearest observations
  
  is_lonlat <- terra::is.lonlat(crs)
  
  # coordinates
  # can (candidate) comes from terra::as.data.frame(grid, xy=TRUE), so it has 'x' and 'y'
  can_coords <- as.matrix(can[, c("x", "y")])
  # ref (observations) has names defined in 'coords'
  ref_coords <- as.matrix(ref[, coords])
  
  dd <- as.vector(terra::distance(can_coords, ref_coords, lonlat = is_lonlat)/1000)
  
  # Create index vector to track original positions
  idx <- seq_along(dd)
  
  if(!is.na(thres)){ 
    mask <- dd < thres
    dd <- dd[mask]
    idx <- idx[mask]
  }
  
  if(length(dd) < neibs) {
    # Handle case where fewer than neibs are found (if thres is strict)
    # The original code just crashed or did something else?
    # Original: match(sort(dd)[1:neibs], dd). If length(dd) < neibs, sort(dd)[1:neibs] has NAs.
    # We should probably take what we have or stop.
    # For now, let's take min(length(dd), neibs)
    k <- min(length(dd), neibs)
    if(k == 0) {
       # No neighbors. Return 0,0 or NA?
       # Original code didn't handle this explicitly here (predday handles length(x)<neibs globally, but thres might filter all).
       return(c(0, 0))
    }
    ord <- order(dd)[1:k]
  } else {
    ord <- order(dd)[1:neibs]
  }
  
  final_idx <- idx[ord]
  ref <- ref[final_idx, , drop=FALSE]
  if (max(ref$val) == 0) {
    pred <- err <- 0
  } else if (sum(diff(ref$val))==0){
    pred <- ref$val[1]
    err <- 0
    } else{
      
      out <- model_fun(ref = ref, can = can, covars = covars)
      out <- round(out, 2)
      pb <- out[1]
      p <- out[2]
      e <- out[3]
    
    #evaluating estimate
    if(pb <= 0.5) pred <- 0 else pred <- p
    err <- e
  }
  return(c(pred, err))
}
