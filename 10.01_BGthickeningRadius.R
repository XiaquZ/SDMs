# ---------------------------------------------------------------
# Compute background thickening radius (Vollering et al., 2019)
# ---------------------------------------------------------------
# Inputs
#   - internal: data.frame with columns "Longitude","Latitude" (training presences)
#   - covariates_pred: SpatRaster of predictors (names must match variable names)
#   - n_bg_uncorr: # of uniform background points for the uncorrected model
#   - crs_proj: projected CRS in meters for variograms/buffers (default: EPSG:3035)
#   - aggregate_fact: speed-up aggregation for template (does not affect final modeling)
#   - tune grid / partitions: passed to ENMevaluate to mirror your modeling
#   - fallback_km: radius used if variable selection/variograms fail
# Output
#   - list(radius_m, ranges_m, base_vars_used, crs_proj)

for (r in seq_along(occurrence.files)) {
  cat("Processing:", occurrence.names[r], "\n")
  
  species_df <- read.csv(occurrence.files[r])
  occs_coords <- species_df[, c("Longitude", "Latitude")]
  
  #### Split into internal (80%) and external (20%)
  set.seed(16)
  n_occs <- nrow(occs_coords)
  test_idx <- sample(seq_len(n_occs), size = ceiling(0.2 * n_occs))
  external <- occs_coords[test_idx, ]
  internal <- occs_coords[-test_idx, ]
}



    n_bg_uncorr   = 10000
    crs_proj      = "EPSG:3035"         # ETRS89 / LAEA Europe (meters)
    aggregate_fact = 10
    seed          = 16
    fc            = c("L","Q","P","LQ","QP","LP","LQP")
    rm            = c(0.5,1,2,3,4,5)
    partitions    = "block"
    partition.settings = list(orientation = "lat_lon")
    parallel      = TRUE
    numCores      = 10
    fallback_km   = 500

  # --- Packages
  require(sf)
  require(terra)
  require(gstat)
  require(ENMeval)
  
  require(dplyr)
  require(stringr)
  
  set.seed(seed)
  
  # --- Template for projection/sampling (speed-up only)
  tmpl_ll <- covariates_pred[[1]]
  tmpl_ll_agg <- terra::aggregate(tmpl_ll, fact = aggregate_fact)
  
  # --- Uniform background for the uncorrected model (in projected space)
  bg_uncorr_pts <- spatSample(
    x = tmpl_ll_agg, size = n_bg_uncorr,
    method = "random", as.points = TRUE, na.rm = TRUE
  )
  # back to the predictor CRS (lon/lat or whatever covariates are in)
  bg_uncorr_coords_ll <- as.data.frame(crds(terra::project(bg_uncorr_pts, crs(tmpl_ll))))
  colnames(bg_uncorr_coords_ll) <- c("Longitude","Latitude")
  
  # --- UNCORRECTED SWD: presences unchanged, background uniform
  occs_uncorr <- cbind(
    internal,
    terra::extract(covariates_pred, internal, ID = FALSE)
  ) %>% na.omit()
  
  bg_uncorr <- cbind(
    bg_uncorr_coords_ll,
    terra::extract(covariates_pred, bg_uncorr_coords_ll, ID = FALSE)
  ) %>% na.omit()
  
  occs_uncorr <- as.data.frame(occs_uncorr)
  bg_uncorr   <- as.data.frame(bg_uncorr)
  names(occs_uncorr) <- make.names(names(occs_uncorr))
  names(bg_uncorr)   <- make.names(names(bg_uncorr))
  
  # --- Fit uncorrected ENMeval (same settings as your main run)
  e_uncorr <- ENMevaluate(
    occs = occs_uncorr,
    bg   = bg_uncorr,
    algorithm  = "maxnet",
    partitions = partitions,
    partition.settings = partition.settings,
    tune.args = list(fc = fc, rm = rm),
    parallel = parallel,
    numCores = numCores
  )
  res_uncorr <- eval.results(e_uncorr)
  best_idx_uncorr <- which(res_uncorr$delta.AICc == min(res_uncorr$delta.AICc))
  best_uncorr <- e_uncorr@models[[best_idx_uncorr]]
  
  # Extract base predictor names from maxnet/R-formula-style beta names
  # Handles: plain vars, I(var^2), var1:var2 interactions
  get_vars_from_beta_names_formula <- function(beta_names, valid_names = NULL) {
    # 1) split interactions so "a:b" -> "a","b"
    tokens <- unlist(strsplit(beta_names, ":", fixed = TRUE), use.names = FALSE)
    tokens <- trimws(tokens)
    
    # 2) drop the I(...) wrapper if present
    tokens <- sub("^I\\((.*)\\)$", "\\1", tokens)
    
    # 3) remove polynomial exponent like "^2" (or any ^something at the end)
    tokens <- sub("\\^.*$", "", tokens)
    
    # 4) trim & drop empties
    tokens <- trimws(tokens)
    tokens <- tokens[nzchar(tokens)]
    
    # 5) if a valid set is provided, keep only those; otherwise return unique tokens
    if (!is.null(valid_names)) {
      tokens <- intersect(tokens, valid_names)
    }
    unique(tokens)
  }
  # Do this once, before extracting data for SWD:
  names(covariates_pred) <- make.names(names(covariates_pred), unique = TRUE)
  
  # Then:
  pred_names_ok <- names(covariates_pred)
  vars_used <- get_vars_from_beta_names_formula(beta_names, valid_names = pred_names_ok)
  
  
  # 1) Sample points once from your study-area polygon (in meters CRS already)
  set.seed(16)
  n_samp <- 10000  # adjust if needed; 20–50k works well
  pts <- terra::spatSample(tmpl_ll_agg, size = n_samp, method = "random", as.points = TRUE,na.rm = TRUE,exhaustive = TRUE)
  
  # Practical range from points, capped by empirical max distance
  # - Fits Sph/Exp with smart starts
  # - Computes "practical range" where gamma reaches 95% of (nugget + psill)
  # - Caps result at max(v_emp$dist) as recommended when no sill is evident
  get_vario_range_from_pts <- function(r, pts, target_bins = 30, frac_cutoff = 0.75,
                                       min_rows = 200, cressie = TRUE,
                                       detrend = FALSE) {
    vals <- terra::extract(r, pts, ID = FALSE)[,1]
    xy   <- terra::crds(pts)
    dat  <- data.frame(x = xy[,1], y = xy[,2], z = vals)
    dat  <- dat[is.finite(dat$z), ]
    if (nrow(dat) < min_rows) return(NA_real_)
    
    # empirical variogram
    rx <- diff(range(dat$x)); ry <- diff(range(dat$y))
    diag_max <- sqrt(rx^2 + ry^2)
    cutoff <- frac_cutoff * diag_max
    
    # (optional) detrend to remove broad gradients that blow up ranges
    form <- if (detrend) z ~ x + y else z ~ 1
    
    v_emp <- gstat::variogram(
      form, locations = ~ x + y, data = dat,
      cutoff = cutoff, width = cutoff / target_bins, cressie = cressie
    )
    maxdist <- max(v_emp$dist, na.rm = TRUE)
    
    # smart starts from empirical curve
    k_tail <- min(5, nrow(v_emp))
    nug0   <- max(min(v_emp$gamma[1], stats::quantile(v_emp$gamma, 0.1, na.rm = TRUE)), 0)
    sill0  <- max(mean(tail(v_emp$gamma, k_tail), na.rm = TRUE), nug0 + 1e-8)
    psill0 <- max(sill0 - nug0, 1e-8)
    
    # rough practical-range guess: 95% of sill crossing
    g <- v_emp$gamma
    idx95 <- which(g >= 0.95 * sill0)[1]
    h_prac <- if (!is.na(idx95)) v_emp$dist[idx95] else 0.75 * maxdist
    
    # candidates (Sph uses range ~ practical; Exp param ~ practical/3)
    cands <- list(
      gstat::vgm(psill = psill0, model = "Sph", range = h_prac,     nugget = nug0),
      gstat::vgm(psill = psill0, model = "Exp", range = h_prac / 3, nugget = nug0)
    )
    
    fits <- lapply(cands, function(m)
      try(suppressWarnings(gstat::fit.variogram(v_emp, model = m, warn.if.neg = FALSE)), silent = TRUE)
    )
    fits_ok <- Filter(function(f) !inherits(f, "try-error") && any(is.finite(f$range)), fits)
    
    # function to get PRACTICAL range from a fitted model via model curve
    practical_range <- function(fit, maxdist, level = 0.95) {
      # total sill level to reach (nugget + psill_total)
      nug <- fit$psill[fit$model == "Nug"]
      ps  <- sum(fit$psill[fit$model != "Nug"])
      target <- nug + level * ps
      line <- gstat::variogramLine(fit, maxdist = maxdist, n = 200)
      i <- which(line$gamma >= target)[1]
      if (is.na(i)) maxdist else line$dist[i]
    }
    
    if (length(fits_ok) == 0L) {
      # no fit: use empirical max distance
      return(maxdist)
    } else {
      sse <- sapply(fits_ok, function(f) attr(f, "SSErr"))
      fit_best <- fits_ok[[ which.min(sse) ]]
      pr <- practical_range(fit_best, maxdist, level = 0.95)
      # cap at empirical max distance
      return(min(pr, maxdist))
    }
  }
  
  
  #### Get the final ranges and radii.####
  ranges_m <- sapply(
    vars_used,
    function(v) get_vario_range_from_pts(
      covariates_pred[[v]], pts,
      detrend = TRUE, cressie = TRUE,
      frac_cutoff = 0.75, target_bins = 30
    )
  )
  
  ranges_m <- ranges_m[is.finite(ranges_m)]
  stopifnot(length(ranges_m) > 0)
  
  thickening_radius <- mean(ranges_m) # meters
  # 1210715 meters
  cat(sprintf("Thickening radius: %.1f km\n", thickening_radius/1000))
  
  # Buffer presences (everything already in a metric CRS)
  occ_sf <- st_as_sf(internal, coords = c("Longitude","Latitude"), crs = crs(covariates_pred))
  presence_buffers <- st_buffer(occ_sf, dist = thickening_radius)



##If we strictly follow the paper: 
get_vario_range_from_pts_paper <- function(r, pts, target_bins = 30,
                                           frac_cutoff = 0.9,  # near full domain
                                           min_rows = 200, cressie = FALSE) {
  vals <- terra::extract(r, pts, ID = FALSE)[,1]
  xy   <- terra::crds(pts)
  dat  <- data.frame(x = xy[,1], y = xy[,2], z = vals)
  dat  <- dat[is.finite(dat$z), ]
  if (nrow(dat) < min_rows) return(NA_real_)
  
  # Empirical variogram (no detrend, omnidirectional)
  rx <- diff(range(dat$x)); ry <- diff(range(dat$y))
  diag_max <- sqrt(rx^2 + ry^2)
  cutoff <- frac_cutoff * diag_max
  
  v_emp <- gstat::variogram(z ~ 1, locations = ~ x + y, data = dat,
                            cutoff = cutoff, width = cutoff / target_bins,
                            cressie = cressie)
  maxdist <- max(v_emp$dist, na.rm = TRUE)
  
  # Spherical only; simple, reasonable starts
  k_tail <- min(5, nrow(v_emp))
  nug0   <- max(0, min(v_emp$gamma[1], stats::quantile(v_emp$gamma, 0.1, na.rm = TRUE)))
  sill0  <- max(mean(tail(v_emp$gamma, k_tail), na.rm = TRUE), nug0 + 1e-8)
  psill0 <- max(sill0 - nug0, 1e-8)
  m0 <- gstat::vgm(psill = psill0, model = "Sph", range = cutoff/3, nugget = nug0)
  
  fit <- try(gstat::fit.variogram(v_emp, model = m0, warn.if.neg = FALSE), silent = TRUE)
  
  # Paper rule: if no sill (failed/absurd), use max variogram distance
  if (inherits(fit, "try-error") || !any(fit$model == "Sph")) return(maxdist)
  
  rng <- fit$range[fit$model == "Sph"][1]
  if (!is.finite(rng) || rng >= maxdist) maxdist else rng
}
# sample once over YOUR study/background area (Europe, in your case)
# pts <- terra::spatSample(Europe, size = 30000, method = "random", as.points = TRUE)

ranges_m <- sapply(vars_used, function(v)
  get_vario_range_from_pts_paper(covariates_pred[[v]], pts)
)
ranges_m <- ranges_m[is.finite(ranges_m)]

if (!length(ranges_m)) stop("No variogram ranges estimated; cannot compute thickening radius per paper.")
thickening_radius <- mean(ranges_m)  # meters

cat(sprintf("Thickening radius (mean): %.1f km\n", thickening_radius/1000))
# 2571.5 km