species_pts <- vect(
  species_df,
  geom = c("Longitude", "Latitude"),
  crs  = "EPSG:3035"
)
plot(species_pts)

# -- plot both on one figure --
# make semi‑transparent fills and outlines
cols1 <- adjustcolor("red",      alpha.f = 0.7)  # 70% opaque outline
bgs1  <- adjustcolor("pink",     alpha.f = 0.4)  # 40% opaque fill
cols2 <- adjustcolor("blue",     alpha.f = 0.7)
bgs2  <- adjustcolor("lightblue",alpha.f = 0.4)

plot(species_pts,
     pch = 21,
     col = cols1,
     bg  = bgs1,
     cex = 1.5,
     main = "Species Occurrences",
     axes = TRUE)

plot(bg_pts,
     pch = 22,
     col = cols2,
     bg  = bgs2,
     cex = 1.5,
     add = TRUE)
