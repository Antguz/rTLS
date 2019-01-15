cube <- function(x=0,y=0,z=0, bordered = TRUE,
                 filled = TRUE, lwd = 2, scale = 1,
                 fillcol = gray(.95),
                 bordercol ='black', ...) {

  mycube <- cube3d()


  # Reduce size to unit
  mycube$vb[4,] <- mycube$vb[4,]/scale*2

  for (i in 1:length(x)) {
    # Add cube border
    if (bordered) {
      bcube <- mycube
      bcube$material$lwd <- lwd
      bcube$material$front <- 'line'
      bcube$material$back <- 'line'
      bcube %>% translate3d(x[i], y[i], z[i]) %>% shade3d
    }
    # Add cube fill
    if (filled) {
      fcube <- mycube
      fcube$vb[4,] <- fcube$vb[4,]*1.01
      fcube$material$col <- fillcol
      fcube %>% translate3d(x[i], y[i], z[i]) %>% shade3d
    }
  }
}

clear3d()
cube(0,0,0)
cube(1,0,0, filled=F)
cube(-1,0,0, bordered=F)
axes3d()

cube(vox$voxels[,1],vox$voxels[,2],vox$voxels[,3])
