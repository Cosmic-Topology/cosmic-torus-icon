# R: major radius
# r: minor radius
# S: number of segments for the major ring
# s: number of segments for the minor ring
# arc: the arc to draw
createTorusMesh <- function(R=1, r=0.33, S=64, s=32, arc=2*pi){
  vertices <- indices <- NULL
  for (j in 0:s) {
    for (i in 0:S) {
      u <- i / S * arc
      v <- j / s * 2*pi
      vertex <- c(
        (R + r*cos(v)) * cos(u),
        (R + r*cos(v)) * sin(u),
        r * sin(v)
      )
      vertices <- cbind(vertices, vertex)
    }
  }
  for (j in 1:s) {
    for (i in 1:S) {
      a <- (S + 1) * j + i 
      b <- (S + 1) * (j - 1) + i 
      c <- (S + 1) * (j - 1) + i + 1
      d <- (S + 1) * j + i + 1
      indices <- cbind(indices, c(a,b,d), c(b,c,d))
    }
  }
  
  list(vertices=vertices, indices=indices)  
}
torus <- createTorusMesh()
verts <- rbind(torus$vertices,1)
trgls <- torus$indices

library(rgl)
library("RColorBrewer")
tmesh <- tmesh3d(verts, trgls)
shade3d(tmesh, color=brewer.pal(n = 8, name = 'YlGnBu'))
rgl.snapshot("torus.png")
