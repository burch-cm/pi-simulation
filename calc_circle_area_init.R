
# create a square
# randomly fill it with points
# calculate how many points are within 1 unit radius of center

n_points <- 10000
x_val <- runif(n_points, min = -1, max = 1)
y_val <- runif(n_points, min = -1, max = 1)

dist_from_origin <- function(x, y) {
    # origin is c(0, 0)
    sqrt(sum(x^2, y^2))
}

circle_fn <- function(center = c(0,0), radius = 1, n_points = 100){
    tt <- seq(0, 2*pi, length.out = n_points)
    xx <- center[1] + radius * cos(tt)
    yy <- center[2] + radius * sin(tt)
    return(data.frame(x = xx, y = yy))
}

df <- data.frame(x_val, y_val)

origin_dist <- 
    Map(dist_from_origin, df$x_val, df$y_val) |> 
    unlist()
df['dist'] = origin_dist

interior_points_ratio <- df[df$dist <= 1, ] |> nrow() |> (\(x) x/n_points)()
square_area = 2 * 2
circle_area = square_area * interior_points_ratio
circle_area

library(ggplot2)

circle_points <- circle_fn(n_points = 100)

ggplot(df, aes(x = x_val, y = y_val)) +
    geom_point(pch = 20) +
    coord_equal() +
    geom_path(data = circle_points,
              aes(x = x, y = y),
              col = 'firebrick1',
              size = 1.2) +
    geom_polygon(data = circle_points, 
                 aes(x = x, y = y), 
                 fill = 'firebrick1',
                 alpha = 0.5)
