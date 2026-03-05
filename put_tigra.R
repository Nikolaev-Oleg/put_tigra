library(tidyverse)
library(plotly)

path <- "C:/Users/nikol/Downloads/Telegram Desktop/LOG001.csv"

data <- read.csv(path)
data$a_g <- sqrt(data$ax_g^2 + data$ay_g^2 + data$az_g^2)
# Rotation ####

rxo = 0
ryo = 0
rzo = 0

rx <- c(rxo)
ry <- c(ryo)
rz <- c(rzo)

Rx = matrix(c(1, 0, 0,
              0, cos(rx), -sin(rx),
              0, sin(rx), cos(rx)),
            byrow = T,
            nrow = 3
)

Ry = matrix(c(cos(ry), 0, sin(ry),
              0, 1, 0,
              -sin(ry), 0, cos(ry)),
            byrow = T,
            nrow = 3
)

Rz = matrix(c(cos(rz), -sin(rz), 0,
              sin(rz), cos(rz), 0,
              0, 0, 1),
            byrow = T,
            nrow = 3
)

R = Rx %*% Ry %*% Rz


for(step in 2:nrow(data)){
  
  .rx = rx[length(rx)] + data$gx_dps[step-1]*pi/180 * data$dt_ms[step]/1000
  .ry = ry[length(ry)] + data$gy_dps[step-1]*pi/180 * data$dt_ms[step]/1000
  .rz = rz[length(rz)] + data$gz_dps[step-1]*pi/180 * data$dt_ms[step]/1000
  
  .Rx = matrix(c(1, 0, 0,
                 0, cos(.rx), -sin(.rx),
                 0, sin(.rx), cos(.rx)),
               byrow = T,
               nrow = 3
  )
  
  .Ry = matrix(c(cos(.ry), 0, sin(.ry),
                 0, 1, 0,
                 -sin(.ry), 0, cos(.ry)),
               byrow = T,
               nrow = 3
  )
  
  .Rz = matrix(c(cos(.rz), -sin(.rz), 0,
                 sin(.rz), cos(.rz), 0,
                 0, 0, 1),
               byrow = T,
               nrow = 3
  )
  
  .R = .Rx %*% .Ry %*% .Rz
  
  R = R %*% .R
  
  glob <- R %*% c(.rx, .ry, .rz)
  
  .rx = glob[1]
  .ry = glob[2]
  .rz = glob[3]
  
  rx <- append(rx, .rx)
  ry <- append(ry, .ry)
  rz <- append(rz, .rz)
  
}

rot <- data_frame(rx, ry, rz)

rot$t <- data$t_s

# Trajectory ####
x <- c(xo)
y <- c(yo)
z <- c(zo)

vx <- c(vxo)
vy <- c(vyo)
vz <- c(vzo)

for(step in 2:nrow(data)){
  
  rx = rot$rx[step-1]
  ry = rot$ry[step-1]
  rz = rot$rz[step-1]
  
  .Rx = matrix(c(1, 0, 0,
                 0, cos(rx), -sin(rx),
                 0, sin(rx), cos(rx)),
               byrow = T,
               nrow = 3
  )
  
  .Ry = matrix(c(cos(ry), 0, sin(ry),
                 0, 1, 0,
                 -sin(ry), 0, cos(ry)),
               byrow = T,
               nrow = 3
  )
  
  .Rz = matrix(c(cos(rz), -sin(rz), 0,
                 sin(rz), cos(rz), 0,
                 0, 0, 1),
               byrow = T,
               nrow = 3
  )
  
  R = .Rx %*% .Ry %*% .Rz
  
  glob = R %*% c(data$ax_g[step-1], data$ay_g[step-1], data$az_g[step-1])
  
  .ax = glob[1]
  .ay = glob[2]
  .az = glob[3] - 1
  
  .vx = vx[length(vx)] + .ax * data$dt_ms[step]/1000
  .vy = vy[length(vy)] + .ay * data$dt_ms[step]/1000
  .vz = vz[length(vz)] + .az * data$dt_ms[step]/1000
  
  .x = x[length(x)] + vx[length(vx)] * data$dt_ms[step]/1000 + .ax/2 * (data$dt_ms[step]/1000)^2
  .y = y[length(y)] + vy[length(vy)] * data$dt_ms[step]/1000 + .ay/2 * (data$dt_ms[step]/1000)^2
  .z = z[length(z)] + vz[length(vz)] * data$dt_ms[step]/1000 + .az/2 * (data$dt_ms[step]/1000)^2
  
  
  vx <- append(vx, .vx)
  vy <- append(vy, .vy)
  vz <- append(vz, .vz)
  
  x <- append(x, .x)
  y <- append(y, .y)
  z <- append(z, .z)
}

df <- data_frame(x, y, z, vx, vy, vz) %>%
  mutate(v = sqrt(vx^2 + vy^2 + vz^2))

# Plots ####

#XY trajectory
ggplot(df, aes(x, y, colour = v))+
  geom_point()+
  geom_path()+
  theme_classic()+
  scale_colour_continuous(type = 'viridis')

#XYZ trajectory
plot_ly(df, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines",
        line = list(width = 5, color = ~v, colorscale = "Viridis")) %>%
  layout(title = "Put tigra",
         scene = list(xaxis = list(title = "X"),
                      yaxis = list(title = "Y"),
                      zaxis = list(title = "Z",
                                   backgroundcolor = "rgb(230, 230,230)",
                                   gridcolor = "rgb(255, 255, 255)",
                                   showbackground = TRUE)))

#Rotation
plot_ly(rot, x = ~rx, y = ~ry, z = ~rz, type = "scatter3d", mode = "lines",
        line = list(width = 5, color = ~t, colorscale = "Viridis")) %>%
  layout(title = "Put tigra",
         scene = list(xaxis = list(title = "X"),
                      yaxis = list(title = "Y"),
                      zaxis = list(title = "Z",
                                   backgroundcolor = "rgb(230, 230,230)",
                                   gridcolor = "rgb(255, 255, 255)",
                                   showbackground = TRUE)))

ggplot()+
  geom_path(data = rot, aes(t, rx),
             color = '#2299ee')+
  geom_path(data = rot, aes(t, ry),
             color = '#22ee88')+
  geom_path(data = rot, aes(t, rz),
             color = '#ee7722')+
  theme_classic()
