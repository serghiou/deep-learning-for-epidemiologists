# Copyright 2020 Stylianos Serghiou. All Rights Reserved.
# Copyright 2016 Google Inc. All Rights Reserved.

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

# http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# ==============================================================================



# Spiral data -------------------------------------------------------------

classifySpiralData <- function(numSamples, noise) {

  n = floor(numSamples / 2)

  genSpiral <- function(deltaT, label) {
    points <- matrix(nrow = n, ncol = 3)
    for (i in 1:n) {
      r = i / n * 5
      t = 1.75 * i / n * 2 * pi + deltaT
      x = r * sin(t) + runif(1, -1, 1) * noise
      y = r * cos(t) + runif(1, -1, 1) * noise
      points[i, ] <- c(x, y, label)
    }
    points
  }

  points <- genSpiral(0, 1)
  points <- rbind(points, genSpiral(pi, -1))
  return(points)
}

library(tidyverse)
classifySpiralData(200, 0.1) %>%
  data.frame() %>%
  setNames(c("x", "y", "labels")) %>%
  as_tibble() %>%
  ggplot(aes(x = x, y = y, color = as.factor(labels))) +
  geom_point(show.legend = F) +
  theme_minimal()


# Circle data -------------------------------------------------------------

classifyCircleData <- function(numSamples, noise){

  radius <- 5
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

  getCircleLabel <- function(p, center) {
    ifelse(euc.dist(p, center) < (radius * 0.5), 1, -1)
  }

  n <- floor(numSamples / 2)
  points <- matrix(nrow = n, ncol = 3)

  # Generate positive points inside the circle.
  for (i in 1:n) {
    r = runif(1, 0, radius * 0.5)
    angle = runif(1, 0, 2 * pi)
    x = r * sin(angle)
    y = r * cos(angle)
    noiseX = runif(1, -radius, radius) * noise
    noiseY = runif(1, -radius, radius) * noise
    label = getCircleLabel(c(x + noiseX, y + noiseY), c(0, 0))
    points[i, ] <- c(x, y, label)
  }

  # Generate negative points outside the circle.
  for (i in 1:n) {
    r = runif(1, radius * 0.7, radius)
    angle = runif(1, 0, 2 * pi)
    x = r * sin(angle)
    y = r * cos(angle)
    noiseX = runif(1, -radius, radius) * noise
    noiseY = runif(1, -radius, radius) * noise
    label = getCircleLabel(c(x + noiseX, y + noiseY), c(0, 0))
    points <- rbind(points, c(x, y, label))
  }
  points
}


# XOR Data ----------------------------------------------------------------


classifyXORData <- function(numSamples, noise) {

  getXORLabel <- function(p) ifelse(p[1] * p[2] >= 0, 1, -1)

  n <- numSamples
  points <- matrix(nrow = n, ncol = 3)

  for (i in 1:n) {
    x = runif(1, -5, 5)
    padding = 0.3
    ifelse(x > 0, x + padding, x - padding)
    y = runif(1, -5, 5)
    ifelse(y > 0, y + padding, y - padding)
    noiseX = runif(1, -5, 5) * noise
    noiseY = runif(1, -5, 5) * noise
    label = getXORLabel(c(x + noiseX, y + noiseY))
    points[i, ] <- c(x, y, label)
  }
  points
}



# Gaussian data ---------------------------------------------------------------

classifyTwoGaussData <- function(numSamples, noise) {

  variance = scales::rescale(noise, to = c(0.5, 4), from = c(0, 0.5))

  genGauss <- function(cx, cy, label) {
    n <- floor(numSamples / 2)
    points <- matrix(nrow = n, ncol = 3)

    for (i in 1:n) {
      x = rnorm(1, cx, variance)
      y = rnorm(1, cy, variance)
      points[i, ] <- c(x, y, label)
    }
    points
  }

  points <- genGauss(2, 2, 1)
  points <- rbind(points, genGauss(-2, -2, -1))
  points
}
