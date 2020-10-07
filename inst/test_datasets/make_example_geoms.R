library(geometr)
library(magrittr)
library(tibble)
library(sf)

# administrative level 1
attr <- tibble(fid = 1,
               NAME_0 = "Estonia")
obj1 <- tibble(x = c(0, 100, 100, 0),
               y = c(0, 0, 100, 100)) %>%
  gs_polygon() %>%
  setFeatures(table = attr) %>%
  gc_sf()
st_write(obj = obj1, dsn = "example_geom1.gpkg")

# administrative level 2
attr <- tibble(fid = c(1:4),
               NAME_0 = "Estonia",
               NAME_1 = c("unit1", "unit2", "unit3", "unit4"))
obj2 <- tibble(x = c(0, 50, 50, 0, 50, 100, 100, 50, 0, 50, 50, 0, 50, 100, 100, 50),
               y = c(0, 0, 50, 50, 0, 0, 50, 50, 50, 50, 100, 100, 50, 50, 100, 100),
               fid = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)) %>%
  gs_polygon() %>%
  setFeatures(table = attr) %>%
  gc_sf()
st_write(obj = obj2, dsn = "example_geom2.gpkg")

# administrative level 3 - first dataseries
attr <- tibble(fid = c(1:6),
               NAME_0 = "Estonia",
               NAME_1 = c("unit1", "unit1", "unit2", "unit3", "unit4", "unit4"),
               NAME_2 = c("unit11", "unit12", "unit21", "unit31", "unit41", "unit42"))
obj3 <- tibble(x = c(0, 25, 25, 0,
                    25, 50, 50, 25,
                    50, 100, 100, 50,
                    0, 50, 50, 0,
                    50, 100, 100, 50,
                    50, 100, 100, 50),
              y = c(0, 0, 50, 50,
                    0, 0, 50, 50,
                    0, 0, 50, 50,
                    50, 50, 100, 100,
                    50, 50, 75, 75,
                    75, 75, 100, 100),
              fid = c(1, 1, 1, 1,
                      2, 2, 2, 2,
                      3, 3, 3, 3,
                      4, 4, 4, 4,
                      5, 5, 5, 5,
                      6, 6, 6, 6)) %>%
  gs_polygon() %>%
  setFeatures(table = attr) %>%
  gc_sf()
st_write(obj = obj3, dsn = "example_geom3.gpkg")

# administrative level 3 - second dataseries
attr <- tibble(fid = c(1:6),
               NAME_0 = "Estonia",
               NAME_1 = c("unit1", "unit1", "unit2", "unit3", "unit4", "unit4"),
               NAME_2 = c("unit11", "unit12", "unit21", "unit31", "unit41", "unit42"))
obj4 <- tibble(x = c(0, 25, 25, 35, 35, 0,
                     25, 25, 35, 35, 50, 50,
                     50, 100, 100, 50,
                     0, 50, 50, 0,
                     50, 100, 100, 50,
                     50, 100, 100, 50),
               y = c(0, 0, 25, 25, 50, 50,
                     0, 25, 25, 50, 50, 0,
                     0, 0, 50, 50,
                     50, 50, 100, 100,
                     50, 50, 75, 75,
                     75, 75, 100, 100),
               fid = c(1, 1, 1, 1, 1, 1,
                       2, 2, 2, 2, 2, 2,
                       3, 3, 3, 3,
                       4, 4, 4, 4,
                       5, 5, 5, 5,
                       6, 6, 6, 6)) %>%
  gs_polygon() %>%
  setFeatures(table = attr) %>%
  gc_sf()
st_write(obj = obj4, dsn = "example_geom4.gpkg")

