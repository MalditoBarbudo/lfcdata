# lfcdata (development version)

* Added `lfcLiDAR$point_value` method, as well as its external counterpart
  `lidar_point_value`
* Refactored lfcLiDAR class and methods. `lfcLiDAR$get_data` method now returns aggregated
  and precalculated data. `lfcLiDAR$get_lowres_raster` access the low resolution rasters
  and `lfcLiDAR$clip_and_stats` makes calculation based on raw data. 
* Added `lfcLiDAR$clip_and_stats` method, as well as its external counterpart
  `lidar_clip_and_stats`

# lfcdata 0.0.4

* Added `lfcNFI$describe_table` method, as well as its external counterpart
  `nif_describe_table`
* Better logic for describe methods in all classes
* General documentation improvements

# lfcdata 0.0.3

* Using RPostgres instead of RPostgreSQL library.
* Fixed slow database connection.
* Improved Allometries class and methods documentation
* Improved warning/error managing in lfcAllometries calculate method

# lfcdata 0.0.2

* Added a `NEWS.md` file to track changes to the package.
* Access to NFI, LiDAR and Allometries databases.
* Cache method implemented.
