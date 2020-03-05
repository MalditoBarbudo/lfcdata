## class object creation works ####
test_that("class object creation works", {
  expect_is(lidar(), c('lfcLiDAR'))
  expect_equal(lfcdata:::lfcLiDAR$new(), lidar())
  expect_true(rlang::is_function(lidar()$get_data))
  expect_true(rlang::is_function(lidar()$get_lowres_raster))
  expect_true(rlang::is_function(lidar()$avail_tables))
  expect_true(rlang::is_function(lidar()$describe_var))
  expect_true(rlang::is_function(lidar()$clip_and_stats))
})

# lidardb to avoid calling the db so often
lidardb <- lidar()

## get data method works ####
test_that("get_data method works", {
  skip_on_cran()
  skip_on_travis()
  expect_s3_class(lidardb$get_data('lidar_provinces', c('DBH', 'AB')), 'sf')
  expect_s3_class(lidardb$get_data('lidar_provinces', 'REC'), 'sf')
  expect_equal(nrow(lidardb$get_data('lidar_provinces', c('DBH', 'AB'))), 4)
  expect_error(lidardb$get_data(1, 'REC'), 'not character')
  expect_error(lidardb$get_data('lidar_provinces', c(1,2)), 'not character')
  expect_error(
    lidardb$get_data(c('lidar_provinces', 'lidar_municipalities'), 'REC'), 'must be of length'
  )
  expect_error(lidardb$get_data('lidar_provincilities', c('DBH', 'AB')), 'Must be one of')
  expect_error(lidardb$get_data('lidar_provinces', c('AC')), 'Must be one of')
  expect_equal(ncol(lidardb$get_data('lidar_provinces', 'REC')), 10)
  expect_true(all(
    c('DBH_pixels', 'AB_pixels', 'DBH_sd', 'AB_sd') %in%
      names(lidardb$get_data('lidar_provinces', c('DBH', 'AB')))
  ))
  expect_true(all(
    c(
      'DBH_pixels', 'AB_pixels', 'REC_pixels', 'VAE_pixels', 'BAT_pixels',
      'BF_pixels', 'CAT_pixels', 'HM_pixels'
    ) %in% names(lidardb$get_data('lidar_provinces', 'all'))
  ))
  expect_true(all(
    c(
      'DBH_pixels', 'AB_pixels', 'REC_pixels', 'VAE_pixels', 'BAT_pixels',
      'BF_pixels', 'CAT_pixels', 'HM_pixels'
    ) %in% names(lidardb$get_data('lidar_provinces', c('all', 'DBH')))
  ))
})

## get_lowres_raster method works ####
test_that("get_lowres_raster method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(lidardb$get_lowres_raster('AB', 'raster'), 'RasterLayer')
  expect_is(lidardb$get_lowres_raster(c('AB', 'DBH'), 'raster'), 'RasterBrick')
  expect_is(lidardb$get_lowres_raster(c('DBH', 'AB'), 'raster'), 'RasterBrick')
  expect_s3_class(lidardb$get_lowres_raster('AB', 'stars'), 'stars')
  expect_s3_class(lidardb$get_lowres_raster(c('AB', 'DBH'), 'stars'), 'stars')
  expect_s3_class(lidardb$get_lowres_raster(c('DBH', 'AB'), 'stars'), 'stars')
  expect_error(lidardb$get_lowres_raster(1, 'raster'), 'not character')
  expect_error(lidardb$get_lowres_raster('non_existent_table', 'raster'), 'Must be one of')
  expect_error(lidardb$get_lowres_raster('AB', 1), 'not character')
})

## avail_tables method works ####
test_that("avail_tables method works", {
  expect_type(lidardb$avail_tables(), 'character')
  expect_true('lidar_provinces' %in% lidardb$avail_tables())
  expect_true('lidar_pein' %in% lidardb$avail_tables())
})

## describe_var method works ####
test_that("describe_var method works", {
  skip_on_cran()
  skip_on_travis()
  expect_is(lidardb$describe_var('AB'), c('lfcLiDAR'))
  expect_output(lidardb$describe_var('AB'))
  expect_output(lidardb$describe_var(c('AB', 'DBH')))
  expect_error(lidardb$describe_var(c('AB', 'DBH', 'tururu')), 'Must be one of')
  expect_error(lidardb$describe_var('tururu'), 'Must be one of')
  expect_error(lidardb$describe_var(25), 'not character')
})

# sf objects to test
sf_polygons <-
  lidardb$get_data('lidar_municipalities', 'DBH') %>%
  dplyr::slice(1:5) %>%
  dplyr::select(tururu = poly_id)

sf_points <-
  nfi()$get_data('plots', spatial = TRUE) %>%
  dplyr::slice(1:5) %>%
  dplyr::select(plot_id)

sf_multipoints <-
  dplyr::tibble(
    point_id = 'wrong',
    geometry = sf::st_multipoint(matrix(1:10, , 2)) %>% sf::st_sfc()
  ) %>%
  sf::st_as_sf(sf_column_name = 'geometry')

sf_polygons_latlong <-
  sf_polygons %>% sf::st_transform(crs = 4326)

sf_empty_polygon <-
  structure(list(poly_id = "Premià de Mar", geometry = structure(list(
    structure(list(list(structure(c(446596.9042, 446625.7538,
                                    446671.6633, 446697.5531, 446840.5029, 446983.4727, 447027.4027,
                                    447132.3127, 447162.4827, 447228.043, 447267.0431, 447302.4732,
                                    447431.0736, 447532.814, 447608.4442, 447664.0996, 447698.5047,
                                    447714.6989, 447833.4634, 447925.3263, 447984.0297, 447977.59,
                                    447968.48, 447959.13, 447950.71, 447942.31, 447936.73, 447931.41,
                                    447926.06, 447921.1, 447918.3, 447915.7, 447910.52, 447906.59,
                                    447894.16, 447881.3, 447867.75, 447854.15, 447845.34, 447837.36,
                                    447829.54, 447814.3, 447795.79, 447777.61, 447766.86, 447756.37,
                                    447735.62, 447706.55, 447677.7, 447663.8, 447650.18, 447622.9,
                                    447614.55, 447605.98, 447588.63, 447577.3, 447574.4, 447571.74,
                                    447566.62, 447562.94, 447560.6, 447552.8, 447547.49, 447544.59,
                                    447530.15, 447515.73, 447502.46, 447489.23, 447479.01, 447469.12,
                                    447463.22, 447457.82, 447447.6, 447441.09, 447439.61, 447439.13,
                                    447439, 447439.15, 447439.3, 447441.65, 447443.83, 447445.78,
                                    447447.51, 447450.83, 447455.68, 447460.53, 447463.03, 447465.85,
                                    447471.48, 447474.05, 447475.98, 447476.49, 447476.72, 447476.42,
                                    447475.69, 447474.08, 447471.78, 447469.84, 447466.7, 447461.64,
                                    447456.33, 447452.6, 447447.31, 447439.39, 447431.42, 447426.28,
                                    447408.36, 447390.44, 447371.77, 447353.09, 447345.92, 447338.57,
                                    447323.7, 447313.91, 447295.58, 447277.22, 447262.44, 447247.69,
                                    447231.33, 447214.97, 447200.41, 447185.81, 447167.62, 447161.34,
                                    447157.62, 447145.84, 447134.07, 447115.88, 447097.7, 447095.12,
                                    447090.56, 447076.71, 447061.98, 447054.63, 447047.32, 447042.09,
                                    447034.5, 447027.09, 447015.99, 447004.98, 446994.21, 446983.62,
                                    446980.49, 446978.4, 446975.51, 446972.72, 446971.07, 446969.16,
                                    446968.58, 446968.33, 446968.59, 446969.99, 446972.1, 446974.07,
                                    446975.24, 446976.74, 446978.34, 446980.74, 446983.14, 446986.46,
                                    446988.12, 446989.78, 446991.32, 446993.24, 446993.84, 446994.1,
                                    446994.2, 446994.28, 446994.35, 446994.45, 446994.67, 446995.67,
                                    446998.1, 447000.87, 447016.3, 447031.79, 447040.62, 447049.68,
                                    447052.36, 447056.53, 447060.7, 447066.61, 447072.56, 447079.07,
                                    447085.59, 447088.81, 447090.39, 447091.91, 447097.28, 447102.66,
                                    447117.41, 447132.16, 447136.27, 447143.27, 447148.39, 447163.32,
                                    447175.46, 447187.64, 447200.87, 447214.14, 447219.27, 447224.32,
                                    447226.92, 447230.39, 447232.16, 447234.11, 447239.63, 447245.36,
                                    447251.18, 447253.32, 447255.45, 447260.46, 447265.44, 447267.2,
                                    447269.6, 447271.99, 447275.14, 447278.33, 447282.02, 447285.73,
                                    447288.3, 447290.88, 447293.23, 447295.58, 447298.17, 447300.76,
                                    447305.37, 447310.07, 447312.58, 447315.1, 447317.38, 447321.22,
                                    447337.44, 447353.66, 447371.22, 447388.74, 447399.63, 447410.51,
                                    447419.09, 447427.65, 447433.06, 447438.49, 447441.81, 447445.06,
                                    447447.01, 447448.5, 447449.67, 447450.09, 447450.14, 447449.68,
                                    447446.45, 447443.22, 447416.56, 447413.87, 447409.86, 447412.55,
                                    447401.8, 447394.57, 447392.14, 447398.98, 447389.35, 447388.56,
                                    447386.67, 447381.85, 447380.78, 447376.8, 447379.58, 447369.37,
                                    447338.93, 447313.29, 447304.37, 447302.87, 447297.55, 447287.45,
                                    447260.25, 447216.67, 447164.65, 447100.56, 447105.62, 447103.52,
                                    447078.32, 447037.1, 447022.42, 447015.92, 447030.07, 447020.59,
                                    447037.45, 447033.32, 447031.12, 447010.68, 447016.69, 447018.74,
                                    447044.14, 447058.18, 447061.12, 447064.05, 447064.03, 447062.69,
                                    447060.82, 447059.78, 447058.43, 447056.72, 447055.01, 447051.55,
                                    447048.19, 447035.2, 447022.2, 447010.36, 446998.52, 446990.88,
                                    446983.17, 446979.99, 446976.8, 446975.48, 446973.8, 446972.66,
                                    446968.8, 446964.93, 446959.74, 446954.54, 446952.05, 446949.57,
                                    446948.18, 446946.78, 446946.42, 446946.26, 446946.07, 446926.85,
                                    446907.03, 446894.66, 446876, 446857.28, 446837.98, 446836.05,
                                    446818.64, 446811.93, 446801.84, 446791.76, 446776.27, 446760.8,
                                    446746.86, 446732.95, 446722.36, 446711.79, 446704.17, 446696.63,
                                    446694.29, 446691.02, 446687.84, 446686.11, 446685.51, 446685.18,
                                    446684.85, 446684.75, 446685.71, 446686.5, 446686.89, 446686.84,
                                    446686.45, 446685.86, 446684.5, 446682.57, 446681.13, 446678.97,
                                    446676.81, 446673.56, 446670.31, 446668.76, 446667.28, 446663.72,
                                    446658.39, 446652.96, 446632.47, 446622.24, 446612.14, 446593.58,
                                    446575.02, 446557.44, 446539.84, 446525.61, 446517.33, 446511.32,
                                    446496.48, 446481.65, 446471.6, 446456.75, 446441.89, 446422.99,
                                    446404.14, 446382.95, 446361.88, 446345.43, 446329.05, 446309.39,
                                    446289.87, 446266.82, 446243.84, 446221.74, 446199.73, 446188.32,
                                    446181.67, 446163.66, 446159.98, 446153.26, 446143.04, 446138.12,
                                    446133.47, 446123.83, 446114.2, 446115.71, 446118.25, 446120.8,
                                    446122.1, 446123.07, 446123.87, 446124.42, 446124.33, 446123.72,
                                    446122.57, 446119.69, 446117.11, 446114.58, 446112.07, 446109.6,
                                    446107.5, 446105.48, 446104.01, 446102.94, 446102.4, 446101.87,
                                    446100.81, 446099.85, 446099.13, 446098.41, 446095.25, 446094.83,
                                    446092.1, 446087.91, 446081.7, 446075.53, 446060.25, 446045.16,
                                    446028.89, 446012.91, 445996.43, 445980.07, 445961.77, 445943.44,
                                    445933.62, 445918.71, 445903.94, 445889.2, 445874.56, 445860.29,
                                    445846.04, 445836.19, 445826.34, 445816.4, 445806.45, 445803.31,
                                    445800.35, 445798.61, 445797.86, 445797.34, 445796.97, 445796.98,
                                    445796.97, 445797.42, 445797.87, 445798.34, 445798.91, 445800.15,
                                    445801.38, 445802.22, 445803.04, 445803.74, 445804.44, 445805.29,
                                    445805.98, 445806.11, 445805.76, 445805.32, 445804.37, 445802.82,
                                    445801.24, 445798.95, 445796.57, 445795.22, 445793.78, 445790.89,
                                    445788.88, 445787.04, 445786.16, 445785, 445783.85, 445782.9,
                                    445781.96, 445781.77, 445782, 445781.89, 445781.02, 445780.18,
                                    445779.62, 445779.06, 445778.39, 445777.62, 445776.63, 445775.63,
                                    445775.01, 445774.4, 445772.94, 445771.45, 445770.62, 445769.78,
                                    445765.69, 445761.6, 445755.83, 445751.47, 445744.93, 445741.7,
                                    445738.51, 445725.39, 445712.35, 445698.9, 445685.47, 445677.75,
                                    445666.34, 445654.86, 445646.2, 445632.95, 445626.38, 445619.95,
                                    445603.89, 445587.86, 445566.67, 445545.39, 445536.48, 445522.91,
                                    445516.17, 445509.55, 445501.54, 445489.94, 445484.13, 445478.2,
                                    445474.3887, 445473.3041, 445446.5159, 445396.7095, 445394.5916,
                                    445245.3, 445305.1144, 445319.4846, 445326.785, 445358.2355,
                                    445429.0563, 445493.3169, 445503.8071, 445514.6676, 445522.1679,
                                    445530.048, 445570.7082, 445604.7985, 445622.9989, 445644.1496,
                                    445676.9298, 445705.1502, 445780.2108, 445826.1111, 445917.9118,
                                    445986.1021, 446008.4122, 446063.4823, 446084.1224, 446133.9728,
                                    446253.3034, 446386.7633, 446417.6634, 446495.924, 446565.5346,
                                    446570.6746, 446596.9042, 4594255.6188, 4594199.1985, 4594121.7981,
                                    4594091.6178, 4594040.4367, 4593980.0955, 4593965.6551, 4593947.2943,
                                    4593947.2941, 4593969.5836, 4593978.7833, 4593978.7831, 4594005.8821,
                                    4594038.5514, 4594052.1608, 4594087.2677, 4594102.1519, 4594107.6633,
                                    4593964.0641, 4593857.6398, 4593786.9619, 4593783.27, 4593778.87,
                                    4593775.02, 4593772.22, 4593769.42, 4593766.81, 4593763.61,
                                    4593759.67, 4593755.3, 4593752.35, 4593749.18, 4593742.75,
                                    4593738.89, 4593729.21, 4593720.14, 4593711.39, 4593702.78,
                                    4593696.67, 4593690.58, 4593684.28, 4593671.16, 4593656.51,
                                    4593641.48, 4593632.27, 4593622.77, 4593603.48, 4593576.49,
                                    4593549.27, 4593535.62, 4593521.69, 4593493.89, 4593485.99,
                                    4593478.32, 4593463.2, 4593452.89, 4593449.84, 4593446.55,
                                    4593439.79, 4593435.53, 4593433.32, 4593427.21, 4593423.64,
                                    4593421.69, 4593412.68, 4593403.65, 4593395.11, 4593386.49,
                                    4593379.38, 4593371.75, 4593366.46, 4593360.73, 4593348.41,
                                    4593340.72, 4593337.94, 4593336.26, 4593334.25, 4593331.76,
                                    4593329.27, 4593326.28, 4593323.19, 4593319.6, 4593315.88,
                                    4593308.35, 4593295.24, 4593282.12, 4593276.08, 4593270.19,
                                    4593258.32, 4593251.11, 4593243.74, 4593240.93, 4593238.06,
                                    4593232.26, 4593228.45, 4593223.33, 4593218.51, 4593215.47,
                                    4593211.63, 4593206.65, 4593202.35, 4593199.85, 4593196.91,
                                    4593193.5, 4593190.31, 4593187.81, 4593177.9, 4593167.97,
                                    4593157.56, 4593147.19, 4593143.53, 4593140.22, 4593133.95,
                                    4593129.52, 4593120.36, 4593111.25, 4593104.4, 4593097.49,
                                    4593089.2, 4593080.9, 4593073.81, 4593066.78, 4593057.37,
                                    4593054.86, 4593053.78, 4593050.91, 4593048.04, 4593043.88,
                                    4593039.71, 4593039.28, 4593039.14, 4593040.63, 4593041.78,
                                    4593042.62, 4593043.91, 4593045.3, 4593048.08, 4593051.35,
                                    4593056.57, 4593061.94, 4593067.47, 4593073.43, 4593075.62,
                                    4593077.52, 4593080.77, 4593084.47, 4593086.91, 4593090.61,
                                    4593092.48, 4593094.35, 4593096.79, 4593100.47, 4593104.14,
                                    4593107.06, 4593108.27, 4593109.27, 4593109.9, 4593110.25,
                                    4593110.28, 4593110.16, 4593109.86, 4593109.26, 4593108.42,
                                    4593106.77, 4593105.76, 4593104.63, 4593101.43, 4593098.24,
                                    4593095.93, 4593093.62, 4593092.54, 4593090.87, 4593088.83,
                                    4593087.28, 4593079.64, 4593072.19, 4593067.87, 4593063.88,
                                    4593063.28, 4593063.28, 4593063.22, 4593062.16, 4593061.41,
                                    4593061.29, 4593061.2, 4593060.86, 4593060.81, 4593061.13,
                                    4593063.26, 4593065.39, 4593067.75, 4593070.11, 4593070.96,
                                    4593073.21, 4593075.37, 4593082.98, 4593089.78, 4593096.41,
                                    4593102.65, 4593108.85, 4593111.39, 4593114.09, 4593115.97,
                                    4593119.39, 4593121.06, 4593122.49, 4593125.57, 4593128.22,
                                    4593130.28, 4593130.97, 4593131.66, 4593133.25, 4593134.95,
                                    4593135.85, 4593137.67, 4593139.51, 4593141.58, 4593143.6,
                                    4593145.7, 4593147.77, 4593149.36, 4593150.92, 4593152.17,
                                    4593153.41, 4593154.85, 4593156.28, 4593158.51, 4593160.57,
                                    4593161.61, 4593162.61, 4593163.56, 4593164.77, 4593172.71,
                                    4593180.65, 4593189.44, 4593198.33, 4593203.79, 4593209.25,
                                    4593213.62, 4593218.01, 4593220.69, 4593223.35, 4593225.12,
                                    4593227.07, 4593228.74, 4593230.93, 4593234.07, 4593237.28,
                                    4593239.69, 4593242.02, 4593249.84, 4593257.67, 4593320.12,
                                    4593318.79, 4593327.92, 4593329.29, 4593354.45, 4593351.29,
                                    4593357.29, 4593360.64, 4593383.39, 4593382.79, 4593386.9,
                                    4593384.88, 4593386.52, 4593385.16, 4593378.42, 4593373.96,
                                    4593360.67, 4593349.47, 4593345.28, 4593344.57, 4593357.02,
                                    4593352.81, 4593341.09, 4593322.3, 4593299.88, 4593272.26,
                                    4593260.7, 4593259.82, 4593249.29, 4593231.15, 4593253.5,
                                    4593249.23, 4593227.69, 4593223.3, 4593181.9, 4593180.22,
                                    4593185.61, 4593176.02, 4593171.99, 4593173.86, 4593161.9,
                                    4593157.06, 4593154.26, 4593151.45, 4593150.65, 4593147.1,
                                    4593143.67, 4593142.31, 4593141.31, 4593140.97, 4593141.27,
                                    4593142.37, 4593143.78, 4593150.53, 4593157.28, 4593164.14,
                                    4593171.01, 4593175.19, 4593179.24, 4593180.9, 4593182.57,
                                    4593183.45, 4593185.18, 4593187.26, 4593197.87, 4593208.48,
                                    4593221.2, 4593233.92, 4593239.27, 4593244.62, 4593248.05,
                                    4593251.48, 4593251.19, 4593251.13, 4593251.11, 4593254.89,
                                    4593258.78, 4593260.94, 4593263.45, 4593265.76, 4593268.17,
                                    4593268.37, 4593270.18, 4593270.63, 4593270.82, 4593270.63,
                                    4593269.99, 4593268.91, 4593267.56, 4593266, 4593264.6, 4593263.1,
                                    4593261.82, 4593260.21, 4593259.46, 4593257.72, 4593255.42,
                                    4593253.85, 4593253.02, 4593252.17, 4593251.01, 4593250.79,
                                    4593248.57, 4593246.31, 4593242.36, 4593240.37, 4593238.4,
                                    4593237, 4593235.21, 4593233.82, 4593233.22, 4593232.83,
                                    4593232.93, 4593233.6, 4593234.27, 4593234.48, 4593233.96,
                                    4593233.09, 4593231.18, 4593229.45, 4593225.22, 4593223.07,
                                    4593220.5, 4593215.15, 4593209.78, 4593204.68, 4593199.58,
                                    4593196.08, 4593194.18, 4593192.8, 4593189.63, 4593186.46,
                                    4593183.92, 4593179.27, 4593174.65, 4593169.61, 4593164.38,
                                    4593157.61, 4593150.47, 4593144.83, 4593138.97, 4593131.39,
                                    4593123.46, 4593113.87, 4593104.12, 4593094.42, 4593084.54,
                                    4593079.04, 4593075.83, 4593067.01, 4593065.53, 4593063.61,
                                    4593061.11, 4593059.46, 4593057.19, 4593051.43, 4593045.68,
                                    4593043.2, 4593037.56, 4593031.92, 4593027.85, 4593023.66,
                                    4593020.92, 4593018.15, 4593016.33, 4593015.14, 4593014.26,
                                    4593013.2, 4593012.85, 4593013.18, 4593014, 4593014.98, 4593015.94,
                                    4593017.1, 4593018.47, 4593020.18, 4593022.17, 4593024.13,
                                    4593025.75, 4593027.43, 4593029.48, 4593031.53, 4593031.55,
                                    4593031.55, 4593031.56, 4593031.27, 4593030.02, 4593028.41,
                                    4593024.42, 4593019.86, 4593014.13, 4593007.66, 4593000.25,
                                    4592992.62, 4592983.67, 4592974.78, 4592970.48, 4592964.44,
                                    4592958.08, 4592951.04, 4592943.78, 4592936.53, 4592929.24,
                                    4592924.13, 4592919.01, 4592914.12, 4592909.2, 4592907.1,
                                    4592904.69, 4592903.38, 4592902.63, 4592901.7, 4592899.39,
                                    4592897.06, 4592896.02, 4592894.56, 4592893.1, 4592891.79,
                                    4592890.52, 4592888.75, 4592886.98, 4592885.27, 4592883.55,
                                    4592882.06, 4592880.56, 4592878.51, 4592876.41, 4592874.47,
                                    4592872.49, 4592870.82, 4592869.36, 4592868.5, 4592867.86,
                                    4592866.12, 4592864.48, 4592864.08, 4592864.02, 4592864.55,
                                    4592865.1, 4592865.97, 4592866.91, 4592868.87, 4592870.83,
                                    4592872.43, 4592874.03, 4592875.42, 4592876.92, 4592877.87,
                                    4592879.68, 4592881.51, 4592883.25, 4592885, 4592886.84,
                                    4592888.64, 4592890.12, 4592891.58, 4592892.77, 4592893.96,
                                    4592895.12, 4592896.24, 4592896.76, 4592897.25, 4592897.87,
                                    4592898.48, 4592899.25, 4592899.62, 4592899.61, 4592899.26,
                                    4592898.63, 4592895.16, 4592891.46, 4592887.61, 4592883.69,
                                    4592881.12, 4592876.73, 4592872.58, 4592870.37, 4592867.95,
                                    4592866.56, 4592864.69, 4592858.56, 4592852.37, 4592844.96,
                                    4592837.81, 4592835.31, 4592832.25, 4592830.57, 4592828.49,
                                    4592825.09, 4592819.06, 4592816.06, 4592813.35, 4592811.9239,
                                    4592813.9953, 4592865.1561, 4592956.6076, 4592968.1331, 4593162.1,
                                    4593236.3271, 4593256.307, 4593305.057, 4593358.8468, 4593449.3864,
                                    4593521.5361, 4593538.596, 4593607.156, 4593642.246, 4593660.606,
                                    4593677.6757, 4593713.1055, 4593755.2554, 4593844.2954, 4593867.9152,
                                    4593910.8551, 4593971.2346, 4594001.4143, 4594081.4437, 4594109.0132,
                                    4594115.5631, 4594122.1227, 4594128.3525, 4594169.0222, 4594222.8214,
                                    4594186.6003, 4594182.9701, 4594256.9296, 4594313.3092, 4594313.3491,
                                    4594255.6188), .Dim = c(581L, 2L)))), class = c("XY", "MULTIPOLYGON",
                                                                                    "sfg"))), class = c("sfc_MULTIPOLYGON", "sfc"), precision = 0, bbox = structure(c(xmin = 445245.3,
                                                                                                                                                                      ymin = 4592811.9239, xmax = 447984.0297, ymax = 4594313.3491), class = "bbox"), crs = structure(list(
                                                                                                                                                                        epsg = 3043L, proj4string = "+proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"), class = "crs"), n_empty = 0L)), row.names = c(NA,
                                                                                                                                                                                                                                                                                                                               -1L), sf_column = "geometry", agr = structure(c(NA_integer_,
                                                                                                                                                                                                                                                                                                                                                                               NA_integer_), .Names = c("poly_id", NA), .Label = c("constant",
                                                                                                                                                                                                                                                                                                                                                                                                                                   "aggregate", "identity"), class = "factor"), class = c("sf",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          "tbl_df", "tbl", "data.frame")) %>%
  sf::st_as_sf()

## clip_and_stats method works ####
test_that("clip_and_stats method works", {
  skip_on_cran()
  skip_on_travis()
  expect_error(lidardb$clip_and_stats('sf', 'tururu', c('AB', 'DBH')), 'not a simple feature')
  expect_error(lidardb$clip_and_stats(sf_polygons, 1, c('AB', 'DBH')), 'not character')
  expect_error(lidardb$clip_and_stats(sf_polygons, 'tururu', c(1,2)), 'not character')
  expect_error(
    lidardb$clip_and_stats(sf_polygons, c('tururu', 'other_tururu'), c('AB', 'DBH')),
    'must be of length'
  )
  expect_error(lidardb$clip_and_stats(sf_polygons, 'tururu', c('AC', 'DBH')), 'Must be one of')
  expect_error(lidardb$clip_and_stats(sf_polygons, 'fake_id', 'AB'), 'Must be one of')
  expect_error(
    lidardb$clip_and_stats(sf_multipoints, 'point_id', c('AB', 'DBH')),
    'not a POLYGON or a MULTIPOLYGON'
  )
  expect_true(inherits(lidardb$clip_and_stats(sf_polygons, 'tururu', c('AB', 'DBH')), 'sf'))
  expect_identical(
    names(lidardb$clip_and_stats(sf_polygons, 'tururu', c('AB', 'DBH'))),
    c(
      'tururu', 'poly_km2',
      'AB_pixels', 'AB_average', 'AB_min', 'AB_max',
      'AB_sd', 'AB_km2', 'AB_km2_perc',
      'DBH_pixels', 'DBH_average', 'DBH_min', 'DBH_max',
      'DBH_sd', 'DBH_km2', 'DBH_km2_perc',
      'geometry'
    )
  )
  expect_equal(nrow(lidardb$clip_and_stats(sf_polygons, 'tururu', c('AB', 'DBH'))), 5)
  expect_equal(
    sf::st_crs(lidardb$clip_and_stats(sf_polygons_latlong, 'tururu', c('AB', 'DBH'))),
    sf::st_crs(4326)
  )
  expect_equal(
    sf::st_crs(lidardb$clip_and_stats(sf_polygons, 'tururu', c('AB', 'DBH'))),
    sf::st_crs(3043)
  )
  expect_equal(
    lidardb$clip_and_stats(sf_empty_polygon, 'poly_id', c('AB', 'DBH'))$poly_km2,
    as.numeric(sf::st_area(sf_empty_polygon)) / 1000000
  )
  expect_true(
    is.na(lidardb$clip_and_stats(sf_empty_polygon, 'poly_id', c('AB', 'DBH'))$AB_average)
  )
  expect_equal(
    nrow(lidardb$clip_and_stats(sf_empty_polygon, 'poly_id', c('AB', 'REC'))),
    1
  )
})

## point_value method works ####
test_that("point_value method works", {
  skip_on_cran()
  skip_on_travis()
  expect_error(lidardb$point_value('sf', 'plot_id', c('AB', 'DBH')), 'not a simple feature')
  expect_error(lidardb$point_value(sf_points, 1, c('AB', 'DBH')), 'not character')
  expect_error(lidardb$point_value(sf_points, 'plot_id', c(1,2)), 'not character')
  expect_error(
    lidardb$point_value(sf_points, c('plot_id', 'other_plot_id'), c('AB', 'DBH')),
    'must be of length'
  )
  expect_error(lidardb$point_value(sf_points, 'plot_id', c('AC', 'DBH')), 'Must be one of')
  expect_error(lidardb$point_value(sf_points, 'fake_id', 'AB'), 'Must be one of')
  expect_error(
    lidardb$point_value(sf_multipoints, 'point_id', c('AB', 'DBH')), 'not a POINT'
  )
  expect_true(inherits(lidardb$point_value(sf_points, 'plot_id', c('AB', 'DBH')), 'sf'))
  expect_identical(
    names(lidardb$point_value(sf_points, 'plot_id', c('AB', 'DBH'))),
    c('plot_id', 'AB', 'DBH', 'geometry')
  )
  expect_equal(nrow(lidardb$point_value(sf_points, 'plot_id', c('AB', 'DBH'))), 5)
})

## cache works ####
test_that("cache works", {
  skip_on_cran()
  skip_on_travis()
  expect_length(lidardb$.__enclos_env__$private$data_cache, 4)
  bar <- lidardb$get_lowres_raster('AB', 'raster')
  expect_is(lidardb$get_lowres_raster('AB', 'raster'), 'RasterLayer')
  temp_postgresql_conn <- pool::poolCheckout(
    lidardb$.__enclos_env__$private$pool_conn
  )
  expect_identical(
    bar,
    rpostgis::pgGetRast(
      temp_postgresql_conn, c('public', 'lidar_stack_utm'), bands = 1
    )
  )
  expect_identical(
    lidardb$get_lowres_raster(c('DBH', 'AB', 'BAT'), 'raster'),
    rpostgis::pgGetRast(
      temp_postgresql_conn, c('public', 'lidar_stack_utm'), bands = c(1,6,2)
    )
  )
  pool::poolReturn(temp_postgresql_conn)
  expect_length(lidardb$.__enclos_env__$private$data_cache, 5)
  baz <- lidardb$get_lowres_raster('DBH', 'raster')
  expect_length(lidardb$.__enclos_env__$private$data_cache, 6)
})

## external methods ####
test_that("external get data wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    lidardb$get_data(
      'lidar_provinces', c('AB', 'BAT', 'BF', 'CAT', 'DBH', 'HM', 'REC', 'VAE')
    ),
    lidar_get_data(lidardb, 'lidar_provinces')
  )
  expect_error(
    lidar_get_data('lidardb', 'lidar_provinces', c('DBH', 'AB')), "class lfcLiDAR"
  )
})

test_that("external get lowres_raster wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    lidardb$get_lowres_raster('AB', 'raster'), lidar_get_lowres_raster(lidardb, 'AB', 'raster')
  )
  expect_error(lidar_get_lowres_raster('lidardb', 'AB', 'raster'), "class lfcLiDAR")
  expect_identical(
    lidardb$get_lowres_raster(c('REC', 'BAT'), 'stars'),
    lidar_get_lowres_raster(lidardb, c('REC', 'BAT'), 'stars')
  )
  expect_length(lidardb$.__enclos_env__$private$data_cache, 7)
})

test_that("external describe_var wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(lidardb$describe_var('AB'), lidar_describe_var(lidardb, 'AB'))
  expect_error(lidar_describe_var('lidardb', 'density'), "class lfcLiDAR")
})

test_that("external clip_and_stats wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    lidardb$clip_and_stats(sf_polygons, 'tururu', 'AB'),
    lidar_clip_and_stats(lidardb, sf_polygons, 'tururu', 'AB')
  )
  expect_error(lidar_clip_and_stats('lidardb', sf_polygons, 'tururu', 'DBH'), "class lfcLiDAR")
})

test_that("external point_value wrapper works", {
  skip_on_cran()
  skip_on_travis()
  expect_identical(
    lidardb$point_value(sf_points, 'plot_id', 'AB'),
    lidar_point_value(lidardb, sf_points, 'plot_id', 'AB')
  )
  expect_error(lidar_point_value('lidardb', sf_points, 'plot_id', 'DBH'), "class lfcLiDAR")
})

rm(lidardb)
