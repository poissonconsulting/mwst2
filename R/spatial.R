#' Crop
#'
#' @param sp A Spatial object.
#' @param xmin A number indicating the minimum Easting of the bounding box.
#' @param xmax A number indicating the maximum Easting of the bounding box.
#' @param ymin A number indicating the minimum Northing of the bounding box.
#' @param ymax A number indicating the maximum Northing of the bounding box.
#' @export
crop <- function (sp, xmin = 1641000, xmax = 1647250, ymin = 613000, ymax = 624250) {
  e1 <- methods::as(raster::extent(xmin, xmax, ymin, ymax), 'SpatialPolygons')
  raster::projection(e1) <- raster::projection(sp)
  sp %<>% raster::intersect(e1)
  sp
}

fortify.SpatialPointsDataFrame <- function (x) {
  dplyr::bind_cols(as.data.frame(sp::coordinates(x)), x@data)
}

#' Add Layer to a Map
#'
#' @param sp A sp object.
#' @param label NULL or a string of a unique column to use for labelling
#' centroids.
#' @param ... Other arguments passed on to ggplot2::layer.
#' @return A list of ggplot2 layers.
#' @export
add_layer <- function(sp, label = NULL, ...) {

  assert_that(is.null(label) || is.string(label))

  centroid <- NULL
  if (is.string(label)) {
    centroid <- as.data.frame(rgeos::gCentroid(sp, byid = TRUE))
    centroid %<>% dplyr::rename_(.dots = list(long = ~x, lat = ~y))
    centroid %<>% dplyr::bind_cols(methods::slot(sp, "data"))
    return(ggplot2::geom_text(
      data = centroid, ggplot2::aes_string(x = "long / 1000", y = "lat / 1000", label = label),
      show.legend = FALSE, inherit.aes = FALSE, ...))
  }

  if(inherits(sp, "SpatialPolygons")) {
    sp %<>% fortify()

    hole <- list(...)
    hole$fill <- NULL
    hole$fill = "white"
    hole$show.legend = FALSE
    hole$data = dplyr::filter_(sp, ~hole)

    return(list(ggplot2::geom_polygon(data = dplyr::filter_(sp, ~!hole),
                                      show.legend = FALSE, ...),
                do.call(ggplot2::geom_polygon, hole)))
  }
  sp %<>% fortify()

  ggplot2::geom_point(data = sp, ggplot2::aes_string(x = "long / 1000", y = "lat / 1000"),
                      show.legend = FALSE, inherit.aes = FALSE, ...)
}

#' Map Layer
#'
#' @inheritParams add_layer
#' @return A ggplot2 object.
#' @export
map <- function(sp, label = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 package not installed")

  ggplot2::ggplot(data = NULL, ggplot2::aes_string(x = "long / 1000", y = "lat / 1000", group = "group")) +
    ggplot2::scale_x_continuous(name = "Easting (km)", label = scales::comma, expand = c(0,0)) +
    ggplot2::scale_y_continuous(name = "Northing (km)", label = scales::comma, expand = c(0,0)) +
    ggplot2::coord_fixed() +
    add_layer(sp, label = label, ...)
}
