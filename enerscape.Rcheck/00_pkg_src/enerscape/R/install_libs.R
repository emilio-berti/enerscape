# to_inst <- c("raster", "sf", "car", "lme4", "nlme",
#              "magrittr", "ggExtra", "cowplot", "progress",
#              "plotly", "rgbif", "see", "segmented",
#              "doParallel", "foreach", "dismo", "knitr")
# installed <- list.files("~/R/x86_64-pc-linux-gnu-library/4.0")
# to_inst <- setdiff(to_inst, installed)
# if (length(to_inst) == 0) {
#   message("All packages already installed")
# } else {
#
#   for (x in to_inst) {
#     install.packages(x)
#   }
#
#   installed <- list.files("~/R/x86_64-pc-linux-gnu-library/4.0")
#   for (x in setdiff(to_inst, installed)) {
#     warning("Package ", x, " not installed",
#             immediate. = TRUE)
#   }
# }
