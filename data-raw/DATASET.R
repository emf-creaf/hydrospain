# Data used within the package.

# File names and the names where to find their gegraphic coordinates.
file_coordinates <- utils::read.csv2(".\\data-raw\\files_coordinates.csv", na.strings = "")

# Basin names and their URL equivalent.
basin_names <- utils::read.csv2(".\\data-raw\\basin_names.csv", na.strings = "")

# Save for internal use.
usethis::use_data(basin_names, file_coordinates, internal = TRUE, overwrite = TRUE)

