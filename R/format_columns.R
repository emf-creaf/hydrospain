#' Title
#'
#' @param df 
#' @param table 
#'
#' @return
#' @export
#'
#' @examples
format_columns <- function(df, table) {
  
  # Different possibilities.
  formcol <- switch(table,
                    afliq = c(rep("character", 2), rep("numeric", 2)),
                    afliqc = c(rep("character", 2), rep("numeric", 2)),
                    afliqci = c(rep("character", 2), rep("numeric", 3)),
                    afliqe = c(rep("character", 2), rep("numeric", 3)),
                    afliqi = c(rep("character", 2), rep("numeric", 3)),
                    anual_a = c(rep("character", 2), rep("numeric", 19)),
                    anual_c = c(rep("character", 2), rep("numeric", 20)),
                    anual_e = c(rep("character", 2), rep("numeric", 6)),
                    canal = c(rep("character", 2), rep("numeric", 19), "character", rep("numeric", 3)),
                    
                    
                    estaf = c(rep("character", 3), rep("numeric", 19), rep("character", 5), rep("numeric", 2), "character"),
                    
                    
                    mensual_a = c(rep("character", 2), rep("numeric", 10)))
  
  
  # Format with set_colmode
  df <- df |> set_colmode(formcol)
  
  return(df)
}