extractor_codigo <- function(
    class_numbers = 1:4,
    source_base_dir = "Mod 2 - Intro a programacion/elaboracion_materiales",
    source_folder_pattern = "clase%d",
    source_file_pattern = "clase_%d.qmd",
    output_base_dir = "Mod 2 - Intro a programacion",
    output_file_pattern = "clase_%d.R",
    documentation = 0,
    verbose = TRUE
) {
  # Guardamos directorio de trabajo original
  original_dir <- getwd()
  on.exit(setwd(original_dir))
  
  # Definimos los nombres específicos de las carpetas de destino
  output_folders <- c(
    "1" = "Clase 1 - Intro a R",
    "2" = "Clase 2 - Intro a tidyverse",
    "3" = "Clase 3 - Tidyverse avanzado y visualizacion",
    "4" = "Clase 4 - Visualizacion avanzada"
  )
  
  # Rastrear archivos extraídos
  extracted_files <- character()
  
  # Procesar cada número de clase
  for (class_num in class_numbers) {
    # Verificamos si tenemos un nombre de carpeta para esta clase
    class_num_str <- as.character(class_num)
    if (!class_num_str %in% names(output_folders)) {
      warning("No hay carpeta de destino definida para la clase ", class_num)
      next
    }
    
    # Definir rutas para esta clase - ahora con la estructura de directorios completa
    source_folder <- file.path(source_base_dir, sprintf(source_folder_pattern, class_num))
    source_file <- sprintf(source_file_pattern, class_num)
    source_path <- file.path(source_folder, source_file)
    
    # Definir carpeta de destino usando los nombres específicos
    output_folder <- file.path(output_base_dir, output_folders[class_num_str])
    output_file <- sprintf(output_file_pattern, class_num)
    output_path <- file.path(output_folder, output_file)
    
    # Verificamos si existe la carpeta fuente
    if (!dir.exists(source_folder)) {
      warning("Carpeta fuente no encontrada: ", source_folder)
      next
    }
    
    # Verificamos si existe el archivo fuente
    if (!file.exists(source_path)) {
      warning("Archivo de clase no encontrado: ", source_path)
      next
    }
    
    # Asegurarse de que exista la carpeta de destino
    if (!dir.exists(output_folder)) {
      if (verbose) {
        cat("Creando carpeta de destino: ", output_folder, "\n")
      }
      dir.create(output_folder, recursive = TRUE)
    }
    
    if (verbose) {
      cat("Extrayendo", source_path, "a", output_path, "\n")
    }
    
    # Extraer código
    knitr::purl(
      input = source_path,
      output = output_path,
      documentation = documentation,
      quiet = !verbose
    )
    
    extracted_files <- c(extracted_files, output_path)
  }
  
  if (verbose) {
    cat("\nExtraídos", length(extracted_files), "scripts de R\n")
  }
  
  return(invisible(extracted_files))
}

extractor_codigo()