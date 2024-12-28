if (!require(openxlsx)) {
    install.packages("openxlsx", dep = TRUE)
    library(openxlsx)
}

# Crear estilo de encabezado
hs1 <- createStyle(
    fgFill = "#cc71e2", halign = "CENTER",
    textDecoration = "bold", border = "Bottom"
)

# Crear libro de trabajo
wb <- createWorkbook("Alastor")

# Función para configurar anchos de columna y escribir datos
escribir_datos <- function(hoja, datos, cols) {
    addWorksheet(wb, hoja)
    setColWidths(wb, hoja, cols = cols, widths = "auto")
    writeData(wb, hoja, datos, borders = "surrounding", headerStyle = hs1, startRow = 1)
}

# Escribir datos en las hojas
escribir_datos("Resultados", proc_fct, 1:nrow(proc_fct)) # nolint: seq_linter.
escribir_datos("Cuestionario", pittsburg[-5], 1:4)

# Guardar y abrir el libro de trabajo según el sistema operativo
saveWorkbook(wb, "Pittsburg.xlsx", overwrite = TRUE, returnValue = FALSE)
switch(Sys.info()[["sysname"]],
    Windows = shell("Pittsburg.xlsx"),
    Linux = print("Archivo generado"),
    #Se cambia por print para que no se abra el archivo en caso de Linux (pensando en un docker)
    Darwin = system2(command = "open", args = "Pittsburg.xlsx")
)

# Eliminar variables temporales
rm(hs1, wb, escribir_datos)