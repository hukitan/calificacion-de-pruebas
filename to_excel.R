if (require(openxlsx)) {
    library(openxlsx)
} else {
    install.packages("openxlsx", dep = TRUE)
    library(openxlsx)
}

hs1 <- createStyle(
    fgFill = "#cc71e2", halign = "CENTER",
    textDecoration = "bold", border = "Bottom"
)

wb <- createWorkbook("Alastor")

# creamos las hojas
addWorksheet(wb, "Resultados")
addWorksheet(wb, "Cuestionario")

setColWidths(wb, "Cuestionario", cols = 1:4, widths = "auto")
writeData(wb, "Cuestionario", pittsburg[-5],
    borders = "surrounding",
    headerStyle = hs1, startRow = 1
)


setColWidths(wb, "Resultados", cols = 1:nrow(proc_fct), widths = "auto")
writeData(wb, "Resultados", proc_fct, borders = "surrounding", headerStyle = hs1, startRow = 1)



switch(Sys.info()[["sysname"]],
    Windows = {
    saveWorkbook(wb, "Pittsburg.xlsx", overwrite = TRUE, returnValue = FALSE)
    shell("Pittsburg.xlsx")
    },
    Linux = {
    saveWorkbook(wb, "Pittsburg.xlsx", overwrite = TRUE, returnValue = FALSE)
    system2(command = "open", args = "Pittsburg.xlsx")
    },
    Darwin = {
        print("I'm a Mac.")
    }
)
rm(hs1, wb)