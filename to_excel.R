if (require(openxlsx)) {
    library(openxlsx)
} else {
    install.packages("openxlsx", dep = TRUE)
    library(openxlsx)
}

hs1 <- createStyle(
    fgFill = "#B607E2", halign = "CENTER",
    textDecoration = "bold", border = "Bottom"
)

wb <- createWorkbook("huki")

# creamos las hojas
addWorksheet(wb, "Resultados")
addWorksheet(wb, "Cuestionario")

setColWidths(wb, "Cuestionario", cols = 1:5, widths = "auto")
writeData(wb, "Cuestionario", pittsburg,
    borders = "surrounding",
    headerStyle = hs1, startRow = 1
)


setColWidths(wb, "Resultados", cols = 1:nrow(proc_fct), widths = "auto")
writeData(wb, "Resultados", proc_fct, borders = "surrounding", headerStyle = hs1, startRow = 1)

saveWorkbook(wb, "Pittsburg.xlsx", overwrite = TRUE, returnValue = FALSE)
rm(hs1, wb)

switch(Sys.info()[["sysname"]],
    Windows = {
        shell("Pittsburg.xlsx")
    },
    Linux = {
        system2(command = "open Pittsburg.xlsx")
    },
    Darwin = {
        system2(command = "open Pittsburg.xlsx")
    }
)
