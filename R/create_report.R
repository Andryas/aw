# #' @title Relátorio Kzas
# #'
# #' @description
# #' Função cria o fluxo de trabalho para desenvolvimento do relátorio
# #'
# #' @examples
# #'
# #' create_report()
# #' list.files()
# #'
# #' @export
# create_report <- function() {

#     pkg_resource = function(...) {
#         system.file(..., package = "keymaker")
#     }

#     css <- pkg_resource("templates/report/styles.css")
#     header <- pkg_resource("templates/report/header.html")

#     rmd <- pkg_resource("templates/report/report.Rmd")
#     images <- pkg_resource("templates/images")
#     images <- list.files(images, all.files = FALSE, full.names = TRUE)

#     if (!dir.exists("report")) {
#         dir.create("report")
#         setwd("report")

#         dir.create("css")
#         dir.create("data")
#         dir.create("images")
#         dir.create("includes")

#         invisible(file.copy(css, "css/"))
#         invisible(file.copy(images, "images/"))
#         invisible(file.copy(header, "includes/"))
#         invisible(file.copy(rmd, "."))
#     } else {
#         stop("Verifique se já não existe uma pasta chamada report no seu diretório.")
#     }
# }
