#' Add textbox
#' 
#' @param text Content of the text box. 
#' @param textcolor Color of the text. Default black. 
#' @param title Title of the text box. Default empty. 
#' @param titlecolor Color of the title. Default white.  
#' @param titleback Color of the background of the title. Default black. 
#' @param backcolor Color of the background of the text. Default white.  
#' @param rounded Describes how much the corner of the text box would be rounded. 
#' @param type Type of the textbox, typically info or warning. 
#' 
#' @importFrom knitr asis_output is_latex_output
#' @import glue
#' @export
rmd_textbox <- function(text, textcolor = "black", 
                        title = "", titlecolor = "black", titleback = "yellow", 
                        backcolor = "white", rounded = "1", type = "info") {
  if (is_latex_output()) {
    return(
      rmd_textbox_latex(text, textcolor, title, titlecolor, titleback, 
                        backcolor, rounded, type)
    )
  }
  else {
    return(
      rmd_textbox_html(text, textcolor, title, titlecolor, titleback, 
                        backcolor, rounded, type)
    )
  }
}

rmd_textbox_latex <- function(text, textcolor, title, titlecolor, titleback, 
                              backcolor, rounded, type) {
  if (type == "warning") {
    knitr::asis_output(paste0(
      glue("\\begin{tcolorbox}[width=\\textwidth,
                            colupper=(textcolor),
                            title=\\bcinterdit (title),
                            coltitle=(titlecolor),
                            colbacktitle=(titleback),
                            colback=(backcolor),
                            outer arc=(rounded)mm]
           (text) 
           \\end{tcolorbox}",
           .open = "(", .close = ")")
    ))
  } else {
    knitr::asis_output(paste0(
      glue("\\begin{tcolorbox}[width=\\textwidth,
                            colupper=(textcolor),
                            title=\\bcinfo (title),
                            coltitle=(titlecolor),
                            colbacktitle=(titleback),
                            colback=(backcolor),
                            outer arc=(rounded)mm]
           (text) 
           \\end{tcolorbox}",
           .open = "(", .close = ")")
    ))
  }
}

rmd_textbox_html <- function(text, textcolor, title, titlecolor, titleback, 
                              backcolor, rounded, type) {
  icon <- if (type == "warning") {
    '&#9940;'
  } else {
    '&#10026;'
  }
  title <- if (type == "warning") {
    'Warning!'
  } else {
    'Info:'
  }
  
  knitr::asis_output(paste0(
    glue('<head>
         <style>
         .alert {
         padding: 20px;
         background-color: [backcolor];
         color: [textcolor];
         }
         </style>
         </head>

         <body>
         <br>
         <div class="alert">
         <strong>',
         .open = "[", .close = "]"), 
    icon, 
    title, 
    glue('</strong> [text]
         </div>
         <br>
         </body>',
         .open = "[", .close = "]")
    ))
}
