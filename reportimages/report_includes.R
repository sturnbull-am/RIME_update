# Some functions to make generating the website easier.

library(htmltools)
library(yaml)
library(stringr)
library(purrr)

ul <- tags$ul
li <- tags$li

callout <- function(title, div_class, ...) {
    # Create a random dialog of a certain class
    div(class = div_class,
        div(class = "panel-heading", 
			h3(class = "panel-title", title)
		),
        div(class = "panel-body", 
            ...
        )
    )
}


note <- function(title, ...) {
    # Create a note dialog
    return(callout(title, "panel panel-primary", ...))
}


# exercise <- function(title, ...) { 
#     # Create an exercise dialog
#     return(callout(paste("Exercise -", title), "panel panel-danger", ...))
# }
# 
# 
# footer <- function() {
#     # get which page is currently being made
#     args <- commandArgs()
#     filename <- str_extract(args[length(args)], "\\w+.Rmd")
#     target <- str_replace(filename, ".Rmd", ".html")
# 
#     # determine next page from _site.yml
#     site <- yaml.load_file('_site.yml')
#     menu <- site$navbar$right[[2]]$menu 
#     menu <- map_chr(menu, ~.x$href)
# 
#     # # indexing oob, naughty, I know...
#     # next_page <- menu[which(menu == target) + 1]
#     # if (is.na(next_page)) {
#     #     return(a(href="index.html", style="font-size:2em;", "Return to frontpage"))
#     # } else {
#     #     return(a(href = next_page, style = "font-size:2em;", "Next section"))
#     # }
# }

