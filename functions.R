##### Collection of custom functions

# Calculate a serial-dilution titration curve
titr <- function(top,fold,n){
  out <- c(top)
  for(i in 2:n){
    out <- c(out, out[i-1]/fold)
  }
  return(out)
}

# Clean FlowJo raw output CSV or xlsx file
read_mfi <- function(file, sheet = 1, skip = 0, n_max = Inf) {
  if (file.exists(file)==FALSE){
    stop('File does not exist.')
  }
  
  if(tools::file_ext(file) == 'xlsx') {
    x <- readxl::read_excel(file, sheet = sheet, skip = skip, n_max = n_max)
  } else if (tools::file_ext(file) == 'csv') {
    x <- read.csv2(file)
  } else {
    stop('File is not csv or Excel xlsx.')
  }
  
  x <- tibble::as_tibble(x)
  
  colnames(x)[1] <- gsub(':','', colnames(x)[1])
  
  if (x[nrow(x), 1] == 'SD' & x[nrow(x)-1, 1] == 'Mean') {
    x <- x[1:(nrow(x)-2),]
  }
  
  return(x)
  
}

# ggplot theming
theme_mpg <- function(){
 ggplot2::theme_bw() +
  ggplot2::theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'),
        panel.grid = element_line(color = 'grey95')) 
}