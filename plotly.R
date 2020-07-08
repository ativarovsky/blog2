filename <- "your-blog-post.Rmd"
knit(input= filename,
     output=output,
     encoding= 'UTF-8')
htmlwidgets_deps(paste0(Sys.Date(),"-",filename), always = T)