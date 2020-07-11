build_article <- function(filename) {
  # set the base url so that it knows where to find stuff
  knitr::opts_knit$set(base.url = "/")
  
  # tell it that we'll be generating an md file
  knitr::render_markdown()
  
  # generate a directory name, where we'll be storing the figures for it
  d = gsub('^_|[.][a-zA-Z]+$', '', filename)
  
  # tell it where to store the figures and cache files
  knitr::opts_chunk$set(
    fig.path   = sprintf('figs/%s/', d),
    cache.path = sprintf('cache/%s/', d),
    
    # THIS IS CRITICAL! without this, it tries to take a screenshot instead of
    # using the js/css files. It took me **a lot of time** to figure this out
    screenshot.force = FALSE
  )
  
  # this is the path to the original file. WARNING: I assume your .Rmd files are
  # at /_source. If that's not the case, adjust this variable
  source = paste0('_R/', filename, '.Rmd')
  
  # this is where we want the md file
  dest = paste0('_posts/', filename, '.md')
  
  # actually knit it!
  knitr::knit(source, dest, quiet = TRUE, encoding = 'UTF-8', envir = .GlobalEnv)
  
  # store the dependencies where they belong
  brocks::htmlwidgets_deps(source)
}

