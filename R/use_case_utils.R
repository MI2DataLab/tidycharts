library(magrittr) # pipes
library(docstring)
source(file.path("utils","drawing_utils.R"))
source(file.path("utils","chart_utils.R"))

initialize() %>% 
  draw_triangle(., 10,10,orientation = "bottom") %>% 
  finalize() %>% 
  show()

# save file :
# write(initialize() %>% 
#         draw_triangle(., 10,10,orientation = "left") %>% 
#         finalize())
