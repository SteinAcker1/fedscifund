library(fedscifund)

# query the API for Iowa grants containing the keyword "neuroscience" for all fields
df <- query_nsf_api(
  printFields = "all",
  perfStateCode = "IA",
  keyword = "neuroscience"
  ) 

# run the same query as above but only retrieve title and abstract
df <- query_nsf_api(
  printFields = c("title", "abstractText"), 
  perfStateCode = "IA", 
  keyword = "neuroscience"
  ) 

# run the above code but export results to Excel without creating an R object
query_nsf_api(
  printFields = c("title", "abstractText"), 
  export_path = "ia_neuro.xlsx", 
  export_fmt = "excel", 
  r_output = FALSE, 
  perfStateCode = "IA", 
  keyword = "neuroscience"
  )