
# functions ---------------------------------------------------------------

add_model_function_for_class <-
  function(class) {
    template_lines <-
      readLines('tools/blms_model_function_template.R')
    for (i in 1:length(template_lines)) {
      template_lines[i] <- gsub('MODELCLASS', class, template_lines[i])
    }
    usethis::use_r(class, open = F)
    class_file <- paste0('R/', class, '.R')
    writeLines(template_lines, class_file)
  }



# create files ------------------------------------------------------------


# add_model_function_for_class('ql_a_2it')

for (class in names(get_all_model_specs())) {
  add_model_function_for_class(class)
}
