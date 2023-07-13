library(bupaR)
orderedplot <- function(data, id = 'patient')
{
  #
  # Needs adjusting for patient and handling as names of the data
  #
  mat <- data %>%
    filter_activity_frequency(percentage = 1.0) %>% 
    filter_trace_frequency(percentage = 1.0) %>%    
    process_matrix() 
  #
  # mat is the original object, we change the order of the labels
  #
  tmp <-data %>% group_by(patient) %>% mutate(good_ranks = order(order(time, decreasing=TRUE)))
  td <- tmp %>% group_by(handling) %>% summarise(m = mean(good_ranks)) %>% arrange(desc(m))
  fac <- lapply(td[,1], as.character)
  names(fac) <- NULL
  ord <- unlist(c('Start',fac))
  newma <- mat
  antecedent=factor(mat$antecedent, levels=ord) 
  consequent=factor(mat$consequent, levels=unlist(c(fac, 'End'))) 
  newma[,1] <-  antecedent
  newma[,2] <-  consequent
  newma
}
exampleplot <- orderedplot(patients)
plot(exampleplot)


library(ggplot2)   
patients %>%
  ggplot(aes(consequent, antecedent)) +
  geom_raster(aes(fill = n)) +
  geom_text(aes(label = n), color = "white", fontface = "bold")  +
  scale_fill_continuous_bupaR(name = "Absolute Frequency") +
  coord_flip() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p
p <- p + labs(x = "Consequent", y = "Antecedent")
p   


