library(ggplot2)
#ggsave("testing.pdf", plot = results, width = 6, height = 4)



spec_obj <- readRDS("sca_results_full.RDS")
library(specr)
plot(spec_obj)
spec_plot <- plot(spec_obj)
ggsave("specification_curve.pdf", plot = spec_plot, width = 30, height = 30)
