#not working

library(pscl)   # for hurdle()

# Custom hurdle model for specr
hurdle_nb <- function(formula, data) {
  pscl::hurdle(
    formula = formula,
    data    = data,
    dist    = "negbin",   # or "poisson"
    zero.dist = "binomial"
  )
}

# ---- 4) Build the spec grid and run SCA ------------------------
specs <- setup(
  data     = dat,
  y        = y_present,
  x        = x_var,
  model    = "hurdle_nb", #as defined above
  controls = c_present,
  subsets  = subsets_spec
  # (can add robust/cluster/fixed args here later if needed)
)

#lm vs hurdle attempt later
#specs <- setup(
#  data     = dat,
#  y        = y_present,
#  x        = x_var,
#  model    = c("lm", "hurdle_nb"),
#  controls = c_present,
#  subsets  = subsets_spec
#)



results <- specr(specs)

# ---- 5) Plot + save -------------------------------------------
p <- plot(results)   # default plot (curve + choices overview)
print(p)

saveRDS(results, "sca_results_full_hurd.RDS")
ggplot2::ggsave("sca_plot_full_hurd.png", p, width = 11, height = 7, dpi = 300)

message("SCA complete (full dataset only). Saved sca_results_full_hurd.RDS and sca_plot_full_glm.hurd.")


