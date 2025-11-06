# ---- 4) Build the spec grid and run SCA ------------------------
specs <- setup(
  data     = dat,
  y        = y_present,
  x        = x_var,
  model    = "glm",
  controls = c_present,
  subsets  = subsets_spec
  # (can add robust/cluster/fixed args here later if needed)
)

results <- specr(specs)

# ---- 5) Plot + save -------------------------------------------
p <- plot(results)   # default plot (curve + choices overview)
print(p)

saveRDS(results, "sca_results_full_glm.RDS")
ggplot2::ggsave("sca_plot_full_glm.png", p, width = 11, height = 7, dpi = 300)

message("SCA complete (full dataset only). Saved sca_results_full_glm.RDS and sca_plot_full_glm.png.")
