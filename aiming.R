# aiming

library(targets)
library(stantargets)

tar_manifest()

tar_visnetwork(targets_only = TRUE)

tar_make()

tar_read(example_summary_x)

tar_read(custom_summary)

print(n = 30, tar_read(postpred))

tar_load(postpred)
postpred

tar_load(model)
model
