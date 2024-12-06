# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(clustermq)

### Running on HPC
## Settings for clustermq
options(
  clustermq.scheduler = "slurm",
  clustermq.template = "./cmq.tmpl" # if using your own template
)

# Set target options:
tar_option_set(
  resources = tar_resources(
    clustermq = tar_resources_clustermq(template = list(
      job_name = "auto-sdms",
      per_cpu_mem = "5000mb",
      n_tasks = 1,
      per_task_cpus = 36,
      walltime = "20:00:00"
    ))
  )
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
tar_plan(
  # Load the required paths
    input_folders = list(
    ForestClim_12 = "E:/Output/SDM_test/belgium/tiles/ForestClim_12/",
    ForestClim_15 = "E:/Output/SDM_test/belgium/tiles/ForestClim_15/",
    ForestClim_05 = "E:/Output/SDM_test/belgium/tiles/ForestClim_05/",
    ForestClim_06 = "E:/Output/SDM_test/belgium/tiles/ForestClim_06/",
    cec = "E:/Output/SDM_test/belgium/tiles/cec/",
    clay = "E:/Output/SDM_test/belgium/tiles/clay/"
  ),
  tar_target(mdl_paths,
    list.files(
    "E:/SDMs/Stef_SDMs/Models/",
    full.names = TRUE
  )),
  # Make future species distributions
  tar_target(futureSDMs,
   predict_futSDM(input_folders, mdl_paths),
   pattern = map(mdl_paths)
   )
)

