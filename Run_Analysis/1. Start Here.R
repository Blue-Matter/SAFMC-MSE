# This script sources the individual scripts for building the operating models
# and running the analyses


# 1. Build Base Case OMs
source('Run_Analysis/Build_Base_Case_OMs.R')

# 2. Build Sensitivity OMs
source('Run_Analysis/Build_Sensitivity_OMs.R')

# 3. Run Historical Simulations
source('Run_Analysis/Simulate_Historical_Fisheries.R')

# 4. Report Stock Status from OMs
source('Run_Analysis/Generate_Figures.R')

# 5. Create MPs
source('Run_Analysis/create_MPs.R')

# 6. Run Base Case Projections
source('Run_Analysis/Projections_Base_Case.R')

# 7. Run Sensitivity Projections
# source('Run_Analysis/Projections_Base_Case.R')

# 8. Calculate Performance
