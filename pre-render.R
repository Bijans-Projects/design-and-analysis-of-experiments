# Quarto Project Pre-Render Script

## TASK 1: Use values already defined in DESCRIPTION

### DO NOT CHANGE
### Source of truth: DESCRIPTION
descriptionFile <- read.dcf("DESCRIPTION")
descriptionConfig <- as.list(as.data.frame(descriptionFile))
### Quarto Project Config File
quartoConfig <- yaml::read_yaml("_quarto.yml")

### CHANGE THESE AS NEEDED
quartoConfig$website$title <- descriptionConfig$Title

### DO NOT CHANGE
### Write back to _quarto.yml
yaml::write_yaml(quartoConfig, "_quarto.yml")