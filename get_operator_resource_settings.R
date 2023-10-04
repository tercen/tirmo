library(dplyr)
library(tercen)
library(jsonlite)
library(gh)
library(gitcreds)

## Tercen settings
serviceUri <- Sys.getenv("TERCEN_SERVICE_URI")
username <- Sys.getenv("TERCEN_GITHUB_USER_USERNAME")
pwd <- Sys.getenv("TERCEN_GITHUB_USER_PASSWORD")
teamName <- Sys.getenv("TERCEN_OPERATOR_STATS_TEAM")
projectName <- Sys.getenv("TERCEN_OPERATOR_STATS_PROJECT")

committerName <- Sys.getenv("TERCEN_COMMITTER_NAME")
committerMail <- Sys.getenv("TERCEN_COMMITTER_EMAIL")

## Get operator stats from Tercen
client = TercenClient$new(
  serviceUri = serviceUri,
  username = username,
  password = pwd
)

projects = client$documentService$findProjectByOwnersAndName(
  startKey = list(teamName, projectName),
  endKey = list(teamName, projectName)
)

project = Find(function(p) identical(p$name, projectName), projects)

files <- client$projectDocumentService$findFileByLastModifiedDate(
  startKey = list(project$id, '2042'),
  endKey = list(project$id, ''),
  useFactory = TRUE
)

file_stats = Find(function(p) identical(p$name, "stats-summary.csv"), files)

bytes <- client$fileService$download(file_stats$id)
char <- rawToChar(bytes)
lns <- strsplit(char, "\n")[[1]]
sp <- sapply(lns, strsplit, "\t", USE.NAMES = FALSE)
lg <- lengths(sp, use.names = FALSE)
exclude_rows <- which(lg != 35)
if(length(exclude_rows) > 0) {
  lns <- lns[-exclude_rows]
}
df_raw <- read.table(text = lns, header = TRUE)

## Default resource settings
min_ratio <- 5
max_ratio <- 50
base_mem_default <- 0.5e9
base_mem_big <- 1.0e9
min_r_squared <- 0.7
min_observations <- 5
confint_level <- 0.98
seed <- 42

set.seed(seed)

df <- df_raw %>% 
  filter(operator_url != "") %>%
  mutate(total_size = files_size + qt_size + row_size + col_size) %>%
  rename(operator = operator_url) %>%
  mutate(base_memory = base_mem_default) %>%
  mutate(base_memory = case_when(operator_kind == "DockerOperator"  ~ base_mem_big,
                                 TRUE ~ base_mem_default)) %>%
  mutate(ratio = (memory_total_usage - base_memory) / total_size) %>% 
  mutate(ratio_nobm = (memory_total_usage) / total_size) %>% 
  select(operator, operator_version, task_duration, memory_total_usage, total_size, base_memory, ratio, row_size, col_size, files_size, qt_size)

do.lm <- function(df_tmp, confint_level) {
  
  ## Estimate memory usage
  m <- lm(memory_total_usage ~ total_size - 1, data = df_tmp)
  c_out <- coefficients(m)
  ci_out <- confint(m, level = confint_level)[, 2]
  r2 <- (summary(m)$r.squared)
  names(ci_out) <- paste0("upper", names(ci_out))
  
  ## Estimate task duration
  m <- lm(task_duration ~ total_size, data = df_tmp)
  c_out_dur <- coefficients(m)
  typical_duration <- median(df_tmp$task_duration, na.rm = TRUE)

  return(tibble(
    ratio_est = c_out,
    ratio_upper = ci_out,
    r2 = r2,
    typical_duration = round(typical_duration / 1e3, digits = 1)#,
    # duration_coef = round(c_out_dur[2], digits = 1), 
    # duration_intercept = round(c_out_dur[1], digits = 1)
  ))
}

df_out <- df %>% 
  mutate(memory_total_usage = (memory_total_usage - base_mem_default)) %>%
  group_by(operator, operator_version, base_memory) %>%
  filter(n() >= min_observations) %>%
  do(do.lm(., confint_level)) %>%
  rename(uri = operator) %>%
  rename(version = operator_version) %>%
  mutate(ratio_rounded = round(ratio_upper + 0.1, digits = 1)) %>%
  mutate(ratio = replace(ratio_rounded, which(ratio_rounded < min_ratio), min_ratio)) %>%
  mutate(ratio = replace(ratio, which(ratio > max_ratio), max_ratio)) %>%
  mutate(ratio = replace(ratio, which(r2 < min_r_squared), min_ratio)) %>%
  mutate(version = numeric_version(version, strict = FALSE)) %>%
  arrange(uri, version) %>%
  mutate(version = as.character(version)) %>%
  ungroup()
  

### Overwrite with custom settings
custom_json <- gh(
  "GET /repos/{owner}/{repo}/contents/{path}",
  owner = 'tercen',
  repo = 'app-library',
  path = 'operator_resource_settings_custom.json',
  .accept = 'application/vnd.github.v3+json',
  .token = Sys.getenv("GITHUB_TOKEN")
)
custom_dec <- base64_dec(custom_json$content)
custom_df <- jsonlite::fromJSON(rawToChar(custom_dec))

dups <- paste0(df_out$uri, df_out$version) %in% paste0(custom_df$uri, custom_df$version)
if(any(dups)) df_out <- df_out[!dups, ]

# df_out <- 
#   df_out %>% 
#   dplyr::full_join(custom_df)

txt_json <- prettify(toJSON(df_out))

### GH API -> replace json
previous_json <- gh(
  "GET /repos/{owner}/{repo}/contents/{path}",
  owner = 'tercen',
  repo = 'app-library',
  path = 'operator_resource_settings_auto.json',
  .accept = 'application/vnd.github.v3+json'
)

new_content <- jsonlite::base64_enc(txt_json) 
new_dec <- base64_dec(new_content)
prev_dec <- base64_dec(previous_json$content)

if(!identical(new_dec, prev_dec)) {
  gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner = 'tercen',
    repo = 'app-library',
    path = 'operator_resource_settings_auto.json',
    message = "Resource settings update",
    committer = list(name = committerName, email = committerMail),
    content = new_content,
    sha = previous_json$sha,
    .accept = 'application/vnd.github.v3+json',
    .token = Sys.getenv("GITHUB_TOKEN")
  )
}
