library(jsonlite)
library(gh)
library(dplyr)

## read library report
report_url <- "https://raw.githubusercontent.com/tercen/app-library/master/library-report.json"
library_report <- jsonlite::fromJSON(report_url)

## Failed test operators

### Check existing GH Issue
existing_issues <- gh(
  "GET /repos/{owner}/{repo}/issues",
  owner = 'tercen',
  repo = 'app-library',
  milestone = '2',
  per_page = 100,
  .accept = 'application/vnd.github.v3+json',
  .token = Sys.getenv("GITHUB_TOKEN")
)
issue_titles <- unlist(lapply(existing_issues, "[[", "title"))


if(nrow(library_report$failed_operators) > 0) {
  for(op_id in seq_len(nrow(library_report$failed_operators))) {
    
    op <- library_report$failed_operators[op_id, ]
    issue_hash <- digest::digest(op, algo = "murmur32")
    op_name <- basename(op$url)
    
    ### Create new issue if different hash
    if(!any(grepl(issue_hash, issue_titles))) {
      message(op_name)
      
      info <- paste0("\nVersion: ", op$version, "\n\n```\n",op$error,"\n```\n")
      title_failed <- paste0("Failed test: ", op_name, " [", issue_hash, "]")
      body_failed <- paste0("The unit test associated with this operator has failed. If you would like this operator to be part of the App Library, the test must be fixed.\n",info,"\n\nNote: This issue has automatically been created by Tercen's operator sentinel.")
      
      gh(
        "POST /repos/{owner}/{repo}/issues",
        owner = 'tercen',
        repo = 'app-library',
        title = title_failed,
        body = body_failed,
        milestone = '2',
        labels = list("enhancement", "compliance"),
        .token = Sys.getenv("GITHUB_TOKEN")
      )
    }
    
  }
}

## No test and no release operators

### Check existing GH Issue
existing_issues <- gh(
  "GET /repos/{owner}/{repo}/issues",
  owner = 'tercen',
  repo = 'app-library',
  milestone = '1',
  page = '1',
  per_page = 100,
  .accept = 'application/vnd.github.v3+json',
  .token = Sys.getenv("GITHUB_TOKEN")
)
issue_titles <- unlist(lapply(existing_issues, "[[", "title"))


for(op in c(library_report$no_release_operators, library_report$notest_operators$url)) {
  issue_hash <- digest::digest(op, algo = "murmur32")
  op_name <- basename(op)
  
  ### Create new issue if different hash
  if(!any(grepl(issue_hash, issue_titles))) {
    message(op_name)
    
    title_norelease <- paste0("Add a unit test to ", op_name, " [", issue_hash, "]")
    body_norelease <- paste0("There is no unit test associated with this operator (",op,"). If you would like this operator to be part of the App Library, it must include [continuous integration workflows](https://tercen.github.io/developers_guide/continuous-integration.html).\n\nNote: This issue has automatically been created by Tercen's operator sentinel.")
  
    gh(
      "POST /repos/{owner}/{repo}/issues",
      owner = 'tercen',
      repo = 'app-library',
      title = title_norelease,
      body = body_norelease,
      milestone = '1',
      labels = list("enhancement", "compliance"),
      .token = Sys.getenv("GITHUB_TOKEN")
    )
  }

}

