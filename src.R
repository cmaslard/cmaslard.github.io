# pkg 
library(rcrossref)
library(rAltmetric) #devtools::install_github("ropensci/rAltmetric")
library(scholar)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(glue)
library(htmltools)
library(stringr)
library(readr)

# function
# Function to generate the Altmetric HTML snippet
generate_altmetric_html <- function(doi_x) {
  html_output = ""
  if (!is.na(doi_x)){
    # URL and image generation using the provided DOI
    url <- altmetric_data(altmetrics(doi = doi_x,apikey = ""))$details_url
    img <- altmetric_data(altmetrics(doi = doi_x,apikey = ""))$images.medium
    
    # Create the HTML code
    html_output <- HTML(glue(
      '<a href="{url}" target="_blank"><img src="{img}" width="64" height="64"></a>'
    ))
  }
  return(html_output)
}

# cosmetic
## Dark_mode also for graph (i add image-dark-light.js from https://github.com/quarto-dev/quarto-cli/discussions/5439)
library(ggdark)
library(devoid)
darksvg = function(file, width, height) {
  on.exit({
    ggplot2::reset_theme_settings()
    # invert_geom_defaults()
  }
  )
  theme_set(dark_mode(theme_get()))
  ggsave(filename = file, width = width, height=height, dev='svg', bg = 'transparent')
  void_dev()
}

############## function that make list of publication ####################
get_pubs <- function() {
  pubs <- gsheet::gsheet2tbl(
    url = 'https://docs.google.com/spreadsheets/d/1Cu1twyhPRdxY3Q4HBF6qmOK7BI_8a4qRav_IWI2n4eA')
  pubs <- make_citations(pubs)
  pubs$stub <- make_stubs(pubs)
  pubs$url_scholar <- ifelse(
    is.na(pubs$id_scholar), NA, 
    glue::glue('https://scholar.google.com/citations?view_op=view_citation&hl=fr&user=TsztyiMAAAAJ&citation_for_view=TsztyiMAAAAJ:{pubs$id_scholar}')
  )
  return(pubs)
}

get_presentations <- function() {
  presentations <- gsheet::gsheet2tbl(
    url = "https://docs.google.com/spreadsheets/d/1Cu1twyhPRdxY3Q4HBF6qmOK7BI_8a4qRav_IWI2n4eA/edit?gid=1523743739#gid=1523743739")
  presentations <- make_talks(presentations)
  #pubs$stub <- make_stubs(presentations)
  presentations$url_scholar <- ifelse(
    is.na(presentations$id_scholar), NA,
    glue::glue('https://scholar.google.com/citations?view_op=view_citation&hl=fr&user=TsztyiMAAAAJ&citation_for_view=TsztyiMAAAAJ:{presentations$id_scholar}')
  )
  return(presentations)
}

make_citations <- function(pubs) {
  pubs$citation <- unlist(lapply(split(pubs, 1:nrow(pubs)), make_citation))
  pubs$icon <- unlist(lapply(split(pubs, 1:nrow(pubs)), make_icon))
  return(pubs)
}

make_talks <- function(presentations) {
  presentations$citation <- unlist(lapply(split(presentations, 1:nrow(presentations)), make_talk))
  presentations$icon <- unlist(lapply(split(presentations, 1:nrow(presentations)), make_icon))
  return(presentations)
}

icon_open <- '<iconify-icon icon="mdi:unlocked-variant-outline" width="1.2em" height="1.2em"  style="color: #58a53b"></iconify-icon>'
icon_under_review <- '<iconify-icon icon="line-md:cog-loop" width="1.2em" height="1.2em" style="color: #58a53b"></iconify-icon>'
icon_under_construction <- '<iconify-icon icon="material-symbols:construction-rounded" width="1.2em" height="1.2em" style="color: #58a53b"></iconify-icon>'

make_citation <- function(pub) {
  if (!is.na(pub$journal)) {
    if (pub$journal=="Under review") {
      pub$journal <- glue::glue(icon_under_review)
    }else if(pub$journal=="Working paper"){
      pub$journal <- glue::glue(icon_under_construction)
    }else{
      pub$journal <- glue::glue('_{pub$journal}_.')
    }
  }
  if (!is.na(pub$open)) {
    if(pub$open=="TRUE"){
      pub$open <- glue::glue(icon_open)
    }else{pub$open=""}
  }
  
  if (!is.na(pub$number)) {
    pub$number <- glue::glue('{pub$number}.')
  }
  if (!is.na(pub$doi)) {
    pub$ncit <- cr_citation_count(doi=pub$doi)$count
    if(!is.na(pub$ncit)){
      if(pub$ncit == 0){
        pub$ncit <- glue::glue("")
      }
      else if(pub$ncit == 1){
        pub$ncit <- glue::glue("({pub$ncit} Citation)")
      }else{
        pub$ncit <- glue::glue("(**{pub$ncit}** Citations)")
      }
    }else{
      pub$ncit <- glue::glue("")
    }
  }
  if (!is.na(pub$doi)) {
    pub$doi <- make_doi(pub$doi)
  }else if(!is.na(pub$url_pub)){
    pub$doi <- make_url(pub$url_pub)
  }
  if (!is.na(pub$url_pdf)) {
    pub$url_pdf <- make_pdf(pub$url_pdf)
  }
  if (!is.na(pub$url_repo)) {
    pub$url_repo <- make_repo(pub$url_repo)
  }
  if (!is.na(pub$url_rg)) {
    pub$url_rg <- make_researchgate(pub$url_rg)
  }
  if (!is.na(pub$id_scholar)) {
    pub$id_scholar <- make_scholar(pub$id_scholar)
  }
  pub$year <- glue::glue("{pub$year}|")
  pub$title <- glue::glue('"{pub$title}"')
  pub[,which(is.na(pub))] <- ''
  return(paste(
    #pub$title,
    pub$year, pub$author, pub$journal, pub$number, pub$ncit, pub$open #,
    #pub$doi, pub$url_pdf, pub$url_repo, pub$id_scholar, pub$url_rg
  ))
}

make_talk <- function(presentation) {
  if (!is.na(presentation$host)) {
      presentation$host <- glue::glue('_{presentation$host}_.')
  }
  if (!is.na(presentation$location)) {
    presentation$location <- glue::glue("**{presentation$location}**")
  }
  if (!is.na(presentation$open)) {
    if(presentation$open=="TRUE"){
      presentation$open <- glue::glue(icon_open)
    }else{presentation$open=""}
  }
  if (!is.na(presentation$url_pdf)) {
    presentation$url_pdf <- make_pdf(presentation$url_pdf)
  }
  if (!is.na(presentation$url_rg)) {
    presentation$url_rg <- make_researchgate(presentation$url_rg)
  }
  if (!is.na(presentation$id_scholar)) {
    presentation$id_scholar <- make_scholar(presentation$id_scholar)
  }
  presentation$year <- glue::glue("{presentation$year}|")
  presentation$title <- glue::glue('"{presentation$title}"')
  presentation[,which(is.na(presentation))] <- ''
  return(paste(
    #pub$title,
    presentation$year, presentation$author, presentation$host, presentation$location,  presentation$open #,
    #pub$doi, pub$url_pdf, pub$url_repo, pub$id_scholar, pub$url_rg
  ))
}

make_icon <- function(pub) {
  if (!is.na(pub$open)) {
    if(pub$open=="TRUE"){
      pub$open <- glue::glue(icon_open)
    }else{pub$open=""}
  }
  if (!is.na(pub$doi)) {
    pub$doi <- make_doi(pub$doi)
  }else if(!is.na(pub$url_pub)){
    pub$doi <- make_url(pub$url_pub)
  }
  if (!is.na(pub$url_pdf)) {
    pub$url_pdf <- make_pdf(pub$url_pdf)
  }
  if (!is.na(pub$url_repo)) {
    pub$url_repo <- make_repo(pub$url_repo)
  }
  if (!is.na(pub$url_rg)) {
    pub$url_rg <- make_researchgate(pub$url_rg)
  }
  if (!is.na(pub$url_preprint)) {
    pub$url_preprint <- make_preprint(pub$url_preprint)
  }
  if (!is.na(pub$id_scholar)) {
    pub$id_scholar <- make_scholar(pub$id_scholar)
  }
  if (!is.na(pub$url_hal)) {
    pub$url_hal <- make_hal(pub$url_hal)
  }
  pub[,which(is.na(pub))] <- ''
  return(paste(
    pub$doi, pub$url_pdf, pub$url_repo, pub$url_preprint, pub$id_scholar, pub$url_rg, pub$url_hal
  ))
}

make_doi <- function(doi) {
  return(glue::glue(
    '<a href="https://doi.org/{doi}" target="_blank" style="text-decoration: none; display: inline-flex; align-items: center;">
      <svg xmlns="http://www.w3.org/2000/svg" width="1.2em" height="1.2em" viewBox="0 0 24 24" style="margin-right: 5px;">
        <path fill="none" stroke="currentColor" stroke-dasharray="28" stroke-dashoffset="28" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 6l2 -2c1 -1 3 -1 4 0l1 1c1 1 1 3 0 4l-5 5c-1 1 -3 1 -4 0M11 18l-2 2c-1 1 -3 1 -4 0l-1 -1c-1 -1 -1 -3 0 -4l5 -5c1 -1 3 -1 4 0">
          <animate fill="freeze" attributeName="stroke-dashoffset" dur="0.6s" values="28;0"/>
        </path>
      </svg>
    </a>'
  ))
}

make_url <- function(url_pub) {
  return(glue::glue(
    '<a href="{url_pub}" target="_blank" style="text-decoration: none; display: inline-flex; align-items: center;">
      <svg xmlns="http://www.w3.org/2000/svg" width="1.2em" height="1.2em" viewBox="0 0 24 24" style="margin-right: 5px;">
        <path fill="none" stroke="currentColor" stroke-dasharray="28" stroke-dashoffset="28" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 6l2 -2c1 -1 3 -1 4 0l1 1c1 1 1 3 0 4l-5 5c-1 1 -3 1 -4 0M11 18l-2 2c-1 1 -3 1 -4 0l-1 -1c-1 -1 -1 -3 0 -4l5 -5c1 -1 3 -1 4 0">
          <animate fill="freeze" attributeName="stroke-dashoffset" dur="0.6s" values="28;0"/>
        </path>
      </svg>
    </a>'
  ))
}

make_pdf <- function(url_pdf) {
  return(glue::glue(
    '<a href="{url_pdf}" target="_blank" style="text-decoration: none; display: inline-flex; align-items: center;">
      <svg xmlns="http://www.w3.org/2000/svg" width="1.2em" height="1.2em" viewBox="0 0 24 24" style="margin-right: 5px;">
        <g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2">
          <path fill="currentColor" fill-opacity="0" stroke-dasharray="20" stroke-dashoffset="20" d="M12 4h2v6h2.5l-4.5 4.5M12 4h-2v6h-2.5l4.5 4.5">
            <animate attributeName="d" begin="0.5s" dur="1.5s" repeatCount="indefinite" values="M12 4h2v6h2.5l-4.5 4.5M12 4h-2v6h-2.5l4.5 4.5;M12 4h2v3h2.5l-4.5 4.5M12 4h-2v3h-2.5l4.5 4.5;M12 4h2v6h2.5l-4.5 4.5M12 4h-2v6h-2.5l4.5 4.5"/>
            <animate fill="freeze" attributeName="fill-opacity" begin="0.7s" dur="0.5s" values="0;1"/>
            <animate fill="freeze" attributeName="stroke-dashoffset" dur="0.4s" values="20;0"/>
          </path>
          <path stroke-dasharray="14" stroke-dashoffset="14" d="M6 19h12">
            <animate fill="freeze" attributeName="stroke-dashoffset" begin="0.5s" dur="0.2s" values="14;0"/>
          </path>
        </g>
      </svg>
    </a>'
  ))
}

make_repo <- function(url_repo) {
  return(glue::glue(
    '<a href="{url_repo}" target="_blank" style="text-decoration: none; display: inline-flex; align-items: center;">
      <svg xmlns="http://www.w3.org/2000/svg" width="1.2em" height="1.2em" viewBox="0 0 24 24" style="margin-right: 5px;">
        <mask id="lineMdGithubLoop0" width="24" height="24" x="0" y="0">
          <g fill="#fff">
            <ellipse cx="9.5" cy="9" rx="1.5" ry="1"/>
            <ellipse cx="14.5" cy="9" rx="1.5" ry="1"/>
          </g>
        </mask>
        <g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2">
          <path stroke-dasharray="32" stroke-dashoffset="32" d="M12 4c1.67 0 2.61 0.4 3 0.5c0.53 -0.43 1.94 -1.5 3.5 -1.5c0.34 1 0.29 2.22 0 3c0.75 1 1 2 1 3.5c0 2.19 -0.48 3.58 -1.5 4.5c-1.02 0.92 -2.11 1.37 -3.5 1.5c0.65 0.54 0.5 1.87 0.5 2.5c0 0.73 0 3 0 3M12 4c-1.67 0 -2.61 0.4 -3 0.5c-0.53 -0.43 -1.94 -1.5 -3.5 -1.5c-0.34 1 -0.29 2.22 0 3c-0.75 1 -1 2 -1 3.5c0 2.19 0.48 3.58 1.5 4.5c1.02 0.92 2.11 1.37 3.5 1.5c-0.65 0.54 -0.5 1.87 -0.5 2.5c0 0.73 0 3 0 3">
            <animate fill="freeze" attributeName="stroke-dashoffset" dur="0.7s" values="32;0"/>
          </path>
          <path stroke-dasharray="10" stroke-dashoffset="10" d="M9 19c-1.406 0-2.844-.563-3.688-1.188C4.47 17.188 4.22 16.157 3 15.5">
            <animate attributeName="d" dur="3s" repeatCount="indefinite" values="M9 19c-1.406 0-2.844-.563-3.688-1.188C4.47 17.188 4.22 16.157 3 15.5;M9 19c-1.406 0-3-.5-4-.5-.532 0-1 0-2-.5;M9 19c-1.406 0-2.844-.563-3.688-1.188C4.47 17.188 4.22 16.157 3 15.5"/>
            <animate fill="freeze" attributeName="stroke-dashoffset" begin="0.8s" dur="0.2s" values="10;0"/>
          </path>
        </g>
        <rect width="8" height="4" x="8" y="11" fill="#000" mask="url(#lineMdGithubLoop0)">
          <animate attributeName="y" dur="10s" keyTimes="0;0.45;0.46;0.54;0.55;1" repeatCount="indefinite" values="11;11;7;7;11;11"/>
        </rect>
      </svg>
    </a>'
  ))
}

make_researchgate <- function(url_rg) {
  return(glue::glue(
    '<a href="{url_rg}" target="_blank" style="text-decoration: none; display: inline-flex; align-items: center;">
      <span style="display: inline-flex; align-items: center; margin-right: 5px;">
        <svg xmlns="http://www.w3.org/2000/svg" width="1em" height="1.2em" viewBox="0 0 384 512">
          <path fill="currentColor" d="M228.66 408.101c-20.194-21.973-47.218-57.268-69.698-97.166c37.134-8.685 64.633-43.55 64.633-78.384c0-51.345-39.88-75.176-92.163-75.176c-27.023 0-48.583 1.365-68.716 1.365c-18.369 0-36.722 0-48.154-.445V171.6l17.401 3.192c11.97 2.302 18.783 7.765 18.783 36.214v180.6c0 28.435-6.813 33.928-18.783 36.2l-17.4 3.252v13.259c12.367-.445 33.912-1.351 55.473-1.351c20.624 0 47.217.906 58.68 1.35v-13.258l-23.847-3.253c-12.366-1.796-18.813-7.764-18.813-36.2v-76.542c11.002.921 20.624.921 35.325.921c27.96 49.95 54.551 87.56 69.652 104.962c13.78 16.526 34.85 27.054 61.442 27.054c7.781 0 16.023-1.367 21.054-3.683v-11.894c-16.496 0-32.992-11.477-44.87-24.321zM119.064 295.344c-15.591 0-22.434-.414-33.008-1.41V178.918c10.574-.92 24.752-.92 37.136-.92c38.531 0 61.427 20.195 61.427 56.839c0 36.215-24.736 60.506-65.555 60.506M258.998 179.64c-.46-2.409-.875-5.217-1.243-8.456c-.383-3.268-.63-7.104-.782-11.63c-.154-4.496-.215-9.99-.215-16.282c0-6.323.061-11.74.215-16.25c.152-4.528.399-8.41.782-11.648c.368-3.223.782-6.046 1.243-8.455a51 51 0 0 1 1.78-6.982c3.943-11.923 10.405-20.885 19.443-26.901C289.244 67.02 300.308 64 313.443 64c6.752 0 12.875.782 18.307 2.377c5.371 1.581 10.189 3.76 14.44 6.553c4.189 2.762 7.81 6.015 10.802 9.698a53 53 0 0 1 7.474 11.878c.75 1.35.537 2.425-.66 3.176l-16.68 6.858c-1.413.753-2.41.37-3.193-1.12c-3.743-6.936-6.936-11.493-12.183-14.807c-5.31-3.3-10.22-4.865-18.308-4.865c-8.793 0-12.721 1.749-18.23 5.693c-5.463 3.867-9.468 8.732-11.832 16.143c-.474 1.335-.905 2.993-1.41 4.942c-.415 1.98-.753 4.404-.967 7.242c-.215 2.84-.415 6.353-.598 10.497c-.123 4.144-.184 9.177-.184 15.008c0 5.86.061 10.894.184 15.038c.184 4.128.383 7.641.598 10.48c.214 2.87.552 5.279.966 7.274c.507 1.919.937 3.575 1.411 4.927c2.364 7.38 5.74 11.415 10.712 14.654c4.911 3.284 10.557 5.648 19.35 5.648c7.811 0 14.962-2.225 19.626-5.618c4.62-3.39 8.456-7.87 10.175-13.994c.753-2.579 1.72-5.786 2.38-9.714c.598-3.929.598-8.087.598-13.825c0-.907-.508-1.367-1.352-1.367h-26.716c-1.504 0-2.24-.736-2.24-2.24v-15.314c0-1.52.736-2.257 2.24-2.257h49.028c1.535 0 2.257.737 2.257 2.257v13.09c0 6.935 0 13.365-.722 19.32c-.691 5.953-1.626 11.109-2.808 14.868c-3.744 11.77-9.682 20.15-18.782 26.394c-9.131 6.291-20.9 9.682-33.684 9.682c-13.135 0-24.199-3.022-33.221-9.022c-9.039-6.077-15.5-14.993-19.443-26.916a51 51 0 0 1-1.78-6.997z"/>
        </svg>
      </span>
    </a>'
  ))
}

make_scholar <- function(id_scholar) {
  return(glue::glue(
    '<a href="https://scholar.google.com/citations?view_op=view_citation&hl=fr&user=TsztyiMAAAAJ&citation_for_view=TsztyiMAAAAJ:{id_scholar}" target="_blank" style="text-decoration: none; display: inline-flex; align-items: center;">
      <svg xmlns="http://www.w3.org/2000/svg" width="1.2em" height="1.2em" viewBox="0 0 384 512" style="margin-right: 5px;">
        <path fill="currentColor" d="M343.759 106.662V79.43L363.524 64h-213.89L20.476 176.274h85.656a82 82 0 0 0-.219 6.225c0 20.845 7.22 38.087 21.672 51.861c14.453 13.797 32.252 20.648 53.327 20.648c4.923 0 9.75-.368 14.438-1.024c-2.907 6.5-4.374 12.523-4.374 18.142c0 9.875 4.499 20.43 13.467 31.642c-39.234 2.67-68.061 9.732-86.437 21.163c-10.531 6.5-19 14.704-25.39 24.531c-6.391 9.9-9.578 20.515-9.578 31.962c0 9.648 2.062 18.336 6.219 26.062c4.156 7.726 9.578 14.07 16.312 18.984c6.718 4.968 14.469 9.101 23.219 12.469c8.734 3.344 17.406 5.718 26.061 7.062A167 167 0 0 0 180.555 448c13.469 0 26.953-1.734 40.547-5.187c13.562-3.485 26.28-8.642 38.171-15.493c11.86-6.805 21.515-16.086 28.922-27.718c7.39-11.68 11.094-24.805 11.094-39.336c0-11.016-2.25-21.039-6.75-30.14c-4.468-9.073-9.938-16.542-16.452-22.345c-6.501-5.813-13-11.155-19.516-15.968c-6.5-4.845-12-9.75-16.468-14.813c-4.485-5.046-6.735-10.054-6.735-14.984c0-4.921 1.734-9.672 5.216-14.265c3.455-4.61 7.674-9.048 12.61-13.306c4.937-4.25 9.875-8.968 14.796-14.133c4.922-5.147 9.141-11.827 12.61-20.008c3.485-8.18 5.203-17.445 5.203-27.757c0-13.453-2.547-24.46-7.547-33.314c-.594-1.022-1.218-1.803-1.875-3.022l56.907-46.672v17.119c-7.393.93-6.624 5.345-6.624 10.635V245.96c0 5.958 4.875 10.834 10.834 10.834h3.989c5.958 0 10.833-4.875 10.833-10.834V117.293c0-5.277.778-9.688-6.561-10.63m-107.36 222.48c1.14.75 3.704 2.78 7.718 6.038c4.05 3.243 6.797 5.695 8.266 7.414a444 444 0 0 1 6.376 7.547c2.813 3.375 4.718 6.304 5.718 8.734q1.5 3.717 3.047 8.946a38.3 38.3 0 0 1 1.485 10.562c0 17.048-6.564 29.68-19.656 37.859c-13.125 8.18-28.767 12.274-46.938 12.274c-9.187 0-18.203-1.093-27.063-3.196c-8.843-2.116-17.311-5.336-25.39-9.601c-8.078-4.258-14.577-10.204-19.5-17.797c-4.938-7.64-7.407-16.415-7.407-26.25c0-10.32 2.797-19.29 8.422-26.906c5.594-7.625 12.938-13.391 22.032-17.315c9.063-3.946 18.25-6.742 27.562-8.398a158 158 0 0 1 28.438-2.555c4.47 0 7.936.25 10.405.696c.455.219 3.032 2.07 7.735 5.563c4.704 3.462 7.625 5.595 8.75 6.384zm-3.359-100.579c-7.406 8.86-17.734 13.288-30.953 13.288c-11.86 0-22.298-4.764-31.266-14.312c-9-9.523-15.422-20.328-19.344-32.43c-3.937-12.109-5.906-23.984-5.906-35.648q.002-20.54 10.781-34.976c7.187-9.65 17.5-14.485 30.938-14.485c11.875 0 22.374 5.038 31.437 15.157c9.094 10.085 15.61 21.413 19.517 33.968c3.922 12.54 5.873 24.53 5.873 35.984c0 13.446-3.702 24.61-11.076 33.454z"/>
      </svg>
    </a>'
  ))
}

make_preprint <- function(url_preprint) {
  return(glue::glue(
    '<a href="{url_preprint}" target="_blank" style="text-decoration: none; display: inline-flex; align-items: center;">
      <span style="display: inline-flex; align-items: center; margin-right: 5px;">
        <svg xmlns="http://www.w3.org/2000/svg" width="1em" height="1em" viewBox="0 0 512 512">
          <path fill="currentColor" d="M396.317 272.785c7.388-9.691 14.778-17.97 20.564-27.25c15.465-24.803 39.678-64.238 45.3-75.09c2.87-5.807 7.053-9.925 9.695-10.18c2.645-.254 6.02-.346 8.877 1.438c2.857 1.787 5.052 6.099 3.052 14.545c-2.001 8.446-3.15 11.519-5.92 18.026c-16.22 35.495-40.017 66.068-65.032 95.639c-4.719 5.579-4.927 9.664-2.283 15.977c11.289 26.96 21.872 54.211 33.08 81.205c3.727 8.973 8.332 17.61 12.933 26.186c4.65 8.673 7.974 13.257 22.098 13.067c5.82-.02 16.882-1.572 17.818 6.852c1.024 9.207-10.156 10.579-16.82 11.938c-7.521 1.533-11.308 2.119-19.002 2.137c-17.738.038-38.699-6.75-47.382-22.507c-8.889-16.13-13.686-27.892-21.841-49.924c-5.353-14.464-11.365-28.7-17.66-44.791c-3.544 3.804-6.88 6.457-9.043 9.857c-26.847 36.397-42.436 65.84-54.46 86.511c-9.48 16.296-10.274 23.496-21.533 21.144c-10.266-4.196-7.303-17.972-6.495-21.462c4.763-20.569 19.837-40.775 31.583-58.536c13.129-19.853 29.268-37.697 43.654-56.748c2.13-2.82 3.593-8.191 2.382-11.172c-12.898-31.736-26.31-63.268-39.771-94.775c-7.322-15.62-12.301-25.58-41.097-24.497c-4.763-.433-7.652-1.688-7.574-7.187c0-6.67 4.503-9.037 7.803-10.016c32.536-9.655 65.227-6.812 82.16 29.56c10.49 22.527 19.777 45.611 29.629 68.437c1.449 3.346 3.019 6.638 5.286 11.615zM15.435 356.443c7.568-5.679 12.84-11.083 15.82-16.224c2.973-5.134 4.462-11.356 4.462-18.657V101.316q-.001-9.325-3.82-18.253a197 197 0 0 0-5.099-7.706Q24.252 71.708 17.464 64h100.59c32.715 0 58.338 6.63 76.864 19.875c18.517 13.253 27.783 31.638 27.783 55.164c0 18.391-6.225 33.805-18.658 46.239c-7.035 7.034-13.93 12.168-20.687 15.412c-6.762 3.245-17.44 6.63-32.043 10.14l27.174 36.098q8.519 11.36 23.12 27.988q16.22 18.254 34.882 37.316q16.626 17.445 26.162 25.35c6.35 5.274 16.022 11.561 29.002 18.86h-65.707q-16.23.002-34.884-18.658q-21.904-22.303-44.616-54.757q-15.417-20.684-27.176-38.127c-6.223-9.19-15.148-23.12-26.771-41.779q76.254-10.544 76.254-59.219c0-30.281-21.497-45.427-64.492-45.427h-16.22v222.219q0 11.786 4.258 19.097q4.26 7.32 16.428 16.655H15.436Z"/>
        </svg>
      </span>
    </a>'
  ))
}

make_hal <- function(url_hal) {
  return(glue::glue(
    '<a href="{url_hal}" target="_blank" style="text-decoration: none; display: inline-flex; align-items: center;">
      <span style="display: inline-flex; align-items: center; margin-right: 5px;">
        <svg xmlns="http://www.w3.org/2000/svg" width="1em" height="1em" viewBox="0 0 512 512">
          <path fill="currentColor" d="M286.415 8c-.757 6.622-.946 13.433-.946 20.055c-.19 54.018 21.002 104.726 59.41 142.945c39.828 39.639 92.238 59.41 144.742 59.41c4.825 0 9.743-.19 14.567-.568c-1.986-18.069-5.674-35.76-11.256-52.41c-40.206.946-80.413-13.812-110.78-43.896c-28.381-28.191-43.895-65.652-43.706-105.29c0-2.744.189-5.488.189-7.948c-16.65-5.865-34.151-10.122-52.22-12.298m-66.6.379c-17.88 2.554-35.004 7-51.464 13.054c23.461 75.776 65.654 145.498 123.74 203.394c56.95 56.762 124.496 96.78 196.394 120.43c6.244-16.366 11.068-33.394 13.811-51.274c-63.194-21.19-122.604-56.762-172.743-106.712C278.28 136.186 241.006 75.168 219.815 8.38M394.26 47.827c-7.568 9.177-11.825 20.623-11.825 32.638c-.284 13.812 5.204 26.772 14.948 36.516s22.514 15.136 36.326 15.136h.19c11.825 0 22.894-3.877 32.07-11.256c-18.636-28.948-43.139-54.019-71.709-73.034M100.333 60.031C86.521 71.1 73.75 83.491 62.493 97.115L412.52 449.792c14.002-11.068 26.394-23.745 37.463-37.462zm-78.71 108.225A243.4 243.4 0 0 0 8.38 219.72c64.897 21.191 125.916 57.139 177.19 108.224c50.14 49.95 86.845 109.55 108.414 174.447c17.88-2.744 35.002-7.379 51.274-13.812c-23.935-73.979-65.275-141.808-122.225-198.38c-58.465-57.802-127.62-98.387-201.409-121.943m8.091 116.648a207 207 0 0 0-21.902.753c2.175 18.07 6.243 35.57 12.013 52.22c40.774-1.514 82.21 13.056 113.24 43.896c28.38 28.191 43.895 65.655 43.706 105.293c0 1.703 0 3.688-.189 5.486A239.4 239.4 0 0 0 228.992 504c.379-5.487.756-11.07.756-16.746c.095-53.828-20.906-104.724-59.315-142.943c-38.74-38.574-89.576-58.46-140.719-59.407m51.508 98.288c-12.203 0-23.745 4.258-33.016 12.015c19.204 28.57 44.275 52.882 73.413 71.33c7.19-8.894 11.067-20.057 11.067-31.693c0-13.812-5.203-26.866-14.947-36.516c-9.744-9.744-22.514-15.136-36.326-15.136z"/>
        </svg>
      </span>
    </a>'
  ))
}

make_stubs <- function(pubs) {
  journal <- str_to_lower(pubs$journal)
  journal <- str_replace_all(journal, ':', '')
  journal <- str_replace_all(journal, '`', '')
  journal <- str_replace_all(journal, "'", '')
  journal <- str_replace_all(journal, "\\.", '')
  journal <- str_replace_all(journal, "&", '')
  journal <- str_replace_all(journal, ',', '')
  journal <- str_replace_all(journal, '  ', '-')
  journal <- str_replace_all(journal, ' ', '-')
  return(paste0(pubs$year, '-', journal))
}

make_pub_list <- function(pubs, category) {
  x <- pubs[which(pubs$category == category),]
  pub_list <- list()
  for (i in 1:nrow(x)) {
    pub_list[[i]] <- make_pub(x[i,], index = i)
  }
  return(htmltools::HTML(paste(unlist(pub_list), collapse = "")))
}

make_talk_list <- function(talks, category) {
  x <- talks[which(talks$category == category),]
  talk_list <- list()
  for (i in 1:nrow(x)) {
    talk_list[[i]] <- make_presentation(x[i,], index = i)
  }
  return(htmltools::HTML(paste(unlist(talk_list), collapse = "")))
}

make_pub <- function(pub, index = NULL) {
  header <- FALSE
  altmetric <- make_altmetric(pub)
  if (is.null(index)) {
    cite <- pub$citation
  } else {
    cite <- glue::glue('{pub$citation}')
    if (index == 1) { header <- TRUE }
  }
    icon <- pub$icon
  
  # Format title in bold with large font size, add index number before, reduce space after title
  formatted_title <- glue::glue(
    '<div style="font-size: 18px; line-height: 1.1;">
     <span style="font-weight: normal;">{index}|</span> 
     <span style="font-weight: bold; margin-top: 0px; margin-bottom: 4px;">{title_to_html(pub$title)}</span>
   </div>
   <div style="font-size: 15px; margin-top: 5px; margin-bottom: -10px; line-height: 1.1;">{markdown_to_html(cite)}</div>
   <div style="font-size: 16px; margin-top: 0px; margin-bottom: -10px;">{icon}</div>
   <hr style="border: 1px solid #ccc;" />'
    )
  
  return(htmltools::HTML(glue::glue(
    '<div class="pub">
    <div class="grid">
    <div class="g-col-9"> {formatted_title} </div>
    <div class="g-col-3" style="text-align: center;"> {altmetric} </div>
    </div>'
  )))
}

make_presentation <- function(presentation, index = NULL) {
  header <- FALSE
  if (is.null(index)) {
    cite <- presentation$citation
  } else {
    cite <- glue::glue('{presentation$citation}')
    if (index == 1) { header <- TRUE }
  }
  icon <- presentation$icon
  
  # Format title in bold with large font size, add index number before, reduce space after title
  formatted_title <- glue::glue(
    '<div style="font-size: 18px; line-height: 1.1;">
     <span style="font-weight: normal;">{index}|</span> 
     <span style="font-weight: bold; margin-top: 0px; margin-bottom: 4px;">{title_to_html(presentation$title)}</span>
   </div>
   <div style="font-size: 15px; margin-top: 5px; margin-bottom: -10px; line-height: 1.1;">{markdown_to_html(cite)}</div>
   <div style="font-size: 16px; margin-top: 0px; margin-bottom: -10px;">{icon}</div>
   <hr style="border: 1px solid #ccc;" />'
  )
  
  return(htmltools::HTML(glue::glue(
    '<div class="presentation">
    <div class="grid">
    <div class="g-col-9"> {formatted_title} </div>
    </div>'
  )))
}

make_altmetric <- function(pub) {
  altmetric <- ""
  if (pub$category == 'peer_reviewed') {
    altmetric <- generate_altmetric_html(pub$doi)
  }
  return(altmetric)
}

markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }
  
  # Replace the author names with underlined last names
  text <- gsub(
    pattern = "\\\\\\*([^,]+), ([^,]+)", 
    replacement = "<u>\\\\*\\1</u>, \\2", 
    text
  )
  text <- gsub(
    pattern = "\\\\\\*\\\\\\*([^,]+), ([^,]+)", 
    replacement = "<u>\\\\*\\\\*\\1</u>, \\2", 
    text
  )
  
  # Render the text as HTML
  return(HTML(markdown::renderMarkdown(text = text)))
}

title_to_html <- function(title) {
  if (is.null(title)) { return(title) }
  # Convertir Markdown en HTML mais retirer les paragraphes et sauts de ligne
  rendered_title <- markdown::renderMarkdown(text = title)
  rendered_title <- gsub("<p>|</p>|\\n", "", rendered_title)  # Retirer <p> et sauts de ligne
  
  return(HTML(glue::glue("<span>{rendered_title}</span>")))
}

#get_pubs()
pubs <- get_pubs()
presentations <- get_presentations()

