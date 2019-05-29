library(shiny)
# using dev branch of gargle
# devtools::install_github("r-lib/gargle@dev")
# use a dev branch of googledrive
# devtools::install_github("tidyverse/gooledrive@byo-token")
library(googledrive)

options(shiny.port = 8100)

# TODO: Figure out how not to require shiny.port to be set in advance
# TODO: Verify that cookies work in Connect/SSP
# TODO: Whole-page protection behind oauth

# TODO: think about access token vs refresh token + access token
# there are various reasons why one might not necessarily get a refresh
# token but, instead, just get access token
# for example, Joe's original demo app probably has "Type = Web Application"
# as opposed to "Type = Other" and that seems to matter
# https://stackoverflow.com/questions/10827920/not-receiving-google-oauth-refresh-token/10857806#10857806

source("oauth.R")

google_oauth_config <- oauth_config(
  oauth_endpoint = httr::oauth_endpoints("google"),
  # oauth_app = httr::oauth_app(
  #   appname = "shiny-google",
  #   key = "350280321053-7bq89pep4da46df2g66ddjnj6e3qrnie.apps.googleusercontent.com",
  #   secret = "8_AHVNXyKyO3tBAZFAy-2y0B",
  #   redirect_uri = NULL
  # ),
  oauth_app = gargle::gargle_app(),
  app_uri = "http://127.0.0.1:8100/",
  scopes = c(
    "https://www.googleapis.com/auth/drive.metadata.readonly",
    "https://www.googleapis.com/auth/userinfo.email"
  )
)


ui <- fluidPage(
  # GOOGLE
  textOutput("username_google", inline = TRUE),
  p(
    oauth_login_ui("oauth_login_google")
  ),
  verbatimTextOutput("userinfo_google"),
  DT::dataTableOutput("table")
)

server <- function(input, output, session) {

  token_google <- callModule(
    oauth_login,
    id = "oauth_login_google",
    google_oauth_config
  )

  output$username_google <- renderText({
    if (is.null(token_google())) {
      "Not logged in to Google"
    } else {
      gargle:::get_email(token_google())
      drive_auth(token = token_google())
    }
  })

  output$userinfo_google <- renderPrint({
    validate(
      need(googledrive:::have_token(), message = "No current user")
    )
    drive_user()
  })

  output$table <- DT::renderDataTable(DT::datatable({
    validate(
      need(googledrive:::have_token(), message = FALSE)
    )
    data <- drive_find(n_max = 10)
    data <- drive_reveal(data, "mime_type")
    data[c("name", "mime_type")]
  }))

}

shinyApp(ui, server, options = list(port = 8100))
