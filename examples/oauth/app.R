library(shiny)

options(shiny.port=8100)

# TODO: Figure out how not to require shiny.port to be set in advance
# TODO: Verify that cookies work in Connect/SSP
# TODO: Whole-page protection behind oauth

source("oauth.R")

github_oauth_config <- oauth_config(
  oauth_endpoint_uri = "https://github.com/login/oauth/authorize",
  token_endpoint_uri = "https://github.com/login/oauth/access_token",
  app_uri = "http://127.0.0.1:8100/",

  # Store client_id and client_secret however you want--just hardcoded for this example
  client_id = "700d40c400de637d9780",
  client_secret = "e6383430779d9df9b253e7d6b1fb53308033873d",

  scope = ""
)

google_oauth_config <- oauth_config(
  oauth_endpoint_uri = "https://accounts.google.com/o/oauth2/v2/auth",
  token_endpoint_uri = "https://www.googleapis.com/oauth2/v4/token",
  app_uri = "http://127.0.0.1:8100/",

  # Store client_id and client_secret however you want--just hardcoded for this example
  client_id = "350280321053-7bq89pep4da46df2g66ddjnj6e3qrnie.apps.googleusercontent.com",
  client_secret = "8_AHVNXyKyO3tBAZFAy-2y0B",

  scope = "https://www.googleapis.com/auth/drive.metadata.readonly"
)


ui <- fluidPage(
  textOutput("username_github", inline = TRUE),
  p(
    oauth_login_ui("oauth_login_github")
  ),
  verbatimTextOutput("userinfo_github"),
  textOutput("username_google", inline = TRUE),
  p(
    oauth_login_ui("oauth_login_google")
  ),
  verbatimTextOutput("userinfo_google")
)

server <- function(input, output, session) {

  ### GITHUB

  token_github <- callModule(oauth_login, id = "oauth_login_github", github_oauth_config)

  output$username_github <- renderText({
    if (is.null(token_github())) {
      "Not logged in to GitHub"
    } else {
      resp <- httr::GET(
        "https://api.github.com/user",
        httr::config(token = token_github())
      )
      paste0("Logged in as ", httr::content(resp)$login)
    }
  })

  output$userinfo_github <- renderPrint({
    validate(
      need(!is.null(token_github()), message = "No current user")
    )
    resp <- httr::GET(
      "https://api.github.com/user",
      httr::config(token = token_github())
    )
    httr::content(resp)[c("name", "login", "id", "bio", "public_repos", "followers")]
  })

  ## GOOGLE

  token_google <- callModule(oauth_login, id = "oauth_login_google", google_oauth_config)

  output$username_google <- renderText({
    if (is.null(token_google())) {
      "Not logged in to Google"
    } else {
      gargle:::get_email(token_google())
    }
  })

  output$userinfo_google <- renderPrint({
    validate(
      need(!is.null(token_google()), message = "No current user")
    )
    gargle:::get_userinfo(token_google())
    # drive_auth(token = token_google())
    # drive_user()
  })

  output$table <- DT::renderDataTable(DT::datatable({
    data <- head(iris, 3)
    #input$man
    data
  }))

}

shinyApp(ui, server, options = list(port = 8100))
