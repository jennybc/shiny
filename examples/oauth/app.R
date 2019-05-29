library(shiny)

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

github_oauth_config <- oauth_config(
  oauth_endpoint = httr::oauth_endpoints("github"),
  oauth_app = httr::oauth_app(
    appname = "shiny-github",
    key = "700d40c400de637d9780",
    secret = "e6383430779d9df9b253e7d6b1fb53308033873d",
    redirect_uri = NULL
  ),
  app_uri = "http://127.0.0.1:8100/",
  scopes = ""
)

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
  # GITHUB
  textOutput("username_github", inline = TRUE),
  p(
    oauth_login_ui("oauth_login_github")
  ),
  verbatimTextOutput("userinfo_github"),

  hr(),

  # GOOGLE
  textOutput("username_google", inline = TRUE),
  p(
    oauth_login_ui("oauth_login_google")
  ),
  verbatimTextOutput("userinfo_google")
)

server <- function(input, output, session) {

  # GITHUB
  token_github <- callModule(
    oauth_login,
    id = "oauth_login_github",
    github_oauth_config
  )

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
    these <- c("name", "login", "id", "bio", "public_repos", "followers")
    httr::content(resp)[these]
  })

  # GOOGLE
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
    }
  })

  output$userinfo_google <- renderPrint({
    validate(
      need(!is.null(token_google()), message = "No current user")
    )
    str(token_google()$credentials)
    gargle:::get_userinfo(token_google())
  })

}

shinyApp(ui, server, options = list(port = 8100))
