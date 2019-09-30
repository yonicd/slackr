#' @title Slack Teams
#' @description Manage multilple Slack teams
#' @details If a json file exists, it will be read on initialization and
#' add teams listed in the file.
#' @format NULL
#' @usage NULL
#' @rdname teams
#' @export
#' @importFrom R6 R6Class
#' @importFrom jsonlite read_json write_json toJSON
teams <- R6::R6Class(
  classname = "Teams",
  public = list(
    initialize = function(file = '~/slackr.json') {

      if(file.exists(file)){

        self$file <- file

        jsn <- jsonlite::read_json(self$file)

        private$teams <- append(private$teams,jsn)

      }
    },
    file       = NA_character_,
    icon_emoji = '',
    print      = function(){

      print(self$names())
    },
    names      = function() names(private$teams),
    get        = function(team){

      idx <- which(team%in%self$names())

      if(length(idx)!=length(team)){

        if(!length(idx)){
          miss_team <- paste0(team[seq_along(team)],collapse = ', ')
        }else{
          miss_team <- paste0(team[-idx],collapse = ', ')
        }
        msg_names <- 'Use the $names() method to return the stored team names'
        msg <- sprintf("Team '%s' not found\n%s", miss_team,msg_names)
        return(message(msg))

        team <- team[idx]

      }

      private$teams[team]

    },
    add        = function(team_name,memberid,key){
      new_team        <- list(memberid = memberid,key = key)
      names(new_team) <- team_name
      private$teams   <- append(private$teams,new_team)
    },
    rm         = function(team){

      idx <- which(team%in%self$names())

      if(length(idx)>0)
        private$teams <- private$teams[-idx]
    },
    use        = function(team){

      private$GET(team)
      private$SETENV()

    },
    update     = function(echo = TRUE, auto_unbox = TRUE, pretty = TRUE,...){

      jsonlite::write_json(self$get(self$names()),self$file,
                           auto_unbox = auto_unbox,
                           pretty = pretty,...)

      if(echo){

        jsonlite::toJSON(self$get(self$names()),
                         auto_unbox = auto_unbox,
                         pretty = pretty,...)

      }
    }
  ),
  private = list(
    teams  = list(),
    creds  = list(),
    API    = 'https://slackr-auth.herokuapp.com/creds',
    GET    = function(team){

      creds <- self$get(team)

      if(length(creds)>1)
        message(sprintf('Only the first team (%s) will be used',team[1]))

      URI <- file.path(private$API,creds[[1]]$memberid,creds[[1]]$key)

      private$creds <- httr::content(httr::GET(URI))

    },
    SETENV = function(){
      Sys.setenv(SLACK_CHANNEL             = private$creds$channel)
      Sys.setenv(SLACK_USERNAME            = 'slackr')
      Sys.setenv(SLACK_ICON_EMOJI          = self$icon_emoji)
      Sys.setenv(SLACK_INCOMING_URL_PREFIX = private$creds$incoming_webhook_url)
      Sys.setenv(SLACK_API_TOKEN           = private$creds$api_token)
    }
  )
)
