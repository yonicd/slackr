#' @title Slack Teams
#' @description Manage multiple Slack teams
#' @format NULL
#' @usage NULL
#' @section Initialization:
#' A new 'Teams'-object is initialized using the `new()` method on the generator:
#'
#' \tabular{l}{
#'  `x <- teams$new(file = '~/slackr.json')`
#' }
#'
#' If a json file exists in the default path, it will be read on initialization
#'  and apply the add method to the teams listed in the file.
#'
#' @section Public Fields:
#'
#' \tabular{lll}{
#' Field \tab  Class \tab Description \cr
#' __file__ \tab  character \tab path the json containing slackr-app key and memberid\cr
#' __icon_emoji__ \tab  character \tab Icon Emoji to use when posting to Slack \cr
#' __username__ \tab  character \tab Username to use when posting to Slack
#' }
#'
#' @section Public Methods:
#'
#' \foldstart{Setup}
#'
#' \tabular{ll}{
#' Method \tab Description \cr
#' __use__(_team_) \tab Query [slackr-app](https://github.com/yonicd/slackr-app) with team name to set environment variables for slackr.\cr
#' __manual_setup__(_file_) \tab Use a dcf file to set up environment variables via [slackr_setup][slackr::slackr_setup]
#' }
#'
#' \foldend
#'
#' \foldstart{Information}
#'
#' \tabular{ll}{
#' Method \tab Description \cr
#' __names__() \tab Return the names of the stored team in the object \cr
#' __get__(_team_) \tab Return slackr-app key and memberid for a team or teams \cr
#' __current_env__() \tab Return the current values of the environment variables defined by slackr
#' }
#'
#' \foldend{}
#'
#' \foldstart{Manipulation}
#'
#' \tabular{ll}{
#' Method \tab Description \cr
#' __add__(_team_name_, _memberid_, _key_) \tab Manually add to the object slack-app key and memberid \cr
#' __rm__(_team_) \tab Remove a team stored in the object \cr
#' __update_file__(_file_) \tab Update the on disk json file with information of the stored teams \cr
#' __upload_file__(_file_) \tab Upload a json file from disk containing slackr-app key and memberid by team
#' }
#'
#' \foldend{}
#'
#' @examples
#' \dontrun{
#'
#' # initialize
#' x <- teams$new()
#'
#' x
#'
#' if(length(x$names())>1){
#'
#' team_name <- x$names()[1]
#'
#' # query slackr-app key and memberid for team
#' x$get(team_name)
#'
#' # query slackr-app API to setup environment variables
#' x$use(team_name)
#'
#' # use slackr
#' slackr::slackr('Test')
#'
#' }
#'
#' # Use dcf file for setup
#' if(file.exists('~/.slackr')){
#'
#' x$manual_setup('~/.slackr')
#'
#' # use slackr
#' slackr::slackr('Test')
#'
#' }
#'
#' }
#'
#' @format NULL
#' @usage NULL
#' @rdname teams
#' @export
#' @importFrom R6 R6Class
#' @importFrom jsonlite read_json write_json toJSON
teams <- R6::R6Class(
  classname = "Teams",
    public = list(initialize = function(file = '~/slackr.json') {
      self$upload_file(file)
    },
    print        = function(){

      print(self$names())
    },
    names        = function(){
      names(private$teams)
    },
    current_env  = function(){
      private$GETENV()
    },
    get          = function(team){

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
    add          = function(team_name, memberid, key){
      new_team        <- list(memberid = memberid,key = key)
      names(new_team) <- team_name
      private$teams   <- append(private$teams,new_team)
    },
    rm           = function(team){

      idx <- which(team%in%self$names())

      if(length(idx)>0)
        private$teams <- private$teams[-idx]
    },
    use          = function(team, verbose = TRUE){

      private$GET(team)
      private$SETENV()

      if(verbose)
        private$SETENV_MSG(team)


    },
    update_file  = function(echo = TRUE, auto_unbox = TRUE, pretty = TRUE,...){

      jsonlite::write_json(self$get(self$names()),self$file,
                           auto_unbox = auto_unbox,
                           pretty = pretty,...)

      if(echo){

        jsonlite::toJSON(self$get(self$names()),
                         auto_unbox = auto_unbox,
                         pretty = pretty,...)

      }
    },
    upload_file  = function(file){

      if(file.exists(file)){

        self$file <- file

        jsn <- jsonlite::read_json(self$file)

        private$teams <- append(private$teams,jsn)

      }
    },
    manual_setup = function(file = '~/.slackr', verbose = TRUE){

      res <- read.dcf(file)

      for(i in intersect(colnames(res),private$cred_fields)){

        if(i%in%c('username','icon_emoji')){

          self[[i]] <- res[[1,i]]

        }else{

          private$creds[[i]] <- res[[1,i]]

        }

      }

      private$SETENV()

      if(verbose)
        private$SETENV_MSG(team = 'user')

    },
    file         = NA_character_,
    icon_emoji   = '',
    username     = 'slackr'
  ),
  private = list(
    teams  = list(),
    creds  = list(),
    cred_fields = c('channel','incoming_webhook_url','api_token','username','icon_emoji'),
    env_fields = c('channel','incoming_url_prefix','api_token','username','icon_emoji'),
    API    = 'https://slackr-auth.herokuapp.com/creds',
    GET    = function(team){

      creds <- self$get(team)

      if(length(creds)>1)
        message(sprintf('Only the first team (%s) will be used',team[1]))

      URI <- file.path(private$API,creds[[1]]$memberid,creds[[1]]$key)

      private$creds <- httr::content(httr::GET(URI))

    },
    SETENV_MSG = function(team){
      tmpl <- "slackr environment variables are set to '%s' supplied definitions"
      message(sprintf(tmpl, team))
    },
    GETENV = function(){
      sapply(
        sprintf('SLACK_%s',toupper(private$env_fields)),Sys.getenv
        )
    },
    SETENV = function(){
      Sys.setenv(SLACK_CHANNEL             = private$creds$channel)
      Sys.setenv(SLACK_USERNAME            = self$username)
      Sys.setenv(SLACK_ICON_EMOJI          = self$icon_emoji)
      Sys.setenv(SLACK_INCOMING_URL_PREFIX = private$creds$incoming_webhook_url)
      Sys.setenv(SLACK_API_TOKEN           = private$creds$api_token)
    }
  )
)
