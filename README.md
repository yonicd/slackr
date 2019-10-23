
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/slackr)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/slackr)](http://cran.r-project.org/package=slackr)

![](slackr.png)

`slackr` - a package to send user messages & webhook API messages to
Slack channels/users

The `slackr` package contains functions that make it possible to
interact with the Slack messaging platform. When you need to share
information/data from R, rather than resort to copy/paste in e-mails or
other services like Skype, you can use this package to send
well-formatted output from multiple R objects and expressions to all
teammates at the same time with little effort. You can also send images
from the current graphics device, R objects (as R data files), and
upload files.

### BREAKING CHANGES

Versions 1.4+ BREAK THINGS.

Support has been removed for the “old style” incoming web hooks (see
“Setup” below for the required incoming web hook URL format).

The incoming webhook “token” is no longer required or used.

### News

  - Version `1.4.2` fixes for changes to the Slack API causing duplicate
    column names and breaking functions
  - Version `1.4.0.9000` new `slackr_msg()` + many fixes and BREAKING
    CHANGES (see above)
  - Version `1.3.1.9000` Removed `data.table` dependency (replaced with
    `dplyr`); added access to `im.list`
    (<https://api.slack.com/methods/im.list>) thx to PR from Quinn Weber
  - Version `1.3.0.9000` Radically changed how `slackr` works. Functions
    have camelCase and under\_score versions
  - Version `1.2.3` added more parameter error cheking, remobved the
    need for ending `?` on webhook URL and added defaults for missing
    setup parameters.
  - Version `1.2.2` fixed
    [issue](https://github.com/hrbrmstr/slackr/issues/4) (bug in `1.2.1`
    fix)
  - Version `1.2.1` fixed
    [issue](https://github.com/hrbrmstr/slackr/issues/3) when there are
    no private groups defined
  - Version `1.2` re-introduced `ggslackr()` (first [CRAN
    version](http://cran.at.r-project.org/web/packages/slackr/index.html))
  - Version `1.1.1` fixed a bug in the new full API `slackr()` function
  - Version `1.1` added graphics & files capability
  - Version `1.0` released

Many thanks to:

  - [Jay Jacobs](https://github.com/jayjacobs)
  - [David Severski](https://github.com/davidski)
  - [Quinn Weber](https://github.com/qsweber)
  - [Konrad Karczewski](https://github.com/konradjk)
  - [Ed Niles](https://github.com/eniles)
  - [Rick Saporta](https://github.com/rsaporta)
  - [Jonathan Sidi](https://github.com/yonicd)

for their contributions to the package\!

The following functions are implemented:

  - `slackr_setup` : initialize necessary environment variables
  - `slackr` : send stuff to Slack
  - `slackr_bot` : send stuff to Slack using an incoming webhook URL
  - `dev_slackr` : send the graphics contents of the current device to a
    to Slack channel
  - `ggslackr` : send a ggplot object to a Slack channel (no existing
    device plot required, useful for scripts)
  - `save_slackr` : save R objects to an RData file on Slack
  - `slackr_upload` : upload any file to Slack
  - `slackr_users` : get a data frame of Slack
  - `slackr_channels` : get a data frame of Slack
  - `slackr_groups` : get a data frame of Slack groups
  - `text_slackr` : Send regular or preformatted messages to Slack
  - `slackr_msg` : Slightly different version of `text_slackr()`
  - `slackr_history`: Return message history of a Slack channel to a
    data.frame
  - `delete_slackr`: Delete messages from a Slack channel
  - `register_onexit`: Append an `on.exit` call to R functions (can be
    used with other package functions) and its output will be sent to a
    Slack channel.

### Create an Incoming Webhook

The following button will create the webhook with the correct
scope

<a href="https://slack.com/oauth/authorize?client_id=220157155520.220159943344&scope=incoming-webhook,files:read,files:write:user,chat:write:bot,chat:write:user,mpim:write,mpim:read,mpim:history,im:write,im:read,im:history,groups:write,groups:read,groups:history,channels:write,channels:read,channels:history,emoji:read,usergroups:read,users:read" target="_blank"><img alt="Add to Slack" height="40" width="139" src="https://platform.slack-edge.com/img/add_to_slack.png" srcset="https://platform.slack-edge.com/img/add_to_slack.png 1x, https://platform.slack-edge.com/img/add_to_slack@2x.png 2x"></a>

### SETUP

<details open>

<summary> <span title="Click to Expand"> `teams` </span> </summary>

`slackr::teams` is an `R6` object that allows the user to manage
multiple Slack teams during a session. The object queries the
[slackr-app](https://github.com/yonicd/slackr-app) database to set the
environment variables needed by slackr.

</details>

<br>

<details closed>

<summary> <span title="Click to Expand"> `slackr_setup` </span>
</summary>

The `slackr_setup()` function will try to read setup values from a
`~/.slackr` (you can change the default) configuration file, which may
be easier and more secure than passing them in manually (plus, will
allow you to have multiple slackr configs for multiple Slack.com teams).
The file is in Debian Control File (DCF) format since it really doesn’t
need to be JSON and R has a handy `read.dcf()` function since that’s
what `DESCRIPTION` files are coded in.

Here’s the basic format for the configuration file:

    api_token: YOUR_FULL_API_TOKEN
    channel: #general
    username: slackr
    incoming_webhook_url: https://hooks.slack.com/services/XXXXX/XXXXX/XXXXX

You can also change the default emoji icon (from the one you setup at
integration creation time) with `icon_emoji`.

</details>

<br>

### Installation

``` r
# stable/CRAN
install.packages("slackr")

# bleeding edge
remotes::install_github("hrbrmstr/slackr")
```

### Usage

``` r
library(slackr)

# current verison
packageVersion("slackr")
#> [1] '1.5.0'
```

``` r
x <- teams$new()
x$use('slackr')

slackr(str(iris))

# send images
library(ggplot2)
qplot(mpg, wt, data=mtcars)
dev.slackr("#results")

barplot(VADeaths)
dev.slackr("@jayjacobs")

ggslackr(qplot(mpg, wt, data=mtcars))
```

### Test Results

``` r
library(slackr)
library(testthat)

date()
#> [1] "Tue Oct 22 21:38:17 2019"

test_dir("tests/")
#> ✔ |  OK F W S | Context
#> ══ testthat results  ════════════════════════════════════════════════════════════════════════════
#> [ OK: 0 | SKIPPED: 2 | WARNINGS: 0 | FAILED: 0 ]
#> 
#> ══ Results ══════════════════════════════════════════════════════════════════════════════════════
#> Duration: 0.1 s
#> 
#> OK:       0
#> Failed:   0
#> Warnings: 0
#> Skipped:  0
#> 
#> You rock!
```

### Onexit Usage

``` r
 ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
 trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
 group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
 weight <- c(ctl, trt)

 #pass a message to Slack channel 'general'
 register_onexit(lm,'bazinga!',channel="#general")

 lm.D9 <- slack_lm(weight ~ group)

 #test that output keeps inheritance
 summary(lm.D9)

 #pass a message to Slack channel 'general' with a header message to begin output
 register_onexit(lm,'bazinga!',
 channel="#general",
 header_msg='This is a message to begin')

 lm.D9 <- slack_lm(weight ~ group)

 #onexit with an expression that calls lm.plot
 register_onexit(lm,{
  par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
  plot(z) #z is the internal output of stats::lm()
 },
 channel="#general",
 header_msg = 'This is a plot just for this output',
 use_device = TRUE)

 lm.D9 <- slack_lm(weight ~ group)

#clean up slack channel from examples
delete_slackr(count = 6,channel = '#general')
```
