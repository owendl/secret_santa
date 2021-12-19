# secret_santa

This repo is too build a simple R shiny app that automatically does secret santa drawings and sends out emails to the participants.

It will also collect some of the tips and tricks I used to build this implementation.

## Basic Architecture
This app will have a web form for users to input:
* Email and Name of each person in the drawing
* (Optional) Any restricted pairings
* (Optional) Admin email- the email of a person to receive a table of all of the assignments.

## SMTP infrastructure
Because I want to deploy this app to shinyapps.io, I cannot use the $sendmailR$ package ([see post](https://community.rstudio.com/t/shinyapps-io-and-the-sendmailr-package/1719/2)). Thus I need to use a separate SMTP server to actually send the email. While it is possible to configure my own macbook to do [this](https://mailtrap.io/blog/setup-smtp-server/), I want this app to live on its own. There are several options out there for free SMTP servers, each with their own pros and cons ([link](https://mailtrap.io/blog/free-smtp-servers/)) but I decided with using the free level of the Gmail service (mostly because I want as much flexibility as my cheap self will allow).