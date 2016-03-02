JSONEndpoint <- "https://data.kcmo.org/resource/8umx-fi4g.json"

write.socrata(Stats,JSONEndpoint,"REPLACE",socrataEmail,socrataPassword,app_token = Token)
 