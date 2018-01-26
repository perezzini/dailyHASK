# dailyHASK
A daily news articles e-mail delivery developed in Haskell.

## Description
It's clear that the Web is formed by billions of documents of all kinds: from restaurant's reviews, social networks, news, etc. Then, given that the Web is enormous, the key here is to use applications that enable us to retrieve the information we're looking for in a simple way. There exist many of these, but it is on us to continue creating these kind of applications. This project aims to be another of those apps.

## News and weather information
When we read newspapers online, we are not usually interested in all of the articles the newspaper presents: we are just *really* interested in a couple of them. So, we use to read differents online blogs and newspapers to find those articles we care about. Sort of the same happens when searching for weather information: many sources and, many times, inaccurate between them.

## How *dailyHASK* works
*dailyHASK* sends daily mails to users with current weather at that time, and a set of news the user is interested about. It uses several APIs:
- [Google Maps Geocoding API](https://developers.google.com/maps/documentation/geocoding/start)
- [NewsAPI.org API](https://newsapi.org/)
- [Open Weather Map API](http://openweathermap.org/api)

## Usage
1. Clone repo:
```shell
git clone https://github.com/perezzini/dailyHASK.git dailyHASK
```
2. Install [MongoDB](https://docs.mongodb.com/manual/installation/), and init it.
3. In the root of the folder, create a configuration file named `app.cfg` with the following contents (for more info about NewsApi.org's endpoint options, please visit their website):
```shell
database.server = YOUR_DATABASE_SERVER
database.port = YOUR_DATABASE_PORT
database.db = dailyhask
database.usersCollection = users

smtp.hostname = YOUR_SMTP_HOSTNAME
smtp.user.name.address = YOUR_SMTP_USER_NAME_ADDRESS
smtp.user.password = YOUR_SMTP_USER_NAME_PASSWORD
smtp.mail.address.alias = YOUR_SMTP_USER_NAME_ADDRESS_ALIAS

api.googlemaps.key = YOUR_GOOGLEMAPS_KEY
api.googlemaps.endpoint = https://maps.googleapis.com/maps/api/geocode/json

api.news.key = YOUR_NEWSAPIORG_KEY
api.news.endpoint.everything = https://newsapi.org/v2/everything
api.news.endpoint.everything.sortBy = YOUR_SORTBY_OPTION
api.news.endpoint.everything.language = YOUR_LANGUAGE_OPTION
api.news.endpoint.sources = https://newsapi.org/v2/sources

api.owm.key = YOUR_OWM_KEY
api.owm.endpoint.current = https://api.openweathermap.org/data/2.5/weather
```
4. Inside `dailyHASK`, run [Stack](https://docs.haskellstack.org/en/stable/README/):
```shell
stack setup
stack build
```
5. Execute application:
```shell
stack exec dailyHASK-exe
```

> Project implemented for a Computer Science class at [National University of Rosario](http://www.unr.edu.ar), [DCC](https://dcc.fceia.unr.edu.ar/)
