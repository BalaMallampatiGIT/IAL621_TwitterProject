################
### Original ###
################
# install.packages("RSelenium")
# install.packages("rvest")
# install.packages("tidyverse")
library(RSelenium)
library(tidyverse)
library(rvest)
binman::list_versions("chromedriver")


# chrome_options.setBinary("C:/Program Files/Google/Chrome Beta/Application")
# chrome_options.binary_location = "C:/Program Files/Google/Chrome Beta/Application/chrome.exe"
# driver = webdriver.Chrome(executable_path='C:/path/to/chromedriver.exe')

# rD <- rsDriver(port = 4568L, browser=c("chrome"), chromever = "95.0.4638.54")
# remDr <- rD[["client"]]
# remDr$navigate("http://www.google.com/ncr")
# remDr$navigate("http://www.bbc.com")
# remDr$close()
# # stop the selenium server
# rD[["server"]]$stop()
# 
# # if user forgets to stop server it will be garbage collected.
# rD <- rsDriver()
# rm(rD)
# gc(rD)
# 
# ## End(Not run)

driver <- rsDriver(port = 4569L, browser=c("chrome"), chromever = "95.0.4638.54")
remote_driver <- driver[["client"]]
remote_driver$open()
remote_driver$navigate("https://www.latlong.net/convert-address-to-lat-long.html")

address_element <- remote_driver$findElement(using = 'class', value = 'width70')
