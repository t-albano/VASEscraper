from selenium import webdriver
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import Select
from bs4 import BeautifulSoup
import re
import pandas as pd


# This is a sample Python script.

# Press Shift+F10 to execute it or replace it with your code.
# Press Double Shift to search everywhere for classes, files, tool windows, actions, and settings.

# create driver. Will open new browser window.
driver = webdriver.Chrome(ChromeDriverManager().install())

# url for VASE. Will append address to this to go through the year
base_url = 'taea.org/vase/'
search_url = 'https://www.taea.org/vase/Search-2022.cfm'


# create list of years up until now
years = list()
recent_year = 2022
for i in range(2001, recent_year + 1):
    years.append(i)

# Go to page
driver.get(search_url)
# Select 2D in drop-down form
dimension = Select(driver.find_element(By.NAME, 'Dimension'))
dimension.select_by_index(2) # 1 for 2D, 2 for 3D
# Click search
driver.find_element(By.XPATH, "//input[@type='submit']").click()

# collect artwork id's on each page. Better than clicking on every thumbnail to get information
ids = list()

main_hand = driver.current_window_handle

driver.find_elements(By.XPATH, "//td[@class = 'artworkThumbnail']/a[@href]")[3].click()
hand = driver.window_handles

# switch to newly opened window with artwork info
driver.switch_to.window(hand[1])
# get artist info from page
a = driver.find_element(By.CLASS_NAME, "artworkViewInfo").text
asplit = re.split('\n|Division:|Region:|   ', a)
# format the info into something workable
for word in asplit:
    word.lstrip(" ")
    word.rstrip(" ")
# check "gold" status. Use beautifulsoup, since selenium puts out exception when "gold" status is not on page
c = BeautifulSoup(driver.current_url, 'lxml')
gold_info = 0
if c.find_all('h5'):
    gold_info = 1
asplit.append(gold_info)
driver.close()



