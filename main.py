from selenium import webdriver
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import Select
from bs4 import BeautifulSoup
import re
import pandas as pd


# This is a sample Python script.

# Press Shift+F10 to execute it or replace it with your code.
# Press Double Shift to search everywhere for classes, files, tool windows, actions, and settings.

def up_to_years(current: int) -> list:
    # Get all years until last competition year from first competition year
    all_years = list()
    first_year = 2001
    for i in range(first_year, current + 1):
        all_years.append(i)

    return all_years


def pretty_data(phrase: str) -> list:
    # get messy string and put into standard format
    cut_up = re.split('\n|Division:|Region:|  ', phrase)
    # format the info into something workable
    for word in cut_up:
        word.lstrip(" ")
        word.rstrip(" ")

    # list in order [name, title, region, division, school]
    return cut_up


def gold_status(url: str) -> int:
    # check "gold" status.
    # Use beautifulsoup, since selenium puts out exception when "gold" status is not on page
    gold = BeautifulSoup(url, 'lxml')

    # default to 0. It is not found on page
    stat = 0
    if gold.find_all('h5'):
        stat = 1

    return stat


def window_info(page: str) -> list:
    # switch to newly opened window with artwork info
    driver.switch_to.window(page)

    # get artist info from page
    basic_info = driver.find_element(By.CLASS_NAME, "artworkViewInfo").text

    artist_info = pretty_data(basic_info)
    g = gold_status(driver.current_url)
    artist_info.append(g)

    # list in the form [name, title, division, region, school, gold status]
    return artist_info


# create driver. Will open new browser window.
driver = webdriver.Chrome(ChromeDriverManager().install())

# url for VASE. Will append address to this to go through the year
search_url = 'https://www.taea.org/vase/Search-2022.cfm'

# create list of years up until now
years = up_to_years(2022)

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

ids.append(window_info(hand[1]))
driver.close()



