from selenium import webdriver
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import Select
from bs4 import BeautifulSoup
import re
import pandas as pd
import requests


def up_to_years(current: int) -> list:
    # Get all years until last competition year from first competition year
    all_years = list()
    first_year = 2017
    for i in range(first_year, current + 1):
        all_years.append(i)

    return all_years


def total_art() -> int:
    # find total number of art pieces in search looking at top of page
    num_art = 0
    h = driver.find_element(By.TAG_NAME, 'p').text
    for i in h.split():
        if i.isdigit():
            num_art = int(i)
            break

    return num_art


def pretty_data(phrase: str) -> list:
    # get messy string and put into standard format
    cut_up = re.split('\n|Division:|Region:|   ', phrase)
    # format the info into something workable
    i = 0
    while i < len(cut_up):
        cut_up[i] = cut_up[i].strip(' ')
        if cut_up[i] == '':
            cut_up.pop(i)
            i -= 1
        i += 1

    # list in order [name, title, region, division, school]
    return cut_up


def gold_status(url: str) -> int:
    # check "gold" status.
    # Use beautifulsoup, since selenium puts out exception when "gold" status is not on page
    response = requests.get(url)

    gold = BeautifulSoup(response.text, 'html.parser')

    # default to 0. It is not found on page
    stat = 0
    if len(gold.find_all("h5")) > 0:
        stat = 1

    return stat


def window_info(page: str, status: int, date: int) -> list:
    # switch to newly opened window with artwork info
    driver.switch_to.window(page)

    # get artist info from page
    basic_info = driver.find_element(By.CLASS_NAME, "artworkViewInfo").text

    artist_info = pretty_data(basic_info)
    g = gold_status(driver.current_url)
    artist_info.append(g)

    if status == 1:
        artist_info.append('2D')
    else:
        artist_info.append('3D')

    artist_info.append(date)
    artist_info.append(driver.current_url)

    # list in the form [name, title, division, region, school, gold status, url]
    return artist_info


if __name__ == "__main__":

    # collect artwork id's on each page. Better than clicking on every thumbnail to get information
    ids = list()
    columns = ['Student', 'Title', 'Division', 'Region', 'School', 'Gold_Seal', 'Dimension', 'Year', 'URL']
    # create list of years up until now
    years = up_to_years(2022)

    for y in years:
        search_url = f"https://www.taea.org/vase/Search-{y}.cfm"

        # create driver. Will open new browser window.
        driver = webdriver.Chrome(ChromeDriverManager().install())

        for d in [1, 2]:
            # Go to page
            driver.get(search_url)

            # Select 2D in drop-down form
            dimension = Select(driver.find_element(By.NAME, 'Dimension'))
            dimension.select_by_index(d) # 1 for 2D, 2 for 3D

            # Click submit for search
            driver.find_element(By.XPATH, "//input[@type='submit']").click()

            art_amount = total_art()

            while art_amount > 0:

                main_hand = driver.current_window_handle
                page_art = driver.find_elements(By.XPATH, "//td[@class = 'artworkThumbnail']/a[@href]")

                art_amount -= len(page_art)

                for view in page_art:
                    view.click()
                    hand = driver.window_handles

                    # click on newly opened handle
                    ids.append(window_info(hand[1], d, y))
                    driver.close()
                    driver.switch_to.window(main_hand)

                if art_amount > 0:
                    # click on next button conditional on number of artworks
                    driver.find_element(By.XPATH, "//input[@src = '/VASE/images/next1.gif']").click()

    driver.quit()

    # export collected data into csv file for manipulation in R
    df = pd.DataFrame(ids, columns=columns)
    art_csv = df.to_csv('VASE_rawdata_newest.csv', index=False)

