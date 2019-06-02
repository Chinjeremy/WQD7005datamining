#! /usr/bin/python

from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import requests
from lxml import html

class AppCrawler:
    def __init__(self, starting_url, depth):
        self.starting_url = starting_url
        self.depth = depth
        self.apps = []
        
    def crawl(self):
        self.get_app_from_link(self.starting_url)
        return

    def get_app_from_link(self, link):
        start_page = requests.get(link)
        tree = html.fromstring(start_page.text)
        name = tree.xpath('//h1[@class="stock-profile f16"]/text()')[0]
        price = tree.xpath('//td[@id="slcontent_0_ileft_0_lastdonetext"]/text()')[0]
        code = tree.xpath('//li[@class="f14"]/text()')[1]
        #print(name)
        #print(code[3:])
        #print(price)
        print(name + ',' + code[3:] + ',' + price)
        return


class App:
    def __init__(self, name, code, price, links):
        self.name = name
        self.code = code
        self.price = price
        self.links = links

    def __str__(self):
        return ("Name: " + self.name.encode('UTF-8') +
        "\r\nCode: " + self.developer.encode('UTF-8') +
        "\r\nPrice: " + self.price.encode('UTF-8') + "\r\n")


alphabets = ["A","B", "C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","0-9"]
#alphabets = ["A"]

def main():
    for alphabet in alphabets:
        driver = webdriver.Chrome(executable_path=r"C:\Users\jeremy.chin.ee.wei\Downloads\WQD7005\chromedriver.exe")
        driver.get("https://www.thestar.com.my/business/marketwatch/stock-list/?alphabet=" + alphabet)
        companyLinks = driver.find_elements_by_xpath('//tr[@class="linedlist"]//a')
        for link in companyLinks:
            value = link.get_attribute('href')
            crawler = AppCrawler(value, 0)
            crawler.crawl()

if __name__ == "__main__": main()