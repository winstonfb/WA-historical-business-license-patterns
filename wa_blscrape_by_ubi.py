""" Scraper for WA State DoR business license database.

	Arguments:
		ubi_source: text file of known UBIs, each on a new line.

	Saves images into the current directory.
	Also references the list of *.jpg files in current directory on each start, to avoid repeat scraping.

"""
import urllib
import sys
from os import listdir
import time

import socket
HTTP_TIMEOUT = 5
socket.setdefaulttimeout(HTTP_TIMEOUT)

from datetime import datetime
startTime = datetime.now()

from selenium import webdriver
driver = webdriver.PhantomJS()

if __name__ == '__main__':

	ubi_source = sys.argv[1]
	already_acquired = [i[:-4] for i in listdir('.') if 'jpg' in i]

	def get_ubi_list(ubi_source):
	    g = open(ubi_source,'r')
	    ubi_list = []
	    for entry in g:
	        ubi_list.append(entry.strip())
	    g.close()
	    return ubi_list

	def retrieve_license_url(ubi):
	    driver.get('http://dor.wa.gov/content/doingbusiness/registermybusiness/brd/Default.aspx')
	    time.sleep(2)
	    element = driver.find_element_by_id('ctl00_MiddlePlaceHolder_BRDSearch1_txtUBITRA')
	    element.send_keys(ubi)
	    time.sleep(2)
	    element = driver.find_element_by_id('ctl00_MiddlePlaceHolder_BRDSearch1_lnkSearch').click()
	    time.sleep(2)
	    element = driver.find_element_by_name('ctl00$MiddlePlaceHolder$BRDSearch1$dgListResults$ctl03$TextImage1').click()
	    time.sleep(2)
	    element = driver.find_element_by_xpath("//img[@alt='Business registration information']")
	    image_source = element.get_attribute('src')
	    save_license(image_source,ubi)
	    return

	def save_license(image_source,ubi):

	    f = open('{0}.jpg'.format(ubi),'wb')
	    f.write(urllib.urlopen(image_source).read())
	    f.close()
	    return

	ubi_list = get_ubi_list(ubi_source)
	marker = 0
	total = len(ubi_list)

	for ubi in ubi_list:
	    if ubi not in already_acquired:
	        try:
	            retrieve_license_url(ubi)
	            marker+=1
	            time.sleep(3)
	        except Exception, e:
	            print(ubi,e)
	    else:
	        marker+=1
	    print('{0} out of {1} records retrieved'.format(marker,total))
	print(datetime.now()-startTime)
