# -*- coding: utf-8 -*-
"""
Created on Mon Apr  2 17:06:03 2018

@author: Norvard
"""

from urllib.request import urlopen
from bs4 import BeautifulSoup as bs
import pandas as pd
import re


url='https://www.consumeraffairs.com/travel/united.html'
urls=[]
urls.append(url)
soups=[]

page=urlopen(url)
soup=bs(page, 'html.parser')

for i in range(2,72):
    new_url='https://www.consumeraffairs.com/travel/united.html?page='+str(i)
    urls.append(new_url)

for j in urls:
    page = urlopen(j)
    soup = bs(page, 'html.parser')
    soups.append(soup)

#dates
dates=[]

for soup in soups:
    for date in soup.find_all('span', attrs={'class': 'ca-txt-cpt ca-txt--clr-gray'}):
        dates.append(date.text)

newdates=[]  
for date in dates:
    newdates.append(date.split()[2:])
    for i in range(len(newdates)):
        ', '.join(newdates[i])

newdates=newdates[4:]
fin_dates=[]
for date in newdates:
    fin_dates.append(', '.join(date))
    
final_dates=[]
for date in fin_dates:
    final_dates.append(''.join(date.split(',',2)))

fin_dates1=[]
for date in final_dates:
   fin_dates1.append( date.replace('.',''))
    

    
# reviews

reviews=[]
for soup in soups:
    for review in soup.find_all('div', 
                                attrs={'class':'rvw-bd ca-txt-bd-2'}):
        if not review.find('div', attrs={'class':'js-collapsed'}):
            reviews.append((review.find('p')))
        else:
            reviews.append((str((review.find('p')))+
          str((review.find('div', attrs={'class':'js-collapsed'})))))

str_rev=[str(i) for i in reviews]
fin_rev=[]
for review in str_rev:
    fin_rev.append(re.sub('<[A-Za-z\/][^>]*>', '', review))
    
rev_df_new= pd.DataFrame({'date':fin_dates1,
                       'reviews':fin_rev})
print(rev_df_new.info())



rev_df_new.to_csv('reviews_final.csv')

