from scrapy.contrib.spiders import CrawlSpider, Rule
from scrapy.spider import BaseSpider
from scrapy.selector import HtmlXPathSelector
from scrapy.contrib.linkextractors.sgml import SgmlLinkExtractor
from scrapy.selector import HtmlXPathSelector
from scrapy.item import Item, Field
from scrapy.http import Request
import re
import os
import requests
import logging
log = logging.getLogger(__name__)

class Vote(Item):
    url = Field()
    congress = Field()
    name = Field()
    number = Field()
    time = Field()
    description = Field()
    yes_count = Field()
    no_count = Field()
    abstain_count = Field()
    session = Field()
    data = Field()

class SenateSpider(CrawlSpider):
    name = "senate"
    allowed_domains = ['www.senate.gov', 'senate.gov']
    start_urls = ["http://www.senate.gov/pagelayout/legislative/a_three_sections_with_teasers/votes.htm"]
    rules = [
        Rule(SgmlLinkExtractor(allow=["/legislative/LIS/roll_call_lists/vote_menu_\d+_\d+.htm"])),
        Rule(SgmlLinkExtractor(allow=["/legislative/LIS/roll_call_lists/roll_call_vote_cfm.cfm\?congress=\d+\&session=\d+\&vote=\d+"]), 'parse_senate')
    ]

    def parse_links(self, response):
        x = HtmlXPathSelector(response)
        urls = x.select('//td[@class="contenttext"]/a/@href').extract()
        requests = []
        for url in urls:
            requests.append(Request(url="http://www.senate.gov" + url))
        return requests

    def parse_senate(self, response):
        url = response.url
        x = HtmlXPathSelector(response)
        content = x.select('//tr/td[@class="contenttext"]/text()').extract()
        congress = re.findall("\d+",url)[0]
        session = re.findall("\d+",url)[1]
        name = content[3]
        number = re.findall("\d+",url)[2]
        time = content[5]
        description = content[9]
        yes_count = content[11]
        no_count = content[13]
        abstain_count = content[15]

        yes_nof = x.select('//tr/td[@class="contenttext"]/b/text()').extract()
        yes_no = []
        for i in xrange(8,len(yes_nof)):
            if yes_nof[i]=="YEAs ---":
                break
            yes_no.append(yes_nof[i])
        sens = []
        for i in xrange(16,len(content)):
            if content[i]=="\n            ":
                sens.append(content[i-1])

        syn = {sens[i] : yes_no[i] for i in xrange(0,len(yes_no))}

        vote = Vote()
        vote['congress'] = congress
        vote['name'] = name
        vote['number'] = number
        vote['time'] = time
        vote['description'] = description
        vote['yes_count'] = yes_count
        vote['no_count'] = no_count
        vote['abstain_count'] = abstain_count
        vote['data'] = syn
        vote['session'] = session

        return vote