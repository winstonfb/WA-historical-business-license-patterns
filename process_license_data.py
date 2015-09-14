""" Scrape NAICS, opening date and closing date from text of WA business license. 

    Arguments:
        1 - target_folder: location of directory with text files.
        2 - outfile: name of output file.

    Note:
        Assumes that all text files in target_folder are business license texts.

"""

import sys
import re
from os import listdir

if __name__ == '__main__':

    def get_naics(record):
        try:
            naics_raw = re.search('(NAICS CODE\s?:\s?\S+)',record)
            naics = re.search('\d\d\d\d\d\d',naics_raw.group(0))
            return naics.group(0)
        except: return 'null'

    def get_open_date(record):
        try:
            open_date_raw = re.search('(OPENED\s?:\s?\S+)',record)
            open_date = re.search('\d\d/\d\d/\d\d\d\d',open_date_raw.group(0))
            return open_date.group(0)
        except: return 'null'

    def get_close_date(record):
        try:
            close_date_raw = re.search('(CLOSED\s?:\s?\S+)',record)
            close_date = re.search('(\d|O)\d/\d\d/\d\d\d\d|OPEN',close_date_raw.group(0))
            if len(close_date.group(0))>4:
                return close_date.group(0).replace('O','0')
            else:
                return close_date.group(0)
        except: return 'null'

    target_folder = sys.argv[1]
    licenses = [f for f in listdir(target_folder) if '.txt' in f]

    """ Open each record's text and pull out the naics and open/close date. """
    record_data = {}
    for l in licenses:
        in_file = open(target_folder+'/'+l,'r')
        record = in_file.read()
        in_file.close()
        
        data_fields = {key: value for (key, value) in 
                [("naics",get_naics(record)), ("opened",get_open_date(record)),
                    ("closed",get_close_date(record))]}
        record_data[l] = data_fields

    """ Write the results to outfile. """

    outfile = open(sys.argv[2],'wb')
    outfile.write('UBI\tNAICS\tOPEN DATE\tCLOSE DATE\n')
    for ubi in record_data:
        outfile.write('{0}\t{1}\t{2}\t{3}\n'.format(ubi[:-4],record_data[ubi]['naics'],record_data[ubi]['opened'],record_data[ubi]['closed']))
    outfile.close()

