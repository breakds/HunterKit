#! /usr/bin/python
# -*- coding: utf-8 -*-



import urllib
import urllib2
import re
import os
import sys
import time
import pickle
from datetime import datetime
from BeautifulSoup import BeautifulSoup




systems = []



def ScrawlSkill( url ) :
    global systems

    soup = BeautifulSoup( urllib2.urlopen( url ).read() )
    
    # find all the headers
    headers = soup.findAll( 'table' )
    headers = filter( lambda (x) : x.has_key( 'style' ), headers )
    

    systems = []
    
    for header in headers :
        for item in header.contents :
            if item.find( 'a' ) and item.find( 'a' ) != -1 :
                name = item.a['title']
                # get the number of skills in this system
                span = 1
                if 0 != len( item.td.attrs ) :
                    # multiple skill in this system
                    span = int(item.td.attrs[0][1])
                # process every skill
                tr = item
                skills = []
                for i in xrange(span) :
                    row = map( lambda x : x.string.strip() if x.string else "None", tr.findAll('td') )
                    if ( len(row) == 4 ) :
                        try :
                            skills.append( ( row[2], int(row[1]), row[3] ) )
                        except :
                            skills.append( ( row[2], 0, row[3] ) )
                    else :
                        try :
                            skills.append( ( row[1], int(row[0]), row[2] ) )
                        except :
                            skills.append( ( row[1], 0, row[2] ) )
                    tr = tr.findNextSibling( 'tr' )
                systems.append( ( name, skills ) )
    
    with open( 'skills.lisp.data', 'w' ) as fout :
        n = 0
        for every in systems :
            fout.write( '(%d "%s" (' % ( n, every[0].encode('utf-8') ) )
            n = n + 1
            for item in every[1] :
                fout.write( '(%d "%s" "%s") ' % ( item[1],
                                              item[0].encode('utf-8'),
                                              item[2].encode('utf-8') ) )
            fout.write( '))\n' )


def ScrawlArmor( url, part ) :
    global systems

    soup = BeautifulSoup( urllib2.urlopen( url ).read() )
    
    entries = filter( lambda x : x.find( 'img' ) and x.find( 'img' ) != -1,
                      soup.findAll( 'tr' ) )

    armors = []

    for entry in entries :
        row = entry.findAll( 'td' )
        
        # name
        name = row[0].a['title']

        try:

            # rare
            rare = 1
            try :
                rare = int(row[1].string.strip())
            except :
                rare = int(row[1].contents[0].strip())


            # weapon type and gender
            weapon = 'both' # 'saber' and 'archer'
            gender = 'both' # 'male' and 'female'
            
            try :
                tmp = row[2].string.strip()
            except :
                tmp = row[2].contents[0].strip()

            (weapon,gender) = { u'共用' : ( 'both', 'both' ),
                                u'剑士' : ( 'saber', 'both' ),
                                u'射手' : ( 'archer', 'both' ),
                                u'剑士【男】' : ( 'saber', 'male' ),
                                u'射手【男】' : ( 'archer', 'male' ),
                                u'剑士【女】' : ( 'saber', 'female' ),
                                u'射手【女】' : ( 'archer', 'female' ),
                                u'女性' : ( 'both', 'female' ),
                                u'男性' : ( 'both', 'male' ) }[tmp]


            # def
            tmp = row[3].string.strip()
            def_min = 0
            def_max = 0
            if tmp.find('/') == -1 :
                def_min = int(tmp)
                def_max = def_min
            else :
                tmp = tmp.split('/')
                def_min = int(tmp[0])
                def_max = int(tmp[1])


            # holes
            holes = len( filter( lambda x : x == u'O', 
                               row[4].string.strip().split( ' ' ) ) )

            # skill set
            skills = []
            if 12 == len(row) :
                try :
                    for item in row[6].ul.findAll('li') :
                        skill_name = item.a['title']
                        value = 0
                        try :
                            value = int(item.font.string.strip())
                        except :
                            pass
                        res = filter( lambda x : systems[x][0] == skill_name, xrange(len(systems)) )
                        if len(res) > 0 :
                            skills.append( (res[0],value) )
                        else :
                            skills.append( (10,value) )
                except :
                    pass
            elif 11 == len(row) :
                try :
                    for item in row[5].ul.findAll('li') :
                        skill_name = item.a['title']
                        value = 0
                        try :
                            value = int(item.font.string.strip())
                        except :
                            pass
                        res = filter( lambda x : systems[x][0] == skill_name, xrange(len(systems)) )
                        if len(res) > 0 :
                            skills.append( (res[0],value) )
                        else :
                            skills.append( (10,value) )
                except :
                    pass
            else :
                skills = armors[-1][7]

            armors.append( ( name, rare, weapon, gender, def_min, def_max, holes, skills ) )
        except :
            print '[Error]', name, row[2]
            
    

    with open( '%s.lisp.data' % part, 'w' ) as fout :
        n = 0
        for item in armors :
            fout.write( '(:id %d\n' % n )
            n = n + 1
            fout.write( ' :name "%s"\n' % item[0].encode('utf-8') )
            fout.write( ' :rare %d\n' % item[1] )
            fout.write( ' :weapon %s\n' % item[2] )
            fout.write( ' :gender %s\n' % item[3] )
            fout.write( ' :def-min %d\n' % item[4] )
            fout.write( ' :def-max %d\n' % item[5] )
            fout.write( ' :holes %d\n' % item[6] )
            fout.write( ' :skills (' )
            for each in item[7] :
                fout.write( '(%d %d) ' % each )
            fout.write( '))\n' )
    


def ScrawlJewel( url ) :
    
    global systems
    
    soup = BeautifulSoup( urllib2.urlopen(url).read() )
    
    headers = filter( lambda x : x.has_key('width'),
                      soup.findAll( 'table' ) )
    
    jewels = []
    
    for header in headers :
        for item in header.findAll('tr') :
            row = item.findAll( 'td' )
            if 5 == len(row) :

                # name
                name = row[0].a['title']

                # holes
                holes = len( filter( lambda x : x == u'O', 
                                     row[2].string.strip() ) )

                # skills
                skills = []
                for ele in row[1].findAll('font') :
                    value = int(ele.b.string.strip())
                    skill_name = ele.a['title']
                    res = filter( lambda x : systems[x][0] == skill_name, xrange(len(systems)) )
                    if 1 == len(res) :
                        skills.append( ( res[0], value ) )
                    else :
                        skills.append( ( 10, value ) )
                jewels.append( ( name, holes, skills ) )
    
    
    with open( 'jewel.lisp.data', 'w' ) as fout :
        n = 0
        for item in jewels :
            fout.write( '(:id %d\n' % n )
            n = n + 1
            fout.write( ' :name "%s"\n' % item[0].encode('utf-8') )
            fout.write( ' :holes %d\n' % item[1] )
            fout.write( ' :skills (' )
            for each in item[2] :
                fout.write( '(%d %d) ' % each )
            fout.write( '))\n' )
    
    

if __name__ == '__main__' :

    ScrawlSkill( 'http://mhp3wiki.duowan.com/%E6%8A%80%E8%83%BD?variant=zh-hans' )

    print '[ ok ] skills'

    ScrawlArmor( 'http://mhp3wiki.duowan.com/%E5%A4%B4%E9%98%B2%E5%85%B7?variant=zh-hans', 'head' )
    
    print '[ ok ] head'

    ScrawlArmor( 'http://mhp3wiki.duowan.com/%E8%83%B4%E9%98%B2%E5%85%B7?variant=zh-hans', 'chest' )

    print '[ ok ] chest'

    ScrawlArmor( 'http://mhp3wiki.duowan.com/%E8%85%95%E9%98%B2%E5%85%B7?variant=zh-hans', 'hand' )

    print '[ ok ] hand'

    ScrawlArmor( 'http://mhp3wiki.duowan.com/%E8%85%B0%E9%98%B2%E5%85%B7?variant=zh-hans', 'waist' )

    print '[ ok ] waist'

    ScrawlArmor( 'http://mhp3wiki.duowan.com/%E8%84%9A%E9%98%B2%E5%85%B7?variant=zh-hans', 'foot' )

    print '[ ok ] foot'

    ScrawlJewel( 'http://mhp3wiki.duowan.com/%E8%A3%85%E9%A5%B0%E7%8F%A0?variant=zh-hans' )

    print '[ ok ] Jewel'





    
    

