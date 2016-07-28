# Science sources

# --- !Ups

insert into sources("section", "url") values('news', 'https://www.newscientist.com/feed/home?cmpid=RSS%7CNSNS-Home&utm_medium=RSS&utm_source=NSNS&utm_campaign=Home&utm_content=Home');
insert into sources("section", "url") values('news', 'http://www.sciencemag.org/rss/news_current.xml');
insert into sources("section", "url") values('news', 'https://www.sciencenews.org/feeds/headlines.rss');

# --- !Downs

delete from sources where url = 'https://www.newscientist.com/feed/home?cmpid=RSS%7CNSNS-Home&utm_medium=RSS&utm_source=NSNS&utm_campaign=Home&utm_content=Home';
delete from sources where url = 'http://www.sciencemag.org/rss/news_current.xml';
delete from sources where url = 'https://www.sciencenews.org/feeds/headlines.rss';
