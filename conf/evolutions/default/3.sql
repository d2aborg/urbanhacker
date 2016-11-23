# Add InfoWorld

# --- !Ups

insert into sources(section, url) values('news', 'http://www.infoworld.com/index.rss');

# --- !Downs

delete from sources where url = 'http://www.infoworld.com/index.rss';
