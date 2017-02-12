# Add InfoWorld

# --- !Ups

insert into sources(section, url, active) values('news', 'http://www.infoworld.com/index.rss', true);

# --- !Downs

delete from sources where url = 'http://www.infoworld.com/index.rss';
