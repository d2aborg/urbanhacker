# Add Google Blog and delete Medium Design blogs as they contain lots of non-coding stuff

# --- !Ups

insert into sources(section, url, active) values('blogs', 'https://blog.google/rss/', true);
delete from sources where url = 'https://medium.com/feed/tag/design';

# --- !Downs

delete from sources where url = 'https://blog.google/rss/';
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/design', TRUE);
