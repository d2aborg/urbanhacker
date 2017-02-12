# Science sources

# --- !Ups

insert into sources("section", "url") values('news', 'http://www.comingsoon.net/feed');

# --- !Downs

delete from sources where url = 'http://www.comingsoon.net/feed';
