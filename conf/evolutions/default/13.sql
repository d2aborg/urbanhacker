# Science sources

# --- !Ups

update sources set "group"='digg' where url like 'http://digg.com/channel/%.rss';
update sources set "group"='javaworld' where url like 'http://www.javaworld.com/blog/%/index.rss';

# --- !Downs

update sources set "group"=null where url like 'http://digg.com/channel/%.rss';
update sources set "group"=null where url like 'http://www.javaworld.com/blog/%/index.rss';
