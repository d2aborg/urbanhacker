# New blog

# --- !Ups

insert into sources(section, url) values('blogs.personal', 'http://ethanjoachimeldridge.info/tech-blog/feed.xml');
insert into sources(section, url) values('blogs.org', 'http://blog.xebia.com/feed/');
insert into sources(section, url) values('blogs.project', 'https://blog.jooq.org/feed/');

# --- !Downs

delete from sources where url in(
    'http://ethanjoachimeldridge.info/tech-blog/feed.xml',
    'http://blog.xebia.com/feed/',
    'https://blog.jooq.org/feed/'
);
