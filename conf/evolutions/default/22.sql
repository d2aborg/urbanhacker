# Delete Medium

# --- !Ups

delete from sources where "group" = 'medium';

# --- !Downs

insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/tech', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/technology', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/artificial-intelligence', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/virtual-reality', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/space', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/social-media', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/self-driving-cars', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/privacy', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/internet-of-things', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/innovation', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/ideas', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/future', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/artificial-intelligence', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/apps', TRUE);
insert into sources("section", "group", "url", "active") values('news', 'medium', 'https://medium.com/feed/tag/bitcoin', TRUE);
