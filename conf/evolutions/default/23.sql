# Add FOSS news sources

# --- !Ups

insert into sources(section, url, active) values('news', 'http://feeds.feedburner.com/ItsFoss', true);
insert into sources(section, url, active) values('news', 'http://feeds.feedburner.com/d0od', true);
insert into sources(section, url, active) values('news', 'https://fedoramagazine.org/feed/', true);
insert into sources(section, url, active) values('news', 'http://www.linuxtoday.com/biglt.rss', true);
insert into sources(section, url, active) values('news', 'http://www.linuxinsider.com/perl/syndication/rssfull.pl', true);
insert into sources(section, url, active) values('news', 'http://lxer.com/module/newswire/headlines.rss', true);
insert into sources(section, url, active) values('news', 'http://www.phoronix.com/rss.php', true);
insert into sources(section, url, active) values('news', 'https://lwn.net/headlines/newrss', true);
insert into sources(section, url, active) values('news', 'https://www.linux.com/feeds/rss', true);

insert into sources(section, "group", url, active) values('news', 'reddit', 'https://www.reddit.com/r/kernel/', true);

# --- !Downs

delete from sources where url = 'https://www.reddit.com/r/kernel/';

delete from sources where url = 'https://www.linux.com/feeds/rss';
delete from sources where url = 'https://lwn.net/headlines/newrss';
delete from sources where url = 'http://www.phoronix.com/rss.php';
delete from sources where url = 'http://lxer.com/module/newswire/headlines.rss';
delete from sources where url = 'http://www.linuxinsider.com/perl/syndication/rssfull.pl';
delete from sources where url = 'http://www.linuxtoday.com/biglt.rss';
delete from sources where url = 'https://fedoramagazine.org/feed/';
delete from sources where url = 'http://feeds.feedburner.com/d0od';
delete from sources where url = 'http://feeds.feedburner.com/ItsFoss';
