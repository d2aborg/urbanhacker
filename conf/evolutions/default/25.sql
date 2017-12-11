# Update moved sources

# --- !Ups

update sources set url = 'https://movingfulcrum.com/rss/' where url = 'http://movingfulcrum.com/rss/';

# --- !Downs

update sources set url = 'http://movingfulcrum.com/rss/' where url = 'https://movingfulcrum.com/rss/';
