# Update moved sources

# --- !Ups

update sources set url = 'http://gizmodo.com/rss' where url = 'http://feeds.gawker.com/gizmodo/full';
update sources set url = 'http://lifehacker.com/rss' where url = 'http://feeds.gawker.com/lifehacker/full';
update sources set url = 'https://medium.freecodecamp.org/feed' where url = 'https://medium.freecodecamp.com/feed';
update sources set url = 'http://joelgrus.com/feeds/rss.xml' where url = 'http://joelgrus.com/feeds/atom.xml';

# --- !Downs

update sources set url = 'http://joelgrus.com/feeds/atom.xml' where url = 'http://joelgrus.com/feeds/rss.xml';
update sources set url = 'https://medium.freecodecamp.com/feed' where url = 'https://medium.freecodecamp.org/feed';
update sources set url = 'http://feeds.gawker.com/lifehacker/full' where url = 'http://lifehacker.com/rss';
update sources set url = 'http://feeds.gawker.com/gizmodo/full' where url = 'http://gizmodo.com/rss';
