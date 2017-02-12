# Add active flag sources and disbale new scientist for now

# --- !Ups

alter table sources add column active boolean;
update sources set active = (url <> 'https://www.newscientist.com/feed/home?cmpid=RSS%7CNSNS-Home&utm_medium=RSS&utm_source=NSNS&utm_campaign=Home&utm_content=Home');
alter table sources alter column active set not null;

# --- !Downs

alter table sources drop column active;
