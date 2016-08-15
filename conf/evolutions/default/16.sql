# make downloadid pk in feeds and add downloadid to articles, remove feedid from both

# --- !ups

alter table feeds alter column download_id set not null;
alter table feeds add constraint downloads_fk foreign key(download_id) references downloads(id) on update restrict on delete cascade;
alter table feeds drop constraint feeds_pkey, add primary key (download_id);

alter table articles add column download_id bigint;
update articles set download_id = (select download_id from feeds where feeds.id = feed_id);
alter table articles alter download_id set not null;
alter table articles drop constraint feed_fk;
alter table articles drop column feed_id;
alter table articles add constraint feed_fk foreign key (download_id) references feeds (download_id) on update restrict on delete cascade;

alter table feeds drop column "id";

# --- !downs

alter table feeds add column id bigserial;

alter table articles drop constraint feed_fk;
alter table articles add column feed_id bigint;
update articles set feed_id = (select id from feeds where feeds.download_id = download_id);
alter table articles alter feed_id set not null;
alter table articles add constraint feed_fk foreign key (feed_id) references feeds (id) on update restrict on delete cascade;
alter table articles drop column download_id;

alter table feeds drop constraint feeds_pkey, add primary key (id);
alter table feeds drop constraint downloads_fk;
alter table feeds alter column download_id drop not null;
