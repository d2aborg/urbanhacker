# make downloadid pk in feeds and add downloadid to articles, remove feedid from both

# --- !Ups

alter table feeds alter column download_id set not null;
alter table feeds add constraint downloads_fk foreign key(download_id) references downloads(id) on update restrict on delete cascade;
create unique index "feeds_download_id_idx" on "feeds" ("download_id");

# --- !Downs

drop index "feeds_download_id_idx";
alter table feeds drop constraint downloads_fk;
alter table feeds alter column download_id drop not null;
