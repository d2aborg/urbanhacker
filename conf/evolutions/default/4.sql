# FeedSource as foreign key in Downloads

# --- !Ups

alter table downloads add column source_id bigint;
update downloads set source_id = sources.id from sources where downloads.url = sources.url;
alter table downloads alter column source_id set not null;
alter table downloads drop column url;
alter table "downloads" add constraint "source_fk" foreign key("source_id") references "sources"("id") on update RESTRICT on delete CASCADE

# --- !Downs

alter table "downloads" drop constraint "source_fk";
alter table downloads add column url varchar;
update downloads set url = sources.url from sources where downloads.source_id = sources.id;
alter table downloads alter column url set not null;
alter table downloads drop column source_id;
