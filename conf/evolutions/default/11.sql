# Add frequency to feeds

# --- !Ups

create index "articles_link_idx" on "articles" ("link");
create index "articles_title_idx" on "articles" ("title");
create index "feeds_parse_version_idx" on "feeds" ("parse_version");
create index "feeds_timestamp_idx" on "feeds" ("timestamp");
create index "sources_group_idx" on "sources" ("group");
create index "sources_section_idx" on "sources" ("section");
create index "sources_url_idx" on "sources" ("url");
create index "downloads_timestamp_idx" on "downloads" ("timestamp");

# --- !Downs

drop index "downloads_timestamp_idx";
drop index "sources_group_idx";
drop index "sources_section_idx";
drop index "sources_url_idx";
drop index "feeds_parse_version_idx";
drop index "feeds_timestamp_idx";
drop index "articles_title_idx";
drop index "articles_link_idx";
