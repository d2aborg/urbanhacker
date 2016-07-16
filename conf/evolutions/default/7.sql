# Articles schema

# --- !Ups

create table "articles" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "source_id" BIGINT NOT NULL,
    "feed_id" BIGINT NOT NULL,
    "title" VARCHAR NOT NULL,
    "link" VARCHAR NOT NULL,
    "comments_link" VARCHAR,
    "pub_date" timestamptz NOT NULL,
    "image_source" VARCHAR,
    "text" VARCHAR NOT NULL
);
alter table "articles" add constraint "feed_fk" foreign key("feed_id") references "feeds"("id") on update RESTRICT on delete CASCADE;
alter table "articles" add constraint "source_fk" foreign key("source_id") references "sources"("id") on update RESTRICT on delete CASCADE;

# --- !Downs

drop table "articles";