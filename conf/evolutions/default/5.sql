# Feeds schema

# --- !Ups

create table "feeds" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "source_id" BIGINT NOT NULL,
    "site_url" VARCHAR,
    "title" VARCHAR,
    "last_modified" VARCHAR,
    "etag" VARCHAR,
    "checksum" VARCHAR NOT NULL,
    "timestamp" TIMESTAMPTZ NOT NULL
);
alter table "feeds" add constraint "source_fk" foreign key("source_id") references "sources"("id") on update RESTRICT on delete CASCADE

# --- !Downs

drop table "feeds";
