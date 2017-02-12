# Sources schema

# --- !Ups

create table "sources" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "section" VARCHAR NOT NULL,
    "group" VARCHAR,
    "url" VARCHAR NOT NULL,
    "site_url" VARCHAR,
    "title" VARCHAR
);

# --- !Downs

DROP TABLE "sources";
