# Downloads schema

# --- !Ups

create table "downloads" (
    "id" BIGSERIAL NOT NULL PRIMARY KEY,
    "url" VARCHAR NOT NULL,
    "last_modified" VARCHAR,
    "etag" VARCHAR,
    "checksum" VARCHAR NOT NULL,
    "timestamp" TIMESTAMP NOT NULL,
    "content" VARCHAR NOT NULL
);

# --- !Downs

DROP TABLE "downloads";
