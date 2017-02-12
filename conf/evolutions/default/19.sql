# Change Download content to byte-array

# --- !Ups

delete from downloads;
alter table downloads alter column content type bytea USING content::bytea;
alter table downloads add column encoding VARCHAR;

# --- !Downs

alter table downloads drop encoding;
alter table downloads alter column content type VARCHAR using content::varchar;
