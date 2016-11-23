# Change Download content to byte-array

# --- !Ups

delete from downloads;
alter table downloads modify content mediumblob;
alter table downloads add encoding text;

# --- !Downs

alter table downloads drop encoding;
alter table downloads modify content mediumtext;
