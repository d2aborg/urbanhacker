# Update timestamp in downloads to timestamptz

# --- !Ups

alter table downloads alter column "timestamp" type timestamptz;

# --- !Downs

alter table downloads alter column "timestamp" type timestamp;
