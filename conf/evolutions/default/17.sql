# Add last requested timestamp to sources

# --- !Ups

alter table sources add column timestamp timestamptz;

# --- !Downs

alter table sources drop column timestamp;
