# Add frequency to feeds

# --- !Ups

delete from feeds;
alter table feeds add column "frequency" DOUBLE PRECISION NOT NULL;

# --- !Downs

alter table feeds drop column "frequency";
