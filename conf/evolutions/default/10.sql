# Add frequency to feeds

# --- !Ups

alter table feeds add column "parse_version" INTEGER NOT NULL default 0;
alter table feeds add column "group_frequency" DOUBLE PRECISION NOT NULL default 'Infinity';

# --- !Downs

alter table feeds drop column "group_frequency";
alter table feeds drop column "parse_version";
