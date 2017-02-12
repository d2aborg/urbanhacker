# Add frequency to feeds

# --- !Ups

alter table feeds add column "frequency" DOUBLE PRECISION NOT NULL DEFAULT 'Infinity';

# --- !Downs

alter table feeds drop column "frequency";
