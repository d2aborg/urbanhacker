# Add downloadId to feeds

# --- !Ups

alter table feeds add column "download_id" BIGINT;

# --- !Downs

alter table feeds drop column "download_id";
