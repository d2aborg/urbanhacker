# MySQL Schema

# --- !Ups

create table `sources` (
  `id` BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `section` VARCHAR(50) NOT NULL,
  `group` VARCHAR(50),
  `url` VARCHAR(1000) NOT NULL,
  `site_url` TEXT,
  `title` TEXT,
  `timestamp` TIMESTAMP NULL,
  `active` BOOLEAN NOT NULL DEFAULT TRUE
);
create index `sources_group_idx` on `sources` (`group`);
create index `sources_section_idx` on `sources` (`section`);
create index `sources_url_idx` on `sources` (`url`);

create table `downloads` (
  `id` BIGINT AUTO_INCREMENT PRIMARY KEY,
  `source_id` BIGINT NOT NULL,
  `last_modified` TEXT,
  `etag` TEXT,
  `checksum` TEXT NOT NULL,
  `timestamp` TIMESTAMP NOT NULL,
  `content` MEDIUMTEXT NOT NULL
);
create index `downloads_timestamp_idx` on `downloads` (`timestamp`);

create table `feeds` (
  `id` BIGINT AUTO_INCREMENT PRIMARY KEY,
  `source_id` BIGINT NOT NULL,
  `download_id` BIGINT NOT NULL,
  `site_url` TEXT,
  `title` TEXT,
  `last_modified` TEXT,
  `etag` TEXT,
  `checksum` TEXT NOT NULL,
  `timestamp` TIMESTAMP NOT NULL,
  `frequency` DOUBLE NOT NULL,
  `group_frequency` DOUBLE NOT NULL,
  `parse_version` INTEGER NOT NULL
);
create unique index `feeds_download_id_idx` on `feeds` (`download_id`);
create index `feeds_parse_version_idx` on `feeds` (`parse_version`);
create index `feeds_timestamp_idx` on `feeds` (`timestamp`);

create table `articles` (
  `id` BIGINT AUTO_INCREMENT PRIMARY KEY,
  `source_id` BIGINT NOT NULL,
  `feed_id` BIGINT,
  `title` VARCHAR(1000) NOT NULL,
  `link` VARCHAR(1000) NOT NULL,
  `comments_link` TEXT,
  `pub_date` TIMESTAMP NOT NULL,
  `image_source` TEXT,
  `text` TEXT NOT NULL
);
create index `articles_link_idx` on `articles` (`link`);
create index `articles_title_idx` on `articles` (`title`);

alter table `downloads` add constraint `downloads_source_fk` foreign key(`source_id`) references `sources`(`id`) on update RESTRICT on delete CASCADE;

alter table `feeds` add constraint `feeds_download_fk` foreign key(`download_id`) references `downloads`(`id`) on update RESTRICT on delete CASCADE;
alter table `feeds` add constraint `feeds_source_fk` foreign key(`source_id`) references `sources`(`id`) on update RESTRICT on delete CASCADE;

alter table `articles` add constraint `articles_feed_fk` foreign key(`feed_id`) references `feeds`(`id`) on update RESTRICT on delete CASCADE;
alter table `articles` add constraint `articles_source_fk` foreign key(`source_id`) references `sources`(`id`) on update RESTRICT on delete CASCADE;

# --- !Downs

drop table `articles`;
drop table `feeds`;
drop table `downloads`;
drop table `sources`;
