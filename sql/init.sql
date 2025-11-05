CREATE DATABASE board;
\c board

CREATE TABLE jobs (

  id            UUID DEFAULT gen_random_uuid(),
  date          BIGINT NOT NULL,

  company       VARCHAR(255) NOT NULL,
  title         VARCHAR(255) NOT NULL,
  description   TEXT NOT NULL,

  country       VARCHAR(100),
  city          VARCHAR(100),

  salary_min    INTEGER,
  salary_max    INTEGER,
  currency      VARCHAR(10),

  external_url  VARCHAR(512) NOT NULL,
  is_remote     BOOLEAN NOT NULL DEFAULT false,
  tags          TEXT[],
  image_url     VARCHAR(512),
  seniority     VARCHAR(50),
  other         TEXT,

  poster_email  VARCHAR(255) NOT NULL,

  is_active     BOOLEAN NOT NULL DEFAULT false

);

ALTER TABLE jobs
ADD CONSTRAINT pk_jobs PRIMARY KEY (id);

-- Indexes for common queries
CREATE INDEX idx_jobs_company ON jobs(company);
CREATE INDEX idx_jobs_is_active ON jobs(is_active);
CREATE INDEX idx_jobs_date ON jobs(date DESC);
