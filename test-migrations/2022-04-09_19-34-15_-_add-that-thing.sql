CREATE TABLE that_thing IF NOT EXISTS (
  id bigserial PRIMARY KEY,
  name text NOT NULL,
  extra_field text,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL
);

-- DOWN

DROP TABLE that_thing CASCADE;