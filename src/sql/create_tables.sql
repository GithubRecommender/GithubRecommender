USE github_recommender;

CREATE TABLE IF NOT EXISTS topic_ontology (
  id   INT          PRIMARY KEY AUTO_INCREMENT,
  label VARCHAR(100) NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS language_ontology (
  id   INT          PRIMARY KEY AUTO_INCREMENT,
  label VARCHAR(100) NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS project (
  id    INT          PRIMARY KEY AUTO_INCREMENT,
  gh_id VARCHAR(100) NOT NULL UNIQUE,

  owner VARCHAR(100) NOT NULL,
  name  VARCHAR(255) NOT NULL,

  description VARCHAR(1000)
);

create TABLE IF NOT EXISTS entitized_project_topics (
  project_id INT,
  topic_id   INT,

  PRIMARY KEY (project_id, topic_id),

  CONSTRAINT fk_pt_project FOREIGN KEY (project_id) REFERENCES project (id) ON DELETE CASCADE,
  CONSTRAINT fk_pt_topic FOREIGN KEY (topic_id) REFERENCES topic_ontology (id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS entitized_project_languages (
  project_id  INT,
  language_id INT,

  PRIMARY KEY (project_id, language_id),
  
  CONSTRAINT fk_pl_project FOREIGN KEY (project_id) REFERENCES project (id) ON DELETE CASCADE,
  CONSTRAINT fk_pl_language FOREIGN KEY (language_id) REFERENCES language_ontology (id) ON DELETE CASCADE
);
