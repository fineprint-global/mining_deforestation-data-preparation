-- Prepended SQL commands --
CREATE EXTENSION postgis;
---

-- Database generated with pgModeler (PostgreSQL Database Modeler).
-- pgModeler  version: 0.9.1-beta
-- PostgreSQL version: 10.0
-- Project Site: pgmodeler.com.br
-- Model Author: ---

-- object: app | type: ROLE --
-- DROP ROLE IF EXISTS app;

-- Prepended SQL commands --
DROP ROLE IF EXISTS app;
-- ddl-end --

CREATE ROLE app WITH 
	LOGIN
	ENCRYPTED PASSWORD 'secret';
-- ddl-end --


-- Database creation must be done outside an multicommand file.
-- These commands were put in this file only for convenience.
-- -- object: vector_mine | type: DATABASE --
-- -- DROP DATABASE IF EXISTS vector_mine;
-- CREATE DATABASE vector_mine
-- 	OWNER = postgres
-- ;
-- -- ddl-end --
-- 

-- object: public.snl_mine | type: TABLE --
-- DROP TABLE IF EXISTS public.snl_mine CASCADE;
CREATE TABLE public.snl_mine(
	id bigserial NOT NULL,
	geometry geometry(POINT) NOT NULL,
	snl_id serial NOT NULL,
	mine_name varchar(100),
	country varchar(100),
	list_of_commodities varchar(250),
	development_stage varchar(50),
	operating_status varchar(50),
	coordinate_accuracy varchar(20),
	known_as text,
	mine_type varchar(50),
	CONSTRAINT snl_mine_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.snl_mine OWNER TO postgres;
-- ddl-end --

-- object: public.snl_mine_area | type: TABLE --
-- DROP TABLE IF EXISTS public.snl_mine_area CASCADE;
CREATE TABLE public.snl_mine_area(
	id bigserial NOT NULL,
	geometry geometry(MULTIPOLYGON),
	id_snl_mine bigint NOT NULL,
	created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	status varchar(10) NOT NULL,
	note text,
	"user" varchar(50) NOT NULL,
	version smallint NOT NULL,
	revision smallint NOT NULL,
	seconds_spent float NOT NULL,
	CONSTRAINT mine_area_pk PRIMARY KEY (id)

);
-- ddl-end --
ALTER TABLE public.snl_mine_area OWNER TO postgres;
-- ddl-end --

-- object: idx_snl_mine_area | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_snl_mine_area CASCADE;
CREATE INDEX idx_snl_mine_area ON public.snl_mine_area
	USING btree
	(
	  id
	);
-- ddl-end --

-- object: idx_snl_mine | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_snl_mine CASCADE;
CREATE INDEX idx_snl_mine ON public.snl_mine
	USING btree
	(
	  id
	);
-- ddl-end --

-- object: snl_mine_fk | type: CONSTRAINT --
-- ALTER TABLE public.snl_mine_area DROP CONSTRAINT IF EXISTS snl_mine_fk CASCADE;
ALTER TABLE public.snl_mine_area ADD CONSTRAINT snl_mine_fk FOREIGN KEY (id_snl_mine)
REFERENCES public.snl_mine (id) MATCH FULL
ON DELETE RESTRICT ON UPDATE CASCADE;
-- ddl-end --

-- object: idx_snlgeom | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_snlgeom CASCADE;
CREATE INDEX idx_snlgeom ON public.snl_mine
	USING gist
	(
	  geometry
	);
-- ddl-end --

-- object: idx_geom_snl_area | type: INDEX --
-- DROP INDEX IF EXISTS public.idx_geom_snl_area CASCADE;
CREATE INDEX idx_geom_snl_area ON public.snl_mine_area
	USING gist
	(
	  geometry
	);
-- ddl-end --


-- Appended SQL commands --
GRANT CONNECT ON DATABASE vector_mine TO app;
GRANT SELECT ON snl_mine TO app;
GRANT SELECT, INSERT, DELETE ON snl_mine_area TO app;

---
