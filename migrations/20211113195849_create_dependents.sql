-- Copyright 2015 Six Colors AB
-- Copyright 2021 Flora.pm development team
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

create materialized view dependents (
  name,
  namespace,
  dependent_id) as
    select distinct p3.name as name, p3.namespace as namespace, p0.package_id as dependent_id
    from "packages" as p0
    inner join "releases" as r1 on r1."package_id" = p0."package_id"
    inner join "package_components" as pc on pc."release_id" = r1."release_id"
    inner join "requirements" as r2 on r2."package_component_id" = pc."release_id"
    inner join "packages" as p3 on p3."package_id" = r2."package_id";

create index on dependents (name, dependent_id);
create unique index on dependents (name, namespace, dependent_id);
