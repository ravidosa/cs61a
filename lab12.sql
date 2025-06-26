.read data.sql


CREATE TABLE number_of_options AS
  SELECT COUNT(DISTINCT meat)
  FROM main_course;


CREATE TABLE calories AS
  SELECT COUNT(*)
  FROM main_course, pies
  WHERE main_course.calories + pies.calories <= 2500;


CREATE TABLE healthiest_meats AS
  SELECT main_course.meat, (main_course.calories + MIN(pies.calories)) as total_calories
  FROM main_course, pies
  GROUP BY meat
  HAVING NOT (main_course.calories + MAX(pies.calories)) > 3000;

