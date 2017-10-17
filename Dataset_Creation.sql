SHOW DATABASES;
USE sakila;
DESC us_chronic_disease_indicators_two;

SELECT distinct(Question) FROM us_chronic_disease_indicators_one WHERE Topic LIKE 'Nutrition%' ORDER BY Question ASC;
SELECT * FROM us_chronic_disease_indicators_one WHERE Question LIKE '%fruit%';

SELECT distinct(Question) FROM us_chronic_disease_indicators_one WHERE Question LIKE ('Median daily frequency of%' OR 'Meeting aerobic%') AND Question LIKE '%18 years';


SELECT YearEnd, LocationDesc, Question, DataValue, StratificationCategory1, Stratification1 FROM us_chronic_disease_indicators_two WHERE Question IN
	('Meeting aerobic physical activity guidelines for additional and more extensive health benefits among adults aged >= 18 years',
    'Median daily frequency of fruit consumption among adults aged >= 18 years',
    'Median daily frequency of vegetable consumption among adults aged >= 18 years',
    'Obesity among adults aged >= 18 years')
    AND StratificationCategory1 = 'Overall';

SELECT * FROM us_chronic_disease_indicators_two WHERE 
	YearEnd = 2015 AND
    StratificationCategory1 = 'Overall' AND
    Question = 'Meeting aerobic physical activity guidelines for additional and more extensive health benefits among adults aged >= 18 years';


SELECT YearEnd, LocationDesc, Question, AVG(DataValue) FROM us_chronic_disease_indicators_two 
    WHERE YearEnd = '2015' AND Question = 'Median daily frequency of vegetable consumption among adults aged >= 18 years';
    GROUP BY LocationDesc HAVING count(*) > 1;


# ------------------- 2015 ------------------- #

CREATE TABLE nutrition1
	SELECT YearEnd, LocationDesc, Question, DataValue, StratificationCategory1, Stratification1 FROM us_chronic_disease_indicators_two 
    WHERE Question IN
	('Meeting aerobic physical activity guidelines for additional and more extensive health benefits among adults aged >= 18 years',
    'Median daily frequency of fruit consumption among adults aged >= 18 years',
    'Median daily frequency of vegetable consumption among adults aged >= 18 years',
    'Obesity among adults aged >= 18 years',
    'Healthy weight among adults aged >= 18 years')
    AND StratificationCategory1 = 'Overall'
    AND YearEnd = '2015';

CREATE TABLE nutrition2015
	SELECT YearEnd, COUNT(LocationDesc), LocationDesc, Question, AVG(DataValue) FROM nutrition1
		WHERE Question = 'Obesity among adults aged >= 18 years' GROUP BY LocationDesc
	UNION
	SELECT YearEnd, COUNT(LocationDesc), LocationDesc, Question, AVG(DataValue) FROM nutrition1
		WHERE Question = 'Median daily frequency of fruit consumption among adults aged >= 18 years' GROUP BY LocationDesc
	UNION
    SELECT YearEnd, COUNT(LocationDesc), LocationDesc, Question, AVG(DataValue) FROM nutrition1
		WHERE Question = 'Median daily frequency of vegetable consumption among adults aged >= 18 years' GROUP BY LocationDesc
	UNION
    SELECT YearEnd, COUNT(LocationDesc), LocationDesc, Question, AVG(DataValue) FROM nutrition1
		WHERE Question = 'Meeting aerobic physical activity guidelines for additional and more extensive health benefits among adults aged >= 18 years' GROUP BY LocationDesc;

# ------------------- 2013 ------------------- #

CREATE TABLE nutrition3
	SELECT YearEnd, LocationDesc, Question, DataValue, StratificationCategory1, Stratification1 FROM us_chronic_disease_indicators_two 
    WHERE Question IN
	('Meeting aerobic physical activity guidelines for additional and more extensive health benefits among adults aged >= 18 years',
    'Median daily frequency of fruit consumption among adults aged >= 18 years',
    'Median daily frequency of vegetable consumption among adults aged >= 18 years',
    'Obesity among adults aged >= 18 years')
    AND StratificationCategory1 = 'Overall'
    AND YearEnd = '2013';
    
CREATE TABLE nutrition2013
	SELECT YearEnd, COUNT(LocationDesc), LocationDesc, Question, AVG(DataValue) FROM nutrition3
		WHERE Question = 'Obesity among adults aged >= 18 years' GROUP BY LocationDesc
	UNION
	SELECT YearEnd, COUNT(LocationDesc), LocationDesc, Question, AVG(DataValue) FROM nutrition3
		WHERE Question = 'Median daily frequency of fruit consumption among adults aged >= 18 years' GROUP BY LocationDesc
	UNION
    SELECT YearEnd, COUNT(LocationDesc), LocationDesc, Question, AVG(DataValue) FROM nutrition3
		WHERE Question = 'Median daily frequency of vegetable consumption among adults aged >= 18 years' GROUP BY LocationDesc
	UNION
    SELECT YearEnd, COUNT(LocationDesc), LocationDesc, Question, AVG(DataValue) FROM nutrition3
		WHERE Question = 'Meeting aerobic physical activity guidelines for additional and more extensive health benefits among adults aged >= 18 years' GROUP BY LocationDesc;




CREATE TABLE nutritionALL
	SELECT * FROM nutrition2011
    UNION
    SELECT * FROM nutrition2012
    UNION
    SELECT * FROM nutrition2013
    UNION
    SELECT * FROM nutrition2014
    UNION
    SELECT * FROM nutrition2015;


SELECT * FROM nutrition2013;


