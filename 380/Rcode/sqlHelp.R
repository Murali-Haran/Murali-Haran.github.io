The INNER JOIN keyword selects records that have matching values in both tables.
INNER JOIN Syntax
SELECT column_name(s)
FROM table1
INNER JOIN table2 ON table1.column_name = table2.column_name;

The SQL GROUP BY Statement

The GROUP BY statement is often used with aggregate functions (COUNT, MAX, MIN, SUM, AVG) to group the result-set by one or more columns.
GROUP BY Syntax
SELECT column_name(s)
FROM table_name
WHERE condition
GROUP BY column_name(s)
ORDER BY column_name(s);

