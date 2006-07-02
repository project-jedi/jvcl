-- create the new columns in the unit table
ALTER TABLE `jh_units` ADD `Author` TEXT NOT NULL AFTER `Name` ,
ADD `Description` TEXT NOT NULL AFTER `Author` ;

ALTER TABLE `jh_units` ADD FULLTEXT (
`Author` ,
`Description`
);

-- fill in the columns from the items table


-- delete the author column in the items table


-- delete the ".dtx" items in the items table
DELETE FROM jh_items WHERE
Name LIKE '%.dtx';