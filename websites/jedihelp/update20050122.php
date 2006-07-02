<?php

require_once "data_access.php";
require_once "utils.php";

StartAccessToDB();


// -----------------------------------------------------------
// Add the userId and LastChange fields in jh_items
// -----------------------------------------------------------
$request = "ALTER TABLE `jh_items` ".
           "ADD `userId` INT DEFAULT '-1' NOT NULL,".
           "ADD `LastChange` TIMESTAMP NOT NULL";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());

// -----------------------------------------------------------
// Add the userId and LastChange fields in jh_units
// -----------------------------------------------------------
$request = "ALTER TABLE `jh_units` ".
           "ADD `userId` INT DEFAULT '-1' NOT NULL,".
           "ADD `LastChange` TIMESTAMP NOT NULL";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());

// -----------------------------------------------------------
// Add the userId and LastChange fields in jh_projects
// -----------------------------------------------------------
$request = "ALTER TABLE `jh_projects` ".
           "ADD `userId` INT DEFAULT '-1' NOT NULL,".
           "ADD `LastChange` TIMESTAMP NOT NULL";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());

// -----------------------------------------------------------
// Add the history table for jh_items
// -----------------------------------------------------------
$request = "CREATE TABLE jh_items_history (".
           "Id int(10) unsigned NOT NULL auto_increment,".
           "UnitId int(10) unsigned NOT NULL default '0',".
           "Name varchar(255) NOT NULL default '',".
           "Summary text,".
           "Description text,".
           "FormattedDescription text,".
           "ReturnValue text,".
           "SeeAlsoUndefined text,".
           "SeeAlsoInternal text,".
           "SeeAlsoExternal text,".
           "Parameters text,".
           "Extras text,".
           "JVCLInfo text NOT NULL,".
           "LastChange timestamp NOT NULL,".
           "userId INT default '-1' NOT NULL,".
           "PRIMARY KEY  (Id,LastChange)".
           ");";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());

// -----------------------------------------------------------
// Add the history table for jh_units
// -----------------------------------------------------------
$request = "CREATE TABLE jh_units_history (".
           "Id int(10) unsigned NOT NULL auto_increment,".
           "ProjectId int(10) unsigned NOT NULL default '0',".
           "Name varchar(255) NOT NULL default '',".
           "Author text NOT NULL,".
           "Description text NOT NULL,".
           "Package varchar(255) NOT NULL default '',".
           "Status varchar(255) NOT NULL default '',".
           "LastChange TimeStamp,".
           "userId INT default '-1' NOT NULL,".
           "PRIMARY KEY  (Id, LastChange)".
           ");";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());


// -----------------------------------------------------------
// Add the history table for jh_projects
// -----------------------------------------------------------
$request = "CREATE TABLE jh_projects_history (".
           "Id tinyint(4) NOT NULL auto_increment,".
           "Name varchar(50) NOT NULL default '',".
           "Description longtext NOT NULL,".
           "ReviewersEmails varchar(255) NOT NULL default '',".
           "AdminEmail varchar(255) NOT NULL default '',".
           "SendNotifications char(2) NOT NULL default '',".
           "LastChange TimeStamp,".
           "userId INT default '-1' NOT NULL,".
           "PRIMARY KEY  (Id,LastChange)".
           ");";
$reqResult = mysql_query($request);
if (!$reqResult)
  die(mysql_error());

echo "Update successful";

EndAccessToDB();

?>