<?php

require_once "data_access.php";
require_once "utils.php";

StartAccessToDB();


// -----------------------------------------------------------
// Add the new columns
// -----------------------------------------------------------
$request = "ALTER TABLE jh_units ".
           "ADD Author TEXT NOT NULL AFTER Name, ".
           "ADD Description TEXT NOT NULL AFTER Author;";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());

// -----------------------------------------------------------
// Add the new indexes
// -----------------------------------------------------------
$request = "ALTER TABLE jh_units ".
           "ADD FULLTEXT `AllFullText` (".
           "Author ,".
           "Description,".
           "Name".
           ");";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());
$request = "ALTER TABLE jh_units ".
           "ADD FULLTEXT `NameFullText` (".
           "Name".
           ");";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());
$request = "ALTER TABLE jh_units ".
           "ADD FULLTEXT `AuthorFullText` (".
           "Author".
           ");";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());
$request = "ALTER TABLE jh_units ".
           "ADD FULLTEXT `DescriptionFullText` (".
           "Description".
           ");";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());


// -----------------------------------------------------------
// Fill in the Description and Author columns
// -----------------------------------------------------------
$request = "SELECT UnitId, Author, Summary, Description ".
           "FROM jh_items ".
           "WHERE Name LIKE '%.pas'";
$reqResult = mysql_query($request);
if (!$reqResult)
  die(mysql_error());
while ($row = mysql_fetch_array($reqResult, MYSQL_ASSOC))
{
  $desc = $row["Summary"];
  if ($desc != "" && $row["Description"] != "")
    $desc .= "<br>\r\n".$row["Description"];
  $desc = EscapeCharsForMySQL($desc);
  $author = EscapeCharsForMySQL($row["Author"]);
    
  $request2 = "UPDATE jh_units ".
              "SET Author=\"".$author."\", ".
              "    Description=\"".$desc."\" ".
              "WHERE Id=".$row["UnitId"]."";
              
  echo $request2."<br>";
              
  $reqResult2 = mysql_query($request2);            
  if (!$reqResult2)
    die($request2.": ".mysql_error());
}

// -----------------------------------------------------------
// Delete the ".pas" items in jh_items
// -----------------------------------------------------------
$request = "DELETE FROM jh_items ".
           "WHERE Name LIKE '%.pas'";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());


// -----------------------------------------------------------
// Delete the Author column in jh_items
// -----------------------------------------------------------
$request = "ALTER TABLE `jh_items` DROP `Author` ;";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());


// -----------------------------------------------------------
// Delete the Author column in jh_submitted_items
// -----------------------------------------------------------
$request = "ALTER TABLE `jh_submitted_items` DROP `Author` ;";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());



EndAccessToDB();

?>