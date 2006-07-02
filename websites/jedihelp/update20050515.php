<?php

require_once "data_access.php";
require_once "utils.php";

StartAccessToDB();


// -----------------------------------------------------------
// Add the CanUpload field in jh_users
// -----------------------------------------------------------
$request = "ALTER TABLE `jh_users` ".
           "ADD `CanUpload` ENUM('Y', 'N') DEFAULT 'N' NOT NULL AFTER `IsAdmin`";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());

$request = "UPDATE `jh_users` ".
           "SET CanUpload = 'Y' ".
           "WHERE IsPower = 'Y' OR IsAdmin = 'Y'";
$reqResult = mysql_query($request);
if (!$reqResult)
  die($request.": ".mysql_error());

echo "Update successful";

EndAccessToDB();

?>