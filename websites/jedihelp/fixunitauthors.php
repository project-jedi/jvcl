<?php

require_once "data_access.php";
require_once "utils.php";

StartAccessToDB();

$unitsInfos = GetUnitsInProject(12);

foreach ($unitsInfos as $unitInfos)
{
  echo "Unit: ".$unitInfos["Name"]."<br>";
  $unit_tmpInfos_request = "SELECT * FROM jh_units_tmp WHERE Name=\"".$unitInfos["Name"]."\"";
  $unit_tmpInfos_res = mysql_query($unit_tmpInfos_request);
  if (!$unit_tmpInfos_res)
    die($unit_tmpInfos_request."<br>".mysql_error());
  else
  {
    $unit_tmpInfos = mysql_fetch_array($unit_tmpInfos_res, MYSQL_ASSOC);
    $update_request = "UPDATE jh_units SET Author=\"".
                      EscapeCharsForMySQL($unit_tmpInfos["Author"])."\" ".
                      "WHERE Id=".$unitInfos["Id"];
    echo $update_request."<br>";
    if (!mysql_query($update_request))
      die($update_request."<br>".mysql_error());
    echo "<br>";
  } 
}

EndAccessToDB();

?>