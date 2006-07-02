<?php
  if (realpath(__FILE__) == realpath($_SERVER["SCRIPT_FILENAME"]))
    header("Location: http://".$_SERVER["HTTP_HOST"].dirname($_SERVER["PHP_SELF"])."/index.php");

require_once("conf.php");
require_once("utils.php");

// internal counter for Start and End Access to db
$db_AccessCounter = 0;

// link to database
$db_link = 0;

function ConnectToDB()
{
  global $db_host, $db_name, $db_user, $db_pwd, $db_link; 
  
	$db_link = mysql_connect($db_host, $db_user, $db_pwd) or 
    die("Could not connect: ".mysql_error());
	mysql_select_db($db_name) or die("Could not select database");
}

function DisconnectFromDB()
{
  global $db_link;
    
	mysql_close($db_link);
}

function StartAccessToDB()
{
  global $db_AccessCounter;
    
	if ($db_AccessCounter == 0)
		ConnectToDB();
	$db_AccessCounter++;
}

function EndAccessToDB()
{
  global $db_AccessCounter;
    
	$db_AccessCounter--;
	if ($db_AccessCounter == 0)
		DisconnectFromDB();
}

function GetUserInfos($username)
{
  StartAccessToDB();
  
  $request = "SELECT * FROM jh_users WHERE username=\"$username\"";
  $reqResult = mysql_query($request);

  if (!$reqResult)
    $result = "Error in ".$request.": ".mysql_error();
  else
  {
    if ($row = mysql_fetch_array($reqResult, MYSQL_ASSOC))
      $result = $row;
    else
      $result = array();
  }
  
  EndAccessToDB();
  return $result;
}

function GetUserInfosById($userId)
{
  StartAccessToDB();
  
  $request = "SELECT * FROM jh_users WHERE Id=\"$userId\"";
  $reqResult = mysql_query($request);
  
  $row = mysql_fetch_array($reqResult, MYSQL_ASSOC);
  if (!$row)
    $result = "Error in ".$request.": ".mysql_error();
  else
    $result = $row;
  
  EndAccessToDB();
  return $result;
}

function GetUsersInfos()
{
  StartAccessToDB(); 
  $request = "SELECT * FROM jh_users ORDER BY username";
  $reqResult = mysql_query($request);

  if(!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
  {
    $result = array();
    while ($user = mysql_fetch_array($reqResult, MYSQL_ASSOC))
    {
      $result[] = $user;
    }
  }
    
  EndAccessToDB(); 

  return $result;
}

function GetUsersCount()
{
  StartAccessToDB();
  
  $request = "SELECT COUNT(*) AS User_Count FROM jh_users";
  $reqResult = mysql_query($request);
  
  if (!$reqResult)
    $result = "Error in ".$request.": ".mysql_error();
  else
  {
    $row = mysql_fetch_array($reqResult, MYSQL_ASSOC);
    $result = $row["User_Count"];
  }
  
  EndAccessToDB();
  return $result;
}

function GetAdminUsersId()
{
  StartAccessToDB(); 
  $request = "SELECT Id FROM jh_users WHERE IsAdmin=\"Y\"";
  $reqResult = mysql_query($request);

  if(!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
  {
    $result = array();
    while ($user = mysql_fetch_array($reqResult, MYSQL_ASSOC))
    {
      $result[] = $user["Id"];
    }
  }
    
  EndAccessToDB(); 

  return $result;
}

function AddUser($username, $md5_password, $FullName, $email, $CanUpload, $IsPower, $IsAdmin, $ProjectsList)
{
  StartAccessToDB();
  
  if ($IsAdmin == "Y")
    $IsPower = "Y";
  
  $request = "INSERT INTO jh_users SET ".
             "username=\"$username\", ".
             "password=\"$md5_password\", ".
             "FullName=\"$FullName\", ".
             "email=\"$email\", ".
             "CanUpload=\"$CanUpload\", ".
             "IsPower=\"$IsPower\", ".
             "IsAdmin=\"$IsAdmin\", ".
             "ProjectsList=\"".implode(",", $ProjectsList)."\" "
             ;
  $reqResult = mysql_query($request);
  
  if (!$reqResult)
    $result = "Error in ".$request.": ".mysql_error();
  else
    $result = "";
  
  EndAccessToDB();
  return $result;
}

function ModifyUser($Id, $md5_password, $FullName, $email, $CanUpload, $IsPower, $IsAdmin, $ProjectsList)
{
  StartAccessToDB();
  
  if ($IsAdmin == "Y")
    $IsPower = "Y";
  
  $request = "UPDATE jh_users SET ";
  
  if ($md5_password != "")
    $request .= "password=\"$md5_password\", ";
  
  $request .= "FullName=\"$FullName\", ".
              "email=\"$email\", ".
              "CanUpload=\"$CanUpload\", ".
              "IsPower=\"$IsPower\", ".
              "IsAdmin=\"$IsAdmin\", ".
              "ProjectsList=\"".implode(",", $ProjectsList)."\" ".
              "WHERE Id=$Id "
              ;
  $reqResult = mysql_query($request);
  
  if (!$reqResult)
    $result = "Error in ".$request.": ".mysql_error();
  else
    $result = "";
  
  EndAccessToDB();
  return $result;
}

function ModifyMyDetails($Id, $md5_password, $FullName, $email)
{
  StartAccessToDB();
  
  $request = "UPDATE jh_users SET ";
  
  if ($md5_password != "")
    $request .= "password=\"$md5_password\", ";
  
  $request .= "FullName=\"$FullName\", ".
              "email=\"$email\" ".
              "WHERE Id=$Id "
              ;
  $reqResult = mysql_query($request);
  
  if (!$reqResult)
    $result = "Error in ".$request.": ".mysql_error();
  else
    $result = "";
  
  EndAccessToDB();
  return $result;
}

function DeleteUser($userId)
{
  StartAccessToDB();
  
  $request = "DELETE FROM jh_users WHERE Id=$userId";
  $reqResult = mysql_query($request);
  
  if (!$reqResult)
    $result = "Error in ".$request.": ".mysql_error();
  else
    $result = "";
  
  EndAccessToDB();
  return $result;
}

function UpdateUserActivity($Id, $ActivityKey)
{
  StartAccessToDB();

  $request = "UPDATE jh_users SET ".
             "ActivityKey=\"$ActivityKey\", ".
             "LastActivity=NOW() ".
             "WHERE Id=$Id";
  $reqResult = mysql_query($request);
  if (!$reqResult)
    $result = "Error in ".$request.": ".mysql_error();
  else
    $result = "";
  
  EndAccessToDB();
  return $result;
}

function ResetUserActivity($Id)
{
  StartAccessToDB();

  $request = "UPDATE jh_users SET ".
             "ActivityKey=NULL, ".
             "LastActivity=NULL ".
             "WHERE Id=$Id";
  $reqResult = mysql_query($request);
  if (!$reqResult)
    $result = "Error in ".$request.": ".mysql_error();
  else
    $result = "";
  
  EndAccessToDB();
  return $result;
}

function GetProjectsInfos()
{
  StartAccessToDB(); 
	$request = mysql_query("SELECT * FROM jh_projects");
	$i=0;
	while ($project = mysql_fetch_array($request, MYSQL_ASSOC))
	{
		$result[$i] = $project;
		$i++;
	}
  EndAccessToDB(); 
  mysql_free_result($request);    
	return $result;
}

function GetProjectsNamesListFromIds($idList)
{
  StartAccessToDB();
  
  $request = "SELECT Name FROM jh_projects ";
  if ($idList != "")
    $request .= "WHERE Id IN ($idList)";
    
  $reqResult = mysql_query($request);
  
  if(!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
  {
    $result = array();
    while ($row = mysql_fetch_array($reqResult, MYSQL_ASSOC))
      $result[] = $row["Name"];
  }
    
  EndAccessToDB();
  return $result;
}

function GetProjectIdForItem($itemId)
{
  StartAccessToDB();
  
  $request = "SELECT jh_projects.Id as ProjectId FROM jh_projects, jh_units, jh_items ".
             "WHERE jh_items.Id=$itemId ".
             "AND jh_units.Id=jh_items.UnitId ".
             "AND jh_projects.Id=jh_units.ProjectId";
  $reqResult = mysql_query($request);
  
  if(!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
  {
    $row = mysql_fetch_array($reqResult, MYSQL_ASSOC);
    $result = $row["ProjectId"];
  }
    
  EndAccessToDB();
  return $result;
}

function GetProjectIdForUnit($unitId)
{
  StartAccessToDB();
  
  $request = "SELECT jh_projects.Id as ProjectId FROM jh_projects, jh_units ".
             "WHERE jh_units.Id=$unitId ".
             "AND jh_projects.Id=jh_units.ProjectId";
  $reqResult = mysql_query($request);
  
  if(!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
  {
    $row = mysql_fetch_array($reqResult, MYSQL_ASSOC);
    $result = $row["ProjectId"];
  }
    
  EndAccessToDB();
  return $result;
}

function GetProjectInfos($projectId)
{
  StartAccessToDB();
  $request = "SELECT * FROM jh_projects WHERE Id=$projectId"; 
  $reqResult = mysql_query($request);
  
  if(!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
    $result = mysql_fetch_array($reqResult, MYSQL_ASSOC);

  EndAccessToDB(); 
  return $result;
}

function AddProject($Name, $Description, $ReviewersEmails, $AdminEmail, $SendNotifications, $userId=-1)
{
  StartAccessToDB();
  $request = "INSERT INTO jh_projects SET ".
             "Name=\"$Name\", ".
             "Description=\"$Description\", ".
             "ReviewersEmails=\"$ReviewersEmails\", ".
             "AdminEmail=\"$AdminEmail\", ".
             "SendNotifications=\"$SendNotifications\", ".
             "userId=$userId ";
  $reqResult = mysql_query($request);
  if (!$reqResult)
    $result = "Error in ".$request.": ".mysql_error();
  else
    $result = "";
  EndAccessToDB();
  return $result;
}

function ModifyProject($Id, $Name, $Description, $ReviewersEmails, $AdminEmail, $SendNotifications, $userId=-1)
{
  StartAccessToDB();
  // First, save history
  $request = "INSERT IGNORE INTO jh_projects_history (Id, Name, Description, ReviewersEmails, AdminEmail, SendNotifications, userId, LastChange) ".
             "SELECT Id, Name, Description, ReviewersEmails, AdminEmail, SendNotifications, userId, LastChange ".
             "FROM jh_projects ".
             "WHERE Id=$Id;";
  $reqResult = mysql_query($request);
  if (!$reqResult)
  {
    $result = "Error in ".$request.": ".mysql_error();
  }
  else
  {
    // Then do the update
    $request = "UPDATE jh_projects SET ".
               "Name=\"$Name\", ".
               "Description=\"$Description\", ".
               "ReviewersEmails=\"$ReviewersEmails\", ".
               "AdminEmail=\"$AdminEmail\", ".
               "SendNotifications=\"$SendNotifications\", ".
               "userId=$userId ".
               "WHERE Id=$Id";
    $reqResult = mysql_query($request);
    if (!$reqResult)
      $result = "Error in ".$request.": ".mysql_error();
    else
      $result = "";
  }
  
  EndAccessToDB();
  return $result;
}

function DeleteProject($Id)
{
  StartAccessToDB();
  
  // Get all the units which are in the project and delete them
  $request = "SELECT Id FROM jh_units where ProjectId=$Id";
  $reqResult = mysql_query($request);
  if (!$reqResult)
    $result = $request.": ".mysql_error();
  else
  {
    while ($unit = mysql_fetch_assoc($reqResult))
      DeleteUnitAndItems($unit["Id"]);
    
    // now that all the units have been removed, delete the project itself  
    $request = mysql_query("DELETE FROM jh_projects where Id=$Id");
    if (!$request)
      $result = mysql_error();
    else
      $result = "";
  }
  EndAccessToDB();
  return $result;
}

function AddUnit($unitname, $Description, $Author, $package, $status, $projectId, $userId=-1)
{
  StartAccessToDB();
  
  $request = "INSERT INTO jh_units SET ".
    "ProjectId=$projectId, ".
    "Name=\"$unitname\", ".
    "Description=\"$Description\", ".
    "Author=\"$Author\", ".
    "Package=\"$package\", ".
    "Status=\"$status\", ".      
    "userId=$userId ";
  $reqResult = mysql_query($request);
  
  if (!$reqResult)
    $result = $request."<br>".mysql_error();
  else
    $result = mysql_insert_id();
  
  EndAccessToDB();
  return $result;
}

function ModifyUnit($unitId, $unitName, $Description, $Author, $package, $status, $userId=-1)
{
  StartAccessToDB();

  if (is_null($unitName) || $unitName == "")
    return "Error: The name of a unit cannot be empty";
  
  // save history first
  $request = "INSERT IGNORE INTO jh_units_history (Id, ProjectId, Name, Author, Description, Package, Status, userId, LastChange) ".
             "SELECT Id, ProjectId, Name, Author, Description, Package, Status, userId, LastChange ". 
             "FROM jh_units ".
             "where Id=$unitId;";
  $reqResult = mysql_query($request);
  if (!$reqResult)
  {
    $result = $request."<br>".mysql_error();
  }
  else
  {
    // Now update
    $request = "UPDATE jh_units SET ".
               "userId=$userId, ".
               "Name=\"$unitName\", ";
               
    if (!is_null($Description))
      $request .= "Description=\"$Description\", ";
    if (!is_null($Author))
      $request .= "Author=\"$Author\", ";
    if (!is_null($package))
      $request .= "Package=\"$package\", ";
    if (!is_null($status))
      $request .= "Status=\"$status\", ";
      
    if (substr($request, -2) != ", ")
      return "Error while updating unit, must update at least one field";
    else
      $request = substr($request, 0, -2)." ";  
      
    $request .= "WHERE Id=$unitId";
    $reqResult = mysql_query($request);
    
    if (!$reqResult)
      $result = $request."<br>".mysql_error();
    else
      $result = "";
  }
  
  EndAccessToDB();
  return $result;
}

function DeleteUnitAndItems($id)
{
  StartAccessToDB();
  
  $result = "";
  
  // Start by deleting all the items pointing to the given unit
  if (DeleteItems("Where UnitId=$id") == "")
  {    
    $request = "DELETE FROM jh_units where Id=$id";
    $reqResult = mysql_query($request);
    if (!$reqResult)
      $result = $request."<br>".mysql_error();
    else
      $result = "";
  }
  
  // then remove the unit itself
  $request = "DELETE FROM jh_units where Id=$id";
  $reqResult = mysql_query($request);
  if (!$reqResult)
    $result = $request."<br>".mysql_error();
  else
    $result = "";

  EndAccessToDB();
  return $result;
}

function DeleteUnitAndItemsByUnitName($unitname)
{
  StartAccessToDB();
  
  $result = "";
  
  // first, find the unit by name
  $request = "SELECT Id FROM jh_units where Name=\"$unitname\"";
  $reqResult = mysql_query($request);
  
  if (!$reqResult)
    $result = $request."<br>".mysql_error();
  elseif (mysql_num_rows($reqResult)>0)
  {
    $idarray = mysql_fetch_array($reqResult, MYSQL_ASSOC);
    $id = $idarray["Id"];
    $result = DeleteUnitAndItems($id);
  }
  EndAccessToDB();
  return $result;
}

function UnitExistsByName($unitname)
{
  StartAccessToDB();
  
  $result = "";
  
  // find the unit by name
  $request = "SELECT Id FROM jh_units where Name=\"$unitname\"";
  $reqResult = mysql_query($request);
  
  if (!$reqResult)
    $result = $request."<br>".mysql_error();
  else
    $result = (mysql_num_rows($reqResult)>0);
    
  EndAccessToDB();
  return $result;
}

function GetUnitsInProjectCount($projectId)
{
  StartAccessToDB(); 
  $requestResult = mysql_query("SELECT COUNT(*) FROM jh_units WHERE ProjectId=$projectId");

  if (!$requestResult)
    return $request." - ".mysql_error();

  $row = mysql_fetch_row($requestResult);
  $result = $row[0];

  EndAccessToDB(); 
  mysql_free_result($requestResult);    
  return $result;
}

// Return the Units in the project, retrieving $count units from index $start
function GetUnitsInProject($projectId, $extraSQL="", $start=0, $count=2147483647)
{
  StartAccessToDB(); 
  $request = 
      "SELECT * FROM jh_units ".
      "WHERE ProjectId=$projectId ".
      "$extraSQL ".
      "LIMIT ".sprintf("%d", $start).",".sprintf("%d", $count);  // use sprintf to be sure not to get 1.8E19 
  
  $requestResult = mysql_query($request);
  if (!$requestResult)
    return $request." - ".mysql_error();


  $result = array();
  while ($unit = mysql_fetch_array($requestResult, MYSQL_ASSOC))
  {
    $result[] = $unit;
  }


  EndAccessToDB(); 
  mysql_free_result($requestResult);    
  return $result;
}

function GetTypesInProjectSelectionSQL($projectId)
{
  return "FROM jh_items, jh_units ".
         "WHERE jh_units.ProjectId=$projectId ".
         "AND jh_items.UnitId=jh_units.Id ".
         "AND ((jh_items.Name LIKE 'TJv%') OR (jh_items.Name LIKE 'EJv%')) ".
         "AND jh_items.Name NOT LIKE '%.%' ";
}

function GetTypesInProjectCount($projectId)
{
  StartAccessToDB();
  $request = 
    "SELECT Count(*) ".
    GetTypesInProjectSelectionSQL($projectId); 
  $requestResult = mysql_query($request);
  
  if (!$requestResult)
    return $request." - ".mysql_error();

  $row = mysql_fetch_row($requestResult);
  $result = $row[0];

  EndAccessToDB(); 
  mysql_free_result($requestResult);    
  return $result;
}

// Return the Types in the project, retrieving $count types from index $start
function GetTypesInProject($projectId, $start=0, $count=2147483647)
{
  StartAccessToDB();
  $request = 
    "SELECT jh_items.Id as Id, jh_items.Name as Name, Summary, ".
    "UnitId, jh_units.Name as UnitName ".
    GetTypesInProjectSelectionSQL($projectId).
    "ORDER BY jh_items.Name ".
    "LIMIT ".sprintf("%d", $start).",".sprintf("%d", $count);  // use sprintf to be sure not to get 1.8E19 
  $requestResult = mysql_query($request);
  
  if (!$requestResult)
    return $request." - ".mysql_error();

  $result = array();
  while ($unit = mysql_fetch_array($requestResult, MYSQL_ASSOC))
  {
    $result[] = $unit;
  }

  EndAccessToDB(); 
  mysql_free_result($requestResult);    
  return $result;
}

function GetUnitInfos($unitId)
{
  StartAccessToDB();
  $request = "SELECT * FROM jh_units WHERE Id=$unitId"; 
  $reqResult = mysql_query($request);
  
  if(!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
    $result = mysql_fetch_array($reqResult, MYSQL_ASSOC);

  EndAccessToDB(); 
  return $result;
}

function GetUnitInfosByName($unitName)
{
  StartAccessToDB();
  $request = "SELECT * FROM jh_units WHERE Name=\"$unitName\""; 
  $reqResult = mysql_query($request);
  
  if(!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
  {
    if (!($result = mysql_fetch_array($reqResult, MYSQL_ASSOC)))
      $result = false;
  }

  EndAccessToDB(); 
  return $result;
}

function AddItem($unitId, $Name, $Summary, $Description, $ReturnValue, $SeeAlso, $Parameters, $Extras, $JVCLInfo, $userId=-1)
{
  if ($Name == "")
    return "Error: The Name of the item cannot be empty"; 
  
  StartAccessToDB();

  // Need the baseclass for the call to FormatDescription
  $baseclass = trim(substr($Name, 0, strpos($Name,".")));
  if ($baseclass=="")
  {
    $baseclass=$Name;
  } 

  $request = "INSERT INTO jh_items SET ".
    "Name=\"".EscapeCharsForMySQL(trim($Name))."\", ".
    "UnitId=$unitId, ". 
    "Summary=\"".EscapeCharsForMySQL($Summary)."\", ".
    "Description=\"".EscapeCharsForMySQL($Description)."\", ".
    "FormattedDescription=\"".EscapeCharsForMySQL(FormatDescription($Description, $baseclass))."\", ".
    "ReturnValue=\"".EscapeCharsForMySQL($ReturnValue)."\", ".
    "SeeAlsoUndefined=\"".EscapeCharsForMySQL($SeeAlso)."\", ".
    "Parameters=\"".EscapeCharsForMySQL($Parameters)."\", ".
    "Extras=\"".EscapeCharsForMySQL($Extras)."\", ".
    "JVCLInfo=\"".EscapeCharsForMySQL($JVCLInfo)."\", ".
    "userId=$userId ";
  $reqResult = mysql_query($request);
  
  if (!$reqResult)
    $result = $request."<br>".mysql_error();
  else
    $result = mysql_insert_id();

  EndAccessToDB();
  return $result;
}

function ModifyItem($itemId, $Name, $Summary, $Description, $ReturnValue, $SeeAlso, $Parameters, $Extras, $JVCLInfo, $userId=-1)
{
  StartAccessToDB();
  
  // Need the baseclass for the call to FormatDescription
  $baseclass = trim(substr($Name, 0, strpos($Name,".")));
  if ($baseclass=="")
  {
    $baseclass=$Name;
  } 
  
  // save history first
  $request = "INSERT IGNORE INTO jh_items_history (Id, UnitId, Name, Summary, Description, FormattedDescription, ReturnValue, SeeAlsoUndefined, SeeAlsoInternal, SeeAlsoExternal, Parameters, Extras, JVCLInfo, userId, LastChange) ".
             "SELECT Id, UnitId, Name, Summary, Description, FormattedDescription, ReturnValue, SeeAlsoUndefined, SeeAlsoInternal, SeeAlsoExternal, Parameters, Extras, JVCLInfo, userId, LastChange ".
             "FROM jh_items ".
             "WHERE jh_items.id = $itemId;";
  $reqResult = mysql_query($request);
  if (!$reqResult)
  {
    $result = $request."<br>".mysql_error();
  }
  else
  {
    // then do update
    $request = "UPDATE jh_items SET ".
      "Name=\"".EscapeCharsForMySQL(trim($Name))."\", ".
      "Summary=\"".EscapeCharsForMySQL($Summary)."\", ".
      "Description=\"".EscapeCharsForMySQL($Description)."\", ".
      "FormattedDescription=\"".EscapeCharsForMySQL(FormatDescription($Description, $baseclass))."\", ".
      "ReturnValue=\"".EscapeCharsForMySQL($ReturnValue)."\", ".
      "SeeAlsoInternal=\"\", ".
      "SeeAlsoExternal=\"\", ".
      "SeeAlsoUndefined=\"".EscapeCharsForMySQL($SeeAlso)."\", ".
      "Parameters=\"".EscapeCharsForMySQL($Parameters)."\", ".
      "Extras=\"".EscapeCharsForMySQL($Extras)."\", ".
      "JVCLInfo=\"".EscapeCharsForMySQL($JVCLInfo)."\", ".
      "userId=$userId ".
      "WHERE Id=$itemId";
    $reqResult = mysql_query($request);
    
    if (!$reqResult)
      $result = $request."<br>".mysql_error();
    else
      $result = "";
  }

  EndAccessToDB();
  return $result;
}

function DeleteItems($sqlCondition)
{
  StartAccessToDB();
  // before removing the items from the table, we must first ensure that
  // no other item is pointing to each of them via their SeeAlsoInternal
  // column
  $request = "SELECT Id FROM jh_items $sqlCondition";
  $reqResult = mysql_query($request);
  if (!$reqResult)
  {
    EndAccessToDB();
    return $request."<br>".mysql_error();
  }
  
  while ($item = mysql_fetch_array($reqResult, MYSQL_ASSOC))
  {
    $referingItems = array();
    // find those that may have the item's id at the end of their see also
    $request = "SELECT Id, SeeAlsoInternal FROM jh_items WHERE SeeAlsoInternal LIKE \"%,".$item["Id"]."\"";
    $reqResultAtEnd = mysql_query($request);
    if (!$reqResultAtEnd)
    {
      EndAccessToDB();
      return $request."<br>".mysql_error();
    } 
    while ($itemAtEnd = mysql_fetch_array($reqResultAtEnd, MYSQL_ASSOC))
      $referingItems[] = $itemAtEnd;
      
    // now, add those that may have the item's id at anywhere but at the end of their see also   
    $request = "SELECT Id, SeeAlsoInternal FROM jh_items WHERE SeeAlsoInternal LIKE \"%".$item["Id"].",%\"";
    $reqResultButEnd = mysql_query($request);
    if (!$reqResultButEnd)
    {
      EndAccessToDB();
      return $request."<br>".mysql_error();
    } 
    while ($itemButEnd = mysql_fetch_array($reqResultButEnd, MYSQL_ASSOC))
      $referingItems[] = $itemButEnd;
      
    // now, add those that may have the item's id alone in the field   
    $request = "SELECT Id, SeeAlsoInternal FROM jh_items WHERE SeeAlsoInternal=\"".$item["Id"]."\"";
    $reqResultAlone = mysql_query($request);
    if (!$reqResultAlone)
    {
      EndAccessToDB();
      return $request."<br>".mysql_error();
    } 
    while ($itemAlone = mysql_fetch_array($reqResultAlone, MYSQL_ASSOC))
      $referingItems[] = $itemAlone;
      
    // we now have in $referingItems all the items that are referring to $item
    // so we need to update their SeeAlsoInternal field to remove the reference
    foreach ($referingItems as $referingItem)
    {
      // replace the see also by an empty string and do it for all conditions,
      // that is: either at the end, anywhere else or alone
      $referingItem["SeeAlsoInternal"] = str_replace(",".$item["Id"], "", $referingItem["SeeAlsoInternal"]);
      $referingItem["SeeAlsoInternal"] = str_replace($item["Id"].",", "", $referingItem["SeeAlsoInternal"]);
      if ($referingItem["SeeAlsoInternal"] == $item["Id"])
        $referingItem["SeeAlsoInternal"] = "";
        
      // now, update the refering item in the database
      $request = 
        "UPDATE jh_items ".
        "SET SeeAlsoInternal=\"".$referingItem["SeeAlsoInternal"]."\" ".
        "WHERE Id=".$referingItem["Id"];
        $reqResultUpdate = mysql_query($request);
        if (!$reqResultUpdate)
        {
          EndAccessToDB();
          return $request."<br>".mysql_error();
        }
    }  
  }
  
  // now that everything is clean, we can effectively remove the items matching the condition
  $request = "DELETE FROM jh_items $sqlCondition";
  $reqResult = mysql_query($request);
  if (!$reqResult)
    $result = $request."<br>".mysql_error();
  else
    $result = "";
    
  EndAccessToDB();
  return $result;
}

function UpdateItemSeeAlsos($Id, $internals, $externals, $undefs)
{
  StartAccessToDB();
  
  $request = "UPDATE jh_items ".
    "SET SeeAlsoInternal=\"$internals\", ".
    "SeeAlsoUndefined=\"".EscapeCharsForMySQL($undefs)."\", ".
    "SeeAlsoExternal=\"$externals\" ".
    "WHERE Id=$Id";
   
  $reqResult = mysql_query($request);
  if (!$reqResult)
    $result = $request."<br>".mysql_error();
  else
    $result = "";
  
  EndAccessToDB();
  return $result;
}

function GetItemsByUnitAndName($unitId, $name)
{
  StartAccessToDB();
  
  $request = "SELECT * FROM jh_items ".
    "WHERE UnitId=$unitId ".
    "AND Name=\"$name\"";
  $reqResult = mysql_query($request);
  
  if (!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
  {
    $result = array();
    while ($item = mysql_fetch_array($reqResult, MYSQL_ASSOC))
    {
      $result[] = $item;
    }
  }

  EndAccessToDB(); 
  mysql_free_result($reqResult);    
  return $result;
}

function GetItemsInUnit($unitId, $extraSQL="")
{
  StartAccessToDB();
  $request = "SELECT * FROM jh_items WHERE UnitId=$unitId $extraSQL";
  $reqResult = mysql_query($request);
  if (!$reqResult)
    return "Error with $request: ".mysql_error();

  while ($unit = mysql_fetch_array($reqResult, MYSQL_ASSOC))
  {
    $result[] = $unit;
  }

  EndAccessToDB(); 
  mysql_free_result($reqResult);    
  return $result;
}

function GetItemsByIDs($IDs)
{
  StartAccessToDB(); 
  $request = "SELECT * FROM jh_items WHERE Id IN (".implode(",", $IDs).")";
  $reqResult = mysql_query($request);

  while ($item = mysql_fetch_array($reqResult, MYSQL_ASSOC))
  {
    $result[] = $item;
  }

  EndAccessToDB(); 
  mysql_free_result($reqResult);    
  return $result;
}

function GetItemInfos($itemId)
{
  StartAccessToDB();
  $request = "SELECT * FROM jh_items WHERE Id=$itemId"; 
  $reqResult = mysql_query($request);
  
  if(!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
    $result = mysql_fetch_array($reqResult, MYSQL_ASSOC);

  EndAccessToDB(); 
  return $result;
}

function GetItemInfosByName($itemName)
{
  StartAccessToDB();
  $request = "SELECT * FROM jh_items WHERE Name=\"$itemName\""; 
  $reqResult = mysql_query($request);
  
  if(!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
  {
    if (!($result = mysql_fetch_array($reqResult, MYSQL_ASSOC)))
      $result = false;
  }

  EndAccessToDB(); 
  return $result;
}

function GetListOfItems($itemIdList)
{
  StartAccessToDB();
  $request = "SELECT * FROM jh_items WHERE Id IN ($itemIdList)"; 
  $reqResult = mysql_query($request);
  
  if(!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
  {
    $result = array();
    while ($item = mysql_fetch_array($reqResult, MYSQL_ASSOC))
    {
      $result[] = $item;
    }
  }

  EndAccessToDB();
  mysql_free_result($reqResult);
  return $result;
}

function GetExternalItemsInfos()
{
  StartAccessToDB();
  $request = "SELECT * FROM jh_external_items"; 
  $reqResult = mysql_query($request);
  
  if(!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
  {
    $result = array();
    while ($item = mysql_fetch_array($reqResult, MYSQL_ASSOC))
    {
      $result[] = $item;
    }
  }

  EndAccessToDB();
  mysql_free_result($reqResult);
  return $result;
}

function AddExternalItem($Name, $Description)
{
  StartAccessToDB();
  $request = mysql_query("INSERT INTO jh_external_items SET Name=\"$Name\", Description=\"$Description\"");
  if (!$request)
    $result = mysql_error();
  else
    $result = "";
  EndAccessToDB();
  return $result;
}

function DeleteExternalItem($Id)
{
  StartAccessToDB();
  $request = mysql_query("DELETE FROM jh_external_items where Id=$Id");
  if (!$request)
    $result = mysql_error();
  else
    $result = "";
  EndAccessToDB();
  return $result;
}

function GetUndefinedSAItems()
{
  StartAccessToDB();
  $request = 
    "SELECT * FROM jh_items ".
    "WHERE SeeAlsoUndefined != ''".
    "AND SeeAlsoUndefined NOT LIKE 'List here other properties, methods%'";
  $reqResult = mysql_query($request);
  if (!$reqResult)
    $result = mysql_error();
  else
  {
    while ($item = mysql_fetch_array($reqResult, MYSQL_ASSOC))
    {
      $result[] = $item;
    }
  }
  EndAccessToDB();
  return $result;
}

function GetUndefinedSAItemsIDs()
{
  StartAccessToDB();
  $request = 
    "SELECT Id FROM jh_items ".
    "WHERE SeeAlsoUndefined != ''".
    "AND SeeAlsoUndefined NOT LIKE 'List here other properties, methods%'";
  $reqResult = mysql_query($request);
  if (!$reqResult)
    $result = mysql_error();
  else
  {
    while ($item = mysql_fetch_array($reqResult, MYSQL_ASSOC))
    {
      $result[] = $item["Id"];
    }
  }
  EndAccessToDB();
  return $result;
}

function GetItemsOrderedByName()
{
  StartAccessToDB();
  $reqResult = mysql_query("SELECT Id, Name FROM jh_items ORDER BY Name");
  if (!$reqResult)
    $result = mysql_error();
  else
  {
    while ($item = mysql_fetch_array($reqResult, MYSQL_ASSOC))
    {
      $result[] = $item;
    }
  }
  EndAccessToDB();
  return $result;
}

function GetItemIDsOrderedByName()
{
  StartAccessToDB();
  $reqResult = mysql_query("SELECT Id FROM jh_items ORDER BY Name");
  if (!$reqResult)
    $result = mysql_error();
  else
  {
    while ($item = mysql_fetch_array($reqResult, MYSQL_ASSOC))
    {
      $result[] = $item;
    }
  }
  EndAccessToDB();
  return $result;
}

function GetExternalItemsOrderedByName()
{
  StartAccessToDB();
  $reqResult = mysql_query("SELECT Id, Name FROM jh_external_items ORDER BY Name");
  if (!$reqResult)
    $result = mysql_error();
  else
  {
    $result = array();
    while ($item = mysql_fetch_array($reqResult, MYSQL_ASSOC))
    {
      $result[] = $item;
    }
  }
  EndAccessToDB();
  return $result;
}

function SearchItems($projectList, $search_text, $fields, $searchType, $limitAuthor, $undocOnly, $ResultsLimit = 50)
{
  if ($search_text == "" && $limitAuthor == "" && !$undocOnly)
    return "Nothing to search for. Please enter a sentence to search for.";

  if ($ResultsLimit == "")
    $ResultsLimit = 50;
    
  StartAccessToDB();
  
  $allFields = false;
  if ($fields=="")
  {
    $allFields = true;
    $fields = "Summary, Extras, Description, ReturnValue, SeeAlsoUndefined, Parameters, Name";
  }
  
  if($searchType == "")
    $searchType = 0; 
 
  $MySQLVersion = mysql_get_server_info();
  switch ($searchType)
  {
    case 1:
      // Boolean Mode is only available from 4.0.1
      if (strcmp(substr($MySQLVersion, 0, 5), "4.0.1")>=0)
        $SQLSearchType = " IN BOOLEAN MODE";
      else
      {
        $SQLSearchType = "";
        echo "Warning: Your version of MySQL ($MySQLVersion) does not support Boolean Mode, it is supported from version 4.0.1";
      }
      break;
    case 2:
      
      // Query expansions is only available from 4.1.1
      if (strcmp(substr($MySQLVersion, 0, 5), "4.1.1")>=0)
        $SQLSearchType = " WITH QUERY EXPANSION";
      else
      {
        $SQLSearchType = "";
        echo "Warning: Your version of MySQL ($MySQLVersion) does not support Query Expansion, it is supported from version 4.1.1";
      }
      break;
    default:
      $SQLSearchType = "";
      break;
  }
    
  // Add the items that may match (only if the $fields specifier is not Author)
  if ($fields != "Author")
  {
    $request = "SELECT Id FROM jh_units WHERE ProjectId IN (".$projectList.") ";
    if ($limitAuthor != "")
      $request .= "AND Author LIKE \"%".$limitAuthor."%\" ";
      
    $reqResult = mysql_query($request);
    if (!$reqResult)
    {
      $result = $request.": ".mysql_error();
    }
    else
    {
      while ($item = mysql_fetch_array($reqResult))
        $unitsIdArray[] = $item["Id"];
        
      if (is_array($unitsIdArray))
      {
        $unitsList = implode(",", $unitsIdArray);
        
        $request = 
          "SELECT Id, UnitId, Name, Summary, Extras, Description, MATCH(".$fields.") AGAINST ('".$search_text."') AS Relevance ".
          "FROM jh_items ".
          "WHERE UnitId IN (".$unitsList.") ";
        if ($undocOnly)
          $request .= "AND (Summary LIKE \"Write here a Summary (1 line)\" OR ". 
                      "     Description LIKE \"Write here a description\") ";
        if ($search_text != "")
          $request .=  
            "AND MATCH(".$fields.") AGAINST ('".$search_text."'".$SQLSearchType.") ".
            "ORDER BY Relevance DESC ";
            
        $request .=
          "LIMIT 0, ".$ResultsLimit;
          
        $reqResult = mysql_query($request);
        if (!$reqResult)
        {
          $result = $request.": ".mysql_error();
        }
        else
        {
          $result = array();
          while ($item = mysql_fetch_array($reqResult))
            $result[] = $item;
        }
      }       
    }
  }
  
  // Now, add the units that may match
  if ($allFields)
  {
    $fields = "Name, Description, Author";
  }
  else
  {
    $fieldsList = explode(",", $fields);
    $fields = "";
    foreach ($fieldsList as $field)
    {
      $field = trim($field);
      if ($field == "Name" ||
          $field == "Description" ||
          $field == "Author")
      {
       if ($fields == "")
         $fields = $field;
       else
         $fields .= ", ".$field;
      } 
    }
  }
  
  $request = 
    "SELECT Id, Name, Description, MATCH(".$fields.") AGAINST ('".$search_text."') AS Relevance ".
    "FROM jh_units ".
    "WHERE ProjectId IN (".$projectList.") ";
    
  if ($limitAuthor != "")
    $request .= "AND Author LIKE \"%".$limitAuthor."%\" ";
  if ($undocOnly)
    $request .= "AND Description LIKE \"Write here a description\" ";
  
  if ($search_text != "")  
    $request .=
      "AND MATCH(".$fields.") AGAINST ('".$search_text."'".$SQLSearchType.") ".
      "ORDER BY Relevance DESC ";
      
  $request .=
    "LIMIT 0, ".$ResultsLimit;
          
        
  $reqResult = mysql_query($request);
  if (!$reqResult)
  {
    $result = $request.": ".mysql_error();
  }
  else
  {
    while ($item = mysql_fetch_array($reqResult))
      $result[] = $item;
  }
  
  
  // Finally, sort all this by Relevance
  FieldQSort($result, "Relevance");
  
  // And limit to the actual number of results required
  if (is_array($result))
  {
    $result = array_slice($result, 0, $ResultsLimit);
  }
  
  EndAccessToDB();
  return $result;
}

function AddSubmittedItem($item_id, $unit_id, $name, $summary,  
           $description, $return_value, $see_also_list, $parameters, $extras, $jvcl_info)
{
  if ($name == "")
    return "The Name of the item cannot be empty"; 
  
  StartAccessToDB();
  $request = mysql_query("INSERT INTO jh_submitted_items SET ".
                         "CreatedOn=NOW(), ".
                         "ItemId=\"$item_id\", ".
                         "UnitId=\"$unit_id\", ".
                         "Name=\"".EscapeCharsForMySQL(trim($name))."\", ".
                         "Summary=\"".EscapeCharsForMySQL($summary)."\", ".
                         "Description=\"".EscapeCharsForMySQL($description)."\", ".
                         "ReturnValue=\"".EscapeCharsForMySQL($return_value)."\", ".
                         "SeeAlsoList=\"".EscapeCharsForMySQL($see_also_list)."\", ".
                         "Parameters=\"".EscapeCharsForMySQL($parameters)."\", ".
                         "Extras=\"".EscapeCharsForMySQL($extras)."\", ".
                         "JVCLInfo=\"".EscapeCharsForMySQL($jvcl_info)."\""
                         );
                         
  if (!$request)
    $result = mysql_error();
  else
    $result = "";
  EndAccessToDB();
  return $result;
}

function GetSubmittedItemInfos($itemId)
{
  StartAccessToDB();
  $request = "SELECT * FROM jh_submitted_items WHERE Id=$itemId"; 
  $reqResult = mysql_query($request);
  
  if(!$reqResult)
    $result = "Error with SQL: ".$request."<br>".mysql_error();
  else
    $result = mysql_fetch_array($reqResult, MYSQL_ASSOC);

  EndAccessToDB(); 
  mysql_free_result($reqResult);    
  return $result;
}

function GetSubmittedItemsInfos()
{
  StartAccessToDB(); 
  $request = mysql_query(
                "SELECT * FROM jh_submitted_items ".
                "ORDER BY CreatedOn");
  $result = array();
  while ($item = mysql_fetch_array($request, MYSQL_ASSOC))
    $result[] = $item;

  EndAccessToDB(); 
  return $result;
}

function DeleteSubmittedItem($ItemId)
{
  StartAccessToDB();
  $request = "DELETE FROM jh_submitted_items WHERE Id=$ItemId";
  $reqResult = mysql_query($request);
  
  if (!$reqResult)
    $result = "SQL Error in $request: ".mysql_error();
  else
    $result = "";
  EndAccessToDB();
  return $result;
}

?>
