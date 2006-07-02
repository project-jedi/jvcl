<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "utils.php";
  require_once "security.php";
  require_once "page_blocks.php";
  require_once "not_a_power_user.php";
  require_once "no_access_to_project.php";

  if (!IsPower())
    die(GetNotAPowerUserPage());
  
  StartAccessToDB();
  
  if (array_key_exists("Id", $_GET))
    $itemId = $_GET["Id"];
  else
    $itemId = ""; 
  if (is_numeric($itemId))
  {
    $itemInfos = GetSubmittedItemInfos($_GET["Id"]);
    if (is_string($itemInfos))
    {
      echo $itemInfos;
      exit;
    }

    $unitInfos = GetUnitInfos($itemInfos["UnitId"]);
    $originalItemId = $itemInfos["ItemId"];
  }
  else
  {
    echo "Error, the Id of the item MUST be indicated.";
    exit;
  }

  $projectId = GetProjectIdForUnit($unitInfos["Id"]);  
  if (!LoggedUserHasAccessToProject($projectId))
    die (GetNoAccessToProjectPage($projectId));

  $tpl = new HTML_Template_IT("./"); 

  $tpl->loadTemplatefile("admin_review_submitted_item.tpl.html", true, true);
  
  SetCommonLoginStatus($tpl);
  SetAdminToolbar($tpl);
  SetCommonFooter($tpl);
  
  $tpl->setVariable("ITEM_ID", $itemId);  
  $tpl->setVariable("UNIT_ID", $itemInfos["UnitId"]);  
  $tpl->setVariable("ITEM_NAME", $itemInfos["Name"]);
  $baseclass = trim(substr($itemInfos["Name"], 0, strpos($itemInfos["Name"],".")));
  
  // Assign data to the various variables
  $tpl->setVariable("SUMMARY", UnescapeCharsFromMySQL($itemInfos["Summary"]));
  $tpl->setVariable("PARAMETERS", UnescapeCharsFromMySQL($itemInfos["Parameters"]));
  $tpl->setVariable("RETURN_VALUE", UnescapeCharsFromMySQL($itemInfos["ReturnValue"]));
  $tpl->setVariable("DESCRIPTION", UnescapeCharsFromMySQL($itemInfos["Description"]));
  $tpl->setVariable("EXTRAS", UnescapeCharsFromMySQL($itemInfos["Extras"]));
  $tpl->setVariable("SEE_ALSO_LIST", UnescapeCharsFromMySQL($itemInfos["SeeAlsoList"]));
  $tpl->setVariable("JVCL_INFO", UnescapeCharsFromMySQL($itemInfos["JVCLInfo"]));
    
  // Set values from the original item (if any)
  if ($originalItemId > 0)
  {  
    $originalItemInfos = GetItemInfos($originalItemId);
    
    $baseclass = trim(substr($originalItemInfos["Name"], 0, strpos($originalItemInfos["Name"],".")));
    $tpl->setVariable("ORIGINAL_ITEM_ID", $originalItemId);
    $tpl->setVariable("ORIGINAL_ITEM_NAME", $originalItemInfos["Name"]);
    $tpl->setVariable("ORIGINAL_SUMMARY", 
                        FormatEndLines($originalItemInfos["Summary"]));
    $tpl->setVariable("ORIGINAL_PARAMETERS", 
                        FormatEndLines($originalItemInfos["Parameters"]));
    $tpl->setVariable("ORIGINAL_RETURN_VALUE", 
                        FormatEndLines($originalItemInfos["ReturnValue"]));
    $tpl->setVariable("ORIGINAL_DESCRIPTION", 
                        FormatEndLines($originalItemInfos["Description"]));
    $tpl->setVariable("ORIGINAL_EXTRAS", FormatEndLines(ProcessExtras($originalItemInfos["Extras"], $baseclass)));
     
    // build the see also information
    $seeAlsoList = "";
    
    if ($originalItemInfos["SeeAlsoInternal"] != "")
    {
      // start with the internal: the value in the DB is a comma separated list 
      // of IDs, so we get the required infos from the database
      $sa_items = GetListOfItems($originalItemInfos["SeeAlsoInternal"]);
      foreach ($sa_items as $sa_item)
      {
        $tpl->setCurrentBlock("see_also_internal");
        if ($sa_item == $sa_items[0])
          $seeAlsoList .= RemoveBaseClass(trim($sa_item["Name"]), $baseclass);
        else
          $seeAlsoList .= ", ".RemoveBaseClass(trim($sa_item["Name"]), $baseclass);
      }
    }  
    
      
    // add the undefined and external  
    if ($originalItemInfos["SeeAlsoUndefined"] != "")
    {
      if ($seeAlsoList!="")
        $seeAlsoList .= ", ";
      $seeAlsoList .= $originalItemInfos["SeeAlsoUndefined"];
    }
      
    if ($originalItemInfos["SeeAlsoExternal"] != "")
    {
      if ($seeAlsoList != "")
        $seeAlsoList .= ", ";
      $seeAlsoList .= $originalItemInfos["SeeAlsoExternal"];
    }
    
    // Finally, set the variable
    $tpl->setVariable("ORIGINAL_SEE_ALSO_LIST", FormatEndLines($seeAlsoList));

    $tpl->setVariable("ORIGINAL_JVCL_INFO", FormatEndLines($originalItemInfos["JVCLInfo"]));
  }
   
  $tpl->show();
  
  EndAccessToDB();

?>