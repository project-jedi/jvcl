<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "utils.php";
  require_once "page_blocks.php";

  StartAccessToDB();
  
  $tpl = new HTML_Template_IT("./"); 

  $edit = false;
  if (array_key_exists("edit", $_GET) && $_GET["edit"]==1)
    $edit = true;

  if ($edit)
    $tpl->loadTemplatefile("unit_edit.tpl.html", true, true);
  else
    $tpl->loadTemplatefile("unit.tpl.html", true, true);
  
  SetCommonFooter($tpl); 
  SetCommonToolbar($tpl);
  
  if (array_key_exists("Id", $_GET))
  {
    $unitInfos = GetUnitInfos($_GET["Id"]);
    if (!is_array($unitInfos))
    {
      if  ($itemInfos == "")
        echo "Item $itemId does not exist in the database";
      else
        echo $itemInfos;
      exit;
    }
  }
  elseif (array_key_exists("Name", $_GET))
  {
    $name = $_GET["Name"];
    if (substr($name, -4) == ".pas")
      $name = str_replace(".pas", ".dtx", $name);
    elseif (substr($name, -4) != ".dtx")
      $name .= ".dtx";
    $infos = GetUnitInfosByName($name);
    if (is_bool($infos))
    {
      echo "Unit ".str_replace(".dtx", ".pas", $name).
           " does not exist in the database. Please check you typed the name correcty.";
      exit;
    }
    elseif (is_string($infos))
    {
      echo $infos;
      exit;
    }
    else
      $unitInfos = $infos;
  }
  elseif (!$edit)
  {
    die("You MUST specify the Name or the Id of the item");
  }
  else
  {
    if (array_key_exists("ProjectId", $_GET))
    {
      $unitInfos["ProjectId"] = $_GET["ProjectId"];
      $unitInfos["Id"] = "";
      $unitInfos["Name"] = "Write here a name";
      $unitInfos["Description"] = "Write here a description";
      $unitInfos["Package"] = "";
      $unitInfos["Status"] = "";
      $unitInfos["Author"] = "";
    }
    else
      die("You must at least specify the Project Id when editing");
  }
    
  $projectInfos = GetProjectInfos($unitInfos["ProjectId"]);
  $tpl->setVariable("PROJECT_NAME", $projectInfos["Name"]);  
  $tpl->setVariable("PROJECT_ID", $projectInfos["Id"]);  

  SetCommonLoginStatus($tpl, array("ProjectId"=>$projectInfos["Id"]));
  if(!$edit)
    SetLastChanged($tpl, $unitInfos["userId"], $unitInfos["LastChange"]);
    
  $unitName = str_replace(".dtx", ".pas", $unitInfos["Name"]);
  $tpl->setVariable("UNIT_ID", $unitInfos["Id"]);
  $tpl->setVariable("UNIT_NAME", $unitName);
  $tpl->setVariable("UNIT_PACKAGE", $unitInfos["Package"]);
  $tpl->setVariable("UNIT_STATUS", $unitInfos["Status"]);
  $tpl->setVariable("UNIT_DESCRIPTION", EncodeString($unitInfos["Description"]));
  $tpl->setVariable("UNIT_AUTHOR", str_replace("\r\n", ", ", EncodeString($unitInfos["Author"])));

  if ($edit)
  {
    // set the cancel block
    if ($unitInfos["Id"] != "")
    {
      $tpl->setCurrentBlock("cancel_unit");
      // this marker MUST NOT be called UNIT_ID or the first replacement above will be reversed to ""
      $tpl->setVariable("CANCEL_UNIT_ID", $unitInfos["Id"]); 
      $tpl->parseCurrentBlock("cancel_unit");
    }
    else
    {
      $tpl->setCurrentBlock("cancel_project");
      // this marker MUST NOT be called PROJECT_ID or the first replacement above will be reversed to ""
      $tpl->setVariable("CANCEL_PROJECT_ID", $unitInfos["ProjectId"]); 
      $tpl->parseCurrentBlock("cancel_project");
    }
  }
  else
  {
    $unitItems = GetItemsInUnit($unitInfos["Id"], "ORDER BY Name");

    if (count($unitItems)>0)
    {    
      foreach ($unitItems as $item)
      {
        // Assign data to the line in the item list 
        $tpl->setCurrentBlock("item_list");
        $tpl->setVariable("ITEM_ID", $item["Id"]);
        
        $name = trim($item["Name"]);
        if ($pos = strpos($name, "@"))
        {
          $name = substr($name, 0, $pos)."<div style=\"margin-left: 25px; margin-top: -2px;\">@".substr($name, $pos+1);
          $name = str_replace("@", "</div><br><div style=\"margin-left: 25px; margin-top: -8px;\">@", $name);
          $name .= "</div>"; 
        }
        
        $tpl->setVariable("ITEM_NAME", $name);
        $tpl->setVariable("ITEM_SUMMARY", EncodeString(GetSummaryFromItem($item)));
        $tpl->parseCurrentBlock("item_list");
      }
    }
    else
    {
      $tpl->setCurrentBlock("no_items");
      $tpl->setVariable("ADD_UNIT_ID", $unitInfos["Id"]);
      $tpl->parseCurrentBlock("no_items");
    }
  }  
   
  $tpl->show();
  
  EndAccessToDB();

?>