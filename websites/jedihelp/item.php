<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "utils.php";
  require_once "page_blocks.php";
  
  function SetBlockVisibility($blockName, $value, &$tpl)
  {
    if ($value == "")
    {
      $tpl->setCurrentBlock("empty_".$blockName);
      $tpl->touchBlock("empty_".$blockName);
      $tpl->parseCurrentBlock("empty_".$blockName);
    }
    else
    {
      $tpl->setCurrentBlock("filled_".$blockName);
      $tpl->touchBlock("filled_".$blockName);
      $tpl->parseCurrentBlock("filled_".$blockName);
    }
  }
  
  function FixQuotes($value)
  {
    $value = UnescapeCharsFromMySql($value);
    $value = str_replace("\"", "&quot;", $value);
    return $value;
  }
  
  StartAccessToDB();
  
  if (array_key_exists("edit", $_GET))
    $edit = ($_GET["edit"] == 1);
  else
    $edit = false; 

  if (array_key_exists("Id", $_GET))
    $itemId = $_GET["Id"];
  else
    $itemId = ""; 
    
  if (is_numeric($itemId))
  {
    $itemInfos = GetItemInfos($_GET["Id"]);
    if (!is_array($itemInfos))//(is_string($itemInfos))
    {
      if  ($itemInfos == "")
        echo "Item $itemId does not exist in the database";
      else
        echo $itemInfos;
      exit;
    }

    $unitInfos = GetUnitInfos($itemInfos["UnitId"]);
  }
  else
  {
    $itemInfos = array();
    $unitInfos = array();
    if (!$edit)
    {
      // we may have a name here, so that the user can access an item directly
      if (array_key_exists("Name", $_GET))
      {
        $name = $_GET["Name"];
        $infos = GetItemInfosByName($name);
        if (is_bool($infos))
        {
          echo "Item $name does not exist in the database. Please check you typed the name correcty.";
          exit;
        }
        elseif (is_string($infos))
        {
          echo $infos;
          exit;
        }
        else
        {
          $itemInfos = $infos;
          $itemId = $itemInfos["Id"];
          $unitInfos = GetUnitInfos($itemInfos["UnitId"]);
        }
      }
      else
      {
        echo "Error, the Id or the Name of the item MUST be indicated when not editing";
        exit;
      }
    }
    else
    {
      // if we are editing without an ItemId, then we MUST have the Unit Id
      if (array_key_exists("UnitId", $_GET))
      {
        $unitInfos = GetUnitInfos($_GET["UnitId"]);
      }
      else
      {
        echo "Error, the Unit Id MUST be indicated when adding an item.";
        exit;
      }
    }
  }

  $tpl = new HTML_Template_IT("./"); 

  if ($edit)
    $tpl->loadTemplatefile("item_edit.tpl.html", true, true);
  else
    $tpl->loadTemplatefile("item.tpl.html", true, true);
    
  SetCommonLoginStatus($tpl, array("UnitId"=>$unitInfos["Id"]));
  SetCommonToolbar($tpl);
  SetCommonFooter($tpl);
  if (!$edit)
    SetLastChanged($tpl, $itemInfos["userId"], $itemInfos["LastChange"]); 
  
  $tpl->setVariable("UNIT_NAME", str_replace(".dtx", ".pas", $unitInfos["Name"]));
  $tpl->setVariable("UNIT_ID", $unitInfos["Id"]); 
  
  if (is_numeric($itemId))
  {
    SetItemImage($tpl, $itemInfos["Name"]);
    $tpl->setVariable("ITEM_NAME", $itemInfos["Name"]);
    $tpl->setVariable("ITEM_ID", $itemId);  
    $baseclass = trim(substr($itemInfos["Name"], 0, strpos($itemInfos["Name"],".")));
    if ($baseclass=="")
    {
      $baseclass=$itemInfos["Name"];
    } 
  }
    
  if (!$edit)
  {  
    // Assign data to the Summary block
    if ($itemInfos["Summary"] != "")
    { 
      $tpl->setCurrentBlock("summary");
      $tpl->setVariable("SUMMARY", FormatEndLines(ProcessExtLinks($itemInfos["Summary"])));
      $tpl->parseCurrentBlock("summary");
    }
     
    // Assign data to the Parameters block
    if ($itemInfos["Parameters"] != "")
    { 
      $tpl->setCurrentBlock("parameters");
      $tpl->setVariable("PARAMETERS", FormatEndLines($itemInfos["Parameters"]));
      $tpl->parseCurrentBlock("parameters");
    }
     
    // Assign data to the Return Value block
    if ($itemInfos["ReturnValue"] != "")
    { 
      $tpl->setCurrentBlock("return_value");
      $tpl->setVariable("RETURN_VALUE", FormatEndLines($itemInfos["ReturnValue"]));
      $tpl->parseCurrentBlock("return_value");
    }
     
    // Assign data to the Description block
    if ($itemInfos["Description"] != "")
    {
      $tpl->setCurrentBlock("description");
      $tpl->setVariable("DESCRIPTION", FormatEndLines(trim($itemInfos["FormattedDescription"])));
      $tpl->parseCurrentBlock("description");
    }
     
    // Assign data to the Extras block
    if ($itemInfos["Extras"] != "")
    { 
      $tpl->setCurrentBlock("extras");
      $tpl->setVariable("EXTRAS", FormatEndLines(ProcessExtras($itemInfos["Extras"], $baseclass)));
      $tpl->parseCurrentBlock("extras");
    }
     
    // Assign data to the See Also block
    if ($itemInfos["SeeAlsoUndefined"] != "" or
        $itemInfos["SeeAlsoInternal"] != "" or
        $itemInfos["SeeAlsoExternal"] != "")
    {
      $tpl->setCurrentBlock("see_also");
  
      $seeAlsoInternalEmpty = true;
      // build the see also information
      if ($itemInfos["SeeAlsoInternal"] != "")
      {
        // start with the internal: the value in the DB is a comma separated list 
        // of IDs, so we get the required infos from the database
        $sa_items = GetListOfItems($itemInfos["SeeAlsoInternal"]);
        foreach ($sa_items as $sa_item)
        {
          $tpl->setCurrentBlock("see_also_internal");
          if ($sa_item == $sa_items[0])
            $tpl->setVariable("SEPARATOR", "");
          else
            $tpl->setVariable("SEPARATOR", ", ");
            
          $tpl->setVariable("SEE_ALSO_NAME", RemoveBaseClass(trim($sa_item["Name"]), $baseclass));
          $tpl->setVariable("SEE_ALSO_ID", $sa_item["Id"]);
          $tpl->parseCurrentBlock("see_also_internal");
          
          $seeAlsoInternalEmpty = false;
        }
      }  
      
      $seeAlsoOthers = "";
      // add the undefined and external  
      if ($itemInfos["SeeAlsoUndefined"] != "")
      {
        if (!$seeAlsoInternalEmpty)
          $seeAlsoOthers .= ", ";
        $seeAlsoOthers .= $itemInfos["SeeAlsoUndefined"];
      }
      
      if ($itemInfos["SeeAlsoExternal"] != "")
      {
        if ($seeAlsoOthers != "")
          $seeAlsoOthers .= ", ";
        $seeAlsoOthers .= $itemInfos["SeeAlsoExternal"];
      }
  
      $tpl->setVariable("SEE_ALSO_OTHERS", FormatEndLines($seeAlsoOthers));
      $tpl->parseCurrentBlock("see_also");
    }

    // Assign data to the JVCL Info block
    if ($itemInfos["JVCLInfo"] != "")
    { 
      $tpl->setCurrentBlock("jvcl_info");
      $tpl->setVariable("JVCL_INFO", FormatEndLines($itemInfos["JVCLInfo"]));
      $tpl->parseCurrentBlock("jvcl_info");
    }
  }
  else
  {
    $seeAlsoList = "";
    if (is_numeric($itemId))
    {
      // build the see also information
      if ($itemInfos["SeeAlsoInternal"] != "")
      {
        // start with the internal: the value in the DB is a comma separated list 
        // of IDs, so we get the required infos from the database
        $sa_items = GetListOfItems($itemInfos["SeeAlsoInternal"]);
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
      if ($itemInfos["SeeAlsoUndefined"] != "")
      {
        if ($seeAlsoList!="")
          $seeAlsoList .= ", ";
        $seeAlsoList .= $itemInfos["SeeAlsoUndefined"];
      }
        
      if ($itemInfos["SeeAlsoExternal"] != "")
      {
        if ($seeAlsoList != "")
          $seeAlsoList .= ", ";
        $seeAlsoList .= $itemInfos["SeeAlsoExternal"];
      }
      
      // Set the cancel button
      $tpl->setCurrentBlock("cancel_item");
      // this marker MUST NOT be called ITEM_ID or the first replacement above will be reversed to ""
      $tpl->setVariable("CANCEL_ITEM_ID", $itemInfos["Id"]); 
      $tpl->parseCurrentBlock("cancel_item");
    }
    else
    {
      $tpl->setCurrentBlock("cancel_unit");
      // this marker MUST not be called UNIT_ID or the first replacement above will be reversed to ""
      $tpl->setVariable("CANCEL_UNIT_ID", $unitInfos["Id"]);
      $tpl->parseCurrentBlock("cancel_unit");
      
      // build a "blank" item
      $itemInfos["Summary"] = "Write here a summary (1 line)";
      $itemInfos["Parameters"] = "";
      $itemInfos["ReturnValue"] = "";
      $itemInfos["Description"] = "Write here a description";
      $itemInfos["Extras"] = "";
      $seeAlsoList = "List here other properties, methods (comma seperated) ".
                     "Remove the 'See Also' section if there are no references";
      $itemInfos["JVCLInfo"] = "";
    }
    
    // Assign data to the various variables
    $tpl->setVariable("SUMMARY", FixQuotes($itemInfos["Summary"]));
    $tpl->setVariable("PARAMETERS", $itemInfos["Parameters"]);
    $tpl->setVariable("RETURN_VALUE", FixQuotes($itemInfos["ReturnValue"]));
    $tpl->setVariable("DESCRIPTION", $itemInfos["Description"]);
    $tpl->setVariable("EXTRAS", FixQuotes($itemInfos["Extras"]));
    $tpl->setVariable("SEE_ALSO_LIST", FixQuotes($seeAlsoList));
    $tpl->setVariable("JVCL_INFO", FixQuotes($itemInfos["JVCLInfo"]));
      
    // set the visibility of the different parts
    SetBlockVisibility("summary", $itemInfos["Summary"], $tpl);
    SetBlockVisibility("parameters", $itemInfos["Parameters"], $tpl);
    SetBlockVisibility("return_value", $itemInfos["ReturnValue"], $tpl);
    SetBlockVisibility("description", $itemInfos["Description"], $tpl);
    SetBlockVisibility("extras", $itemInfos["Extras"], $tpl);
    SetBlockVisibility("see_also", $seeAlsoList, $tpl);
    SetBlockVisibility("jvcl_info", $itemInfos["JVCLInfo"], $tpl);
  }
   
  $tpl->show();
  
  EndAccessToDB();

?>
