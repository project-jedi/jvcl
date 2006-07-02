<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "page_blocks.php";
  
  // ---------------------------------------------------------------
  // Process the pages lists
  // ---------------------------------------------------------------
  function ProcessPagesLists($count, &$tpl, $projectId)
  {
    if ($count == 0)
    {
      return array(0,0);
    }
    
    $tpl->setVariable("NAV_PANE_PROJECT_ID", $projectId);
      
    $per_pages_list = array(25, 50, 75, 100);
    
    if (array_key_exists("original_page", $_POST) && $_POST["original_page"] != "")
    {
      $page = $_POST["page"];
      $original_page = $_POST["original_page"];
      $per_pages = $_POST["per_pages"];
      $original_per_pages = $_POST["original_per_pages"];
      
      // we can only change page OR original_page, but not both
      if ($page != $original_page)
      {
      }
      elseif ($per_pages != $original_per_pages)
      {
        // here, we do a bit of calculation so that the first item in
        // the previous page is in the new page, even if the number of
        // items per page has changed.
        $first_in_page_index = $page * $original_per_pages;  // from 0 to $count-1
        $page = ceil($first_in_page_index / $per_pages);
      }
    }
    else
    {
      $page = "1";
      $per_pages = $per_pages_list[0];
    }
    $page_count = ceil($count / $per_pages);
    
    // If user goes decreases the number of items per pages then there is a risk
    // to go over the maximum number of pages. We fix this here.
    if ($page > $page_count)
      $page = $page_count;
    
    // Set the next/prev buttons
    if ($page == 1)
    {
      $tpl->setCurrentBlock("prev_page_disabled");
      $tpl->touchBlock("prev_page_disabled");
      $tpl->parseCurrentBlock("prev_page_disabled");
    }
    else
    {
      $tpl->setCurrentBlock("prev_page");
      $tpl->touchBlock("prev_page");
      $tpl->parseCurrentBlock("prev_page");
    }
    
    if ($page == $page_count)
    {
      $tpl->setCurrentBlock("next_page_disabled");
      $tpl->touchBlock("next_page_disabled");
      $tpl->parseCurrentBlock("next_page_disabled");
    }
    else
    {
      $tpl->setCurrentBlock("next_page");
      $tpl->touchBlock("next_page");
      $tpl->parseCurrentBlock("next_page");
    }
    
    // Fill in the lists
    for ($i=1;$i<=$page_count;$i++)
    {
      $tpl->setCurrentBlock("page_list");
      $tpl->setVariable("VALUE", $i);
      $tpl->setVariable("SELECTED", ($i==$page)?"selected":"");
      $tpl->parseCurrentBlock("page_list");
    }    
      
    for ($i=0;$i<count($per_pages_list);$i++)
    {
      $tpl->setCurrentBlock("per_pages_list");
      $tpl->setVariable("VALUE", $per_pages_list[$i]);
      $tpl->setVariable("SELECTED", ($per_pages_list[$i]==$per_pages)?"selected":"");
      $tpl->parseCurrentBlock("per_pages_list");
    }    
      
    $tpl->setVariable("PAGE", $page);
    $tpl->setVariable("PER_PAGES", $per_pages);
    $result = array(($page-1)*$per_pages, $per_pages);
    return $result;
  }
  
  StartAccessToDB();
  
  $tpl = new HTML_Template_IT("./"); 

  $tpl->loadTemplatefile("browse.tpl.html", true, true);
  
  SetCommonLoginStatus($tpl);
  SetCommonFooter($tpl);

  $projectInfos = GetProjectInfos($_GET["Id"]);
  if (!is_array($projectInfos))
  {
    if ($projectInfos == "")
      die("Project ".$_GET["Id"]." does not exist in the database.");
    else
      die($projectInfos);
    exit;
  }

  SetCommonToolbar($tpl, array("ProjectId"=>$projectInfos["Id"]));
  SetLastChanged($tpl, $projectInfos["userId"], $projectInfos["LastChange"]); 
        
  $tpl->setVariable("PROJECT_ID", $projectInfos["Id"]);
  $tpl->setVariable("PROJECT_NAME", $projectInfos["Name"]);
  $tpl->setVariable("PROJECT_DESCRIPTION", $projectInfos["Description"]);
  
  // ---------------------------------------------------------------
  // Decide to display units or types
  // ---------------------------------------------------------------
  if (array_key_exists("types", $_GET) && $_GET["types"] == 1)
  {
    $limits = ProcessPagesLists(GetTypesInProjectCount($_GET["Id"]), $tpl, $projectInfos["Id"]);
    $projectTypes = GetTypesInProject($_GET["Id"], $limits[0], $limits[1]);
    if (!is_array($projectTypes))
    {
      if ($projectTypes == "")
        die("Project ".$_GET["Id"]." does not exist in the database.");
      else
        die($projectTypes);
    }
    elseif (count($projectTypes)>0)
    {
      $tpl->setVariable("TYPES", "1");
      $tpl->setCurrentBlock("by_types");
      foreach ($projectTypes as $type)
      {
        $unitName = str_replace(".dtx", ".pas", $type["UnitName"]);
        
        // Assign data to the types list block 
        $tpl->setCurrentBlock("type_list");
        $tpl->setVariable("TYPE_ID", $type["Id"]);
        $tpl->setVariable("TYPE_NAME", FormatItemName(trim($type["Name"])));
        $tpl->setVariable("TYPE_SUMMARY", $type["Summary"]);
        $tpl->setVariable("TYPE_UNIT_NAME", $unitName);
        $tpl->setVariable("TYPE_UNIT_ID", $type["UnitId"]);
        $tpl->parseCurrentBlock("type_list");
      }
      $tpl->parseCurrentBlock("by_types");
    }
    else
    {
      // Assign data to the types list block 
      $tpl->setCurrentBlock("no_types");
      $tpl->touchBlock("no_types");
      $tpl->parseCurrentBlock("no_types");
    }
  }
  else
  {
    $limits = ProcessPagesLists(GetUnitsInProjectCount($_GET["Id"]), $tpl, $projectInfos["Id"]);
    $projectUnits = GetUnitsInProject($_GET["Id"], "ORDER BY Name", $limits[0], $limits[1]);
    if (!is_array($projectUnits))
    {
      if ($projectUnits == "")
        die("Project ".$_GET["Id"]." does not exist in the database.");
      else
        die($projectUnits);
    }
    elseif (count($projectUnits)>0)
    {
      $tpl->setVariable("TYPES", "");
      $tpl->setCurrentBlock("by_units");
      foreach ($projectUnits as $unit)
      {
        $unitName = str_replace(".dtx", ".pas", $unit["Name"]);
        $summary = $unit["Description"];
        if (!(strpos($summary, "\r\n")===0))
          $summary = substr($summary, 0, strpos($summary, "\r\n"));
          
        // Assign data to the unit list block 
        $tpl->setCurrentBlock("unit_list");
        $tpl->setVariable("UNIT_ID", $unit["Id"]);
        $tpl->setVariable("UNIT_NAME", $unitName);
        $tpl->setVariable("UNIT_SUMMARY", $summary);
        $tpl->setVariable("UNIT_AUTHOR", $unit["Author"]);
        $tpl->parseCurrentBlock("unit_list");
      }
      $tpl->parseCurrentBlock("by_units");
    }
    else
    {
      $tpl->setCurrentBlock("no_units");
      $tpl->touchBlock("no_units");
      $tpl->parseCurrentBlock("no_units");
    }
  }  
   
  $tpl->show();
  
  EndAccessToDB();
?>