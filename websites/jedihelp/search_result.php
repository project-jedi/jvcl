<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "utils.php";
  require_once "page_blocks.php";

  $tpl = new HTML_Template_IT("./"); 

  $tpl->loadTemplatefile("search_result.tpl.html", true, true);
  
  SetCommonLoginStatus($tpl);
  SetCommonToolbar($tpl);
  SetCommonFooter($tpl); 

  if (array_key_exists("projects", $_POST) && is_array($_POST["projects"]))
  {
    // advanced search
    $projectList = implode(",", $_POST["projects"]);
    $fields = $_POST["field"];
  }
  else
  {
    $projectList = $_POST["project"];
  }
  $search_text = $_POST["search_text"];
  $limit_author = $_POST["limit_author"];
  
  $undocumented_only = array_key_exists("undocumented_only", $_POST) && $_POST["undocumented_only"] == 1;
  
  $startTimeStr = microtime();
  $search_type = "";
  if (array_key_exists("search_type", $_POST))
    $search_type = $_POST["search_type"];
  if (!isset($fields))
    $fields="";  
      
  $items = SearchItems($projectList, $search_text, $fields, $search_type, $limit_author, $undocumented_only, $_POST["results_limit"]);
  $endTimeStr = microtime();
  $duration = MicrotimeDifference($startTimeStr, $endTimeStr);
  $tpl->SetVariable("SEARCH_DURATION", round($duration, 4));
   
  if (!is_array($items))
  {
    if (is_string($items))
      $tpl->SetVariable("RESULT_MESSAGE", $items);
    else
      $tpl->SetVariable("RESULT_MESSAGE", "No Items were found matching '".$search_text."'");

    $tpl->setCurrentBlock("item_list");
    $tpl->parseCurrentBlock("item_list");
  }
  else
  {
    $tpl->setCurrentBlock("item_list");
    
    $result_message = count($items)." items were found matching '".$search_text."' ";
    if ($limit_author != "")
      $result_message .= "limited to author '$limit_author' ";
    if ($undocumented_only)
      $result_message .= "with undocumented items only";
    $tpl->SetVariable("RESULT_MESSAGE", $result_message);
    foreach($items as $item)
    {
      $tpl->setCurrentBlock("item_list_items");
      $tpl->SetVariable("ITEM_RELEVANCE", round($item["Relevance"],2));
      
      
      
      $tpl->SetVariable("ITEM_SUMMARY", GetSummaryFromItem($item));
      
      $itemName = trim($item["Name"]);
      // if the item is actually a unit, then we make the link point to the unit
      // directly instead of the item
      if (substr($itemName, -4) == ".dtx")
      {
//        $tpl->setCurrentBlock("item_is_unit");
        $tpl->SetVariable("UNIT_ID", $item["Id"]);
        $itemName = str_replace(".dtx", ".pas", $itemName);
//        $tpl->parseCurrentBlock("item_is_unit");
      }
      else
      {      
//        $tpl->setCurrentBlock("item_is_not_unit");
        $tpl->SetVariable("ITEM_ID", $item["Id"]);
//        $tpl->parseCurrentBlock("item_is_not_unit");
      }
      
      $tpl->SetVariable("ITEM_NAME", $itemName);
      
      $tpl->parseCurrentBlock("item_list_items");
    }
    $tpl->parseCurrentBlock("item_list");
  }

  $tpl->Show();
?>