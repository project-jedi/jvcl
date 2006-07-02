<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "security.php";
  require_once "security_utils.php";
  require_once "page_blocks.php";
  require_once "not_a_power_user.php";

  if (!IsPower())
    die(GetNotAPowerUserPage());
  
  StartAccessToDB();
  $msg = "";
  if (array_key_exists("action", $_POST))
    $action = $_POST["action"];
  else
    $action = "";

  if ($action == "accept")
  {
    if ($_POST["original_item_id"] > 0)
    {
      // This is a modification
      if (!LoggedUserHasAccessToProject(GetProjectIdForItem($_POST["original_item_id"])))
        $msg = "You do not have access to this project.";
      else
        $msg = ModifyItem($_POST["original_item_id"], 
                          $_POST["name"], 
                          $_POST["summary"], 
                          $_POST["description"], 
                          $_POST["return_value"], 
                          $_POST["see_also_list"],
                          $_POST["parameters"], 
                          $_POST["extras"],
                          $_POST["jvcl_info"],
                          GetLoggedUserId());
      }
    else
    {
      // This is an addition
      if (!LoggedUserHasAccessToProject(GetProjectIdForUnit($_POST["unit_id"])))
        $msg = "You do not have access to this project.";
      else
      {
        $msg = AddItem($_POST["unit_id"], 
                       $_POST["name"], 
                       $_POST["summary"], 
                       $_POST["description"], 
                       $_POST["return_value"], 
                       $_POST["see_also_list"],
                       $_POST["parameters"], 
                       $_POST["extras"],
                       $_POST["jvcl_info"],
                       GetLoggedUserId());
        if (is_numeric($msg))
          $msg = "";       
      }
    }
    
    if ($msg != "")
      $msg .= "<br>";
      
    // delete the accepted item only if there were no errors
    if ($msg == "")
      $msg .= DeleteSubmittedItem($_POST["item_id"]);
    
    if ($msg == "")
      $msg = "Item accepted successfuly";
    else
      $msg = "Error while accepting item: ".$msg;
  }
  elseif ($action == "reject")
  {
    if (array_key_exists("items", $_POST) && is_array($_POST["items"]))
    {
      foreach ($_POST["items"] as $item)
      {
        $subItemInfos = GetSubmittedItemInfos($item);
        $projectId = GetProjectIdForUnit($subItemInfos["UnitId"]);
        if (!LoggedUserHasAccessToProject($projectId))
        {
          $projectInfos = GetProjectInfos($projectId);
          $error = "You do not have access to project '".$projectInfos["Name"]."'";
        }
        else
          $error = DeleteSubmittedItem($item);
          
        if ($error != "")
        {
          if ($msg != "")
            $msg .= "<br>";
          $msg .= $error;
        }
      }
      if ($msg == "")
        $msg = "Rejection successful";
      else
        $msg = "Error(s) while rejecting: ".$msg; 
    }
    elseif (array_key_exists("item_id", $_POST))
    {
      $msg = DeleteSubmittedItem($_POST["item_id"]);
      if ($msg == "")
        $msg = "Rejection successful";
      else
        $msg = "Error(s) while rejecting: ".$msg; 
    }
    else
      $msg = "Error: Please select an item to reject";
  }
  
  
  $tpl = new HTML_Template_IT("./"); 
  $tpl->loadTemplatefile("admin_submitted_items.tpl.html", true, true);
  
  SetCommonLoginStatus($tpl);
  SetAdminToolbar($tpl);
  SetCommonFooter($tpl); 

  $items = GetSubmittedItemsInfos();

  $tpl->setCurrentBlock("items_table");
  if(is_array($items) && count($items) > 0)
  {
    foreach ($items as $item)
    {
      // get project infos from UnitId
      $unitInfos = GetUnitInfos($item["UnitId"]);
      $projectInfos = GetProjectInfos($unitInfos["ProjectId"]);
      
      // Assign data to the items list 
      $tpl->setCurrentBlock("items");
      $tpl->setVariable("ID", $item["Id"]);
      $tpl->setVariable("NAME", $item["Name"]);
      $tpl->setVariable("SUMMARY", $item["Summary"]);
      $tpl->setVariable("SUBMITTED_ON", $item["CreatedOn"]);
      $tpl->setVariable("PROJECT_NAME", $projectInfos["Name"]);
      $tpl->parseCurrentBlock("items");
    }
  }
  else
  {
    $tpl->touchBlock("no_items");
  }
  $tpl->parseCurrentBlock("items_table");
  
  $tpl->setVariable("MESSAGE", $msg);
  $tpl->show();
  EndAccessToDB();
?>