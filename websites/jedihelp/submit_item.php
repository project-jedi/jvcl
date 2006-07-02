<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "security_utils.php";
  require_once "page_blocks.php";

  StartAccessToDB();

  $tpl = new HTML_Template_IT("./"); 
  $tpl->loadTemplatefile("submit_item.tpl.html", true, true);
  
  SetCommonLoginStatus($tpl);
  SetCommonToolbar($tpl);
  SetCommonFooter($tpl);
  
  // setup the "Back" link  
  if (array_key_exists("item_id", $_POST) && $_POST["item_id"] != "")
  {
    $tpl->setCurrentBlock("back_item");
    $tpl->setVariable("ITEM_ID", $_POST["item_id"]);
    $tpl->parseCurrentBlock("back_item");
    $projectId = GetProjectIdForItem($_POST["item_id"]);
  }
  else
  {
    $tpl->setCurrentBlock("back_unit");
    $tpl->setVariable("UNIT_ID", $_POST["unit_id"]);
    $tpl->parseCurrentBlock("back_unit");
    $projectId = GetProjectIdForUnit($_POST["unit_id"]);
  }

  $isLogged = IsLogged(); 
  
  // If user is logged in and had write access, we directly update the database
  if ($isLogged && IsWriter($projectId))
  {
    $_POST["description"] = str_replace("\\\\", "\\", $_POST["description"]);
    // if we have an item_id, we update, else we add
    if (array_key_exists("item_id", $_POST) && $_POST["item_id"] != "")
    {
      $msg = ModifyItem($_POST["item_id"], 
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
    }

    if (is_string($msg) && ($msg != ""))
      $msg = "Error while submitting the Item: ".$msg;
    else
      $msg = "Item added/updated successfuly";
      
    $tpl->setCurrentBlock("thanks_logged_in");  
    $tpl->touchBlock("thanks_logged_in");  
    $tpl->parseCurrentBlock("thanks_logged_in");  
  }
  else
  {
    // if the user is not logged in or does not have write access, 
    // we submit the item for review
    $msg = AddSubmittedItem(
             $_POST["item_id"], 
             $_POST["unit_id"], 
             $_POST["name"], $_POST["summary"], $_POST["description"], 
             $_POST["return_value"], $_POST["see_also_list"], $_POST["parameters"],
             $_POST["extras"], $_POST["jvcl_info"]);
             
    if ($msg != "")
      $msg = "Error while submitting the Item: ".$msg;
    
    $unitInfos = GetUnitInfos($_POST["unit_id"]);
    if (is_string($unitInfos))
    {
      if ($msg != "")
        $msg .= "<br>";
      $msg .= "Error getting Unit infos: ".$unitInfos;
    }
    $projectInfos = GetProjectInfos($unitInfos["ProjectId"]);
    if (is_string($projectInfos))
    {
      if ($msg != "")
        $msg .= "<br>";
      $msg .= "Error getting project infos: ".$projectInfos;
    }
    
    // Send the email to notify admins a new item has been submitted, if asked to do so
    if ($projectInfos["SendNotifications"]==1)
    {
      $mailResult = mail($projectInfos["ReviewersEmails"], 
                         "An item has been submitted: ".$_POST["name"],
                         "Item ".$_POST["name"]." has just been submitted into jedihelp. Please review it.",
                         "From: ".$projectInfos["AdminEmail"], 
                         "-f".$projectInfos["AdminEmail"]);
      if (!$mailResult)
      {
        if ($msg != "")
          $msg .= "<BR>";
        $msg .= "Error sending mail.";
      }
    }
    
    // if user was actually logged we tell him why he wasn't allowed to write in the
    // database directly
    if ($isLogged)
    {
      $tpl->setCurrentBlock("thanks_logged_in_no_write");  
      $tpl->touchBlock("thanks_logged_in_no_write");  
      $tpl->parseCurrentBlock("thanks_logged_in_no_write");  
    }
    else
    {
      $tpl->setCurrentBlock("thanks_submit");  
      $tpl->touchBlock("thanks_submit");  
      $tpl->parseCurrentBlock("thanks_submit");
    }  
  }
  
  $tpl->setVariable("MESSAGE", $msg);
  $tpl->show();
  
  EndAccessToDB();  
?>