<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "security_utils.php";
  require_once "security.php";
  require_once "page_blocks.php";
  
  $msg="";
  
  if (array_key_exists("action", $_POST) && $_POST["action"] == "modify")
  {
    // Rules:
    // - The full name must always be indicated
    // - The email must always be indicated
    
    if ($_POST["full_name"] == "")
      $result = "The Full Name cannot be blank";
    elseif ($_POST["email"] == "")
      $result = "The E-Mail cannot be blank";
    else
    {
      $result = ModifyMyDetails(
                   GetLoggedUserId(),
                   $_POST["md5_hash"],
                   $_POST["full_name"],
                   $_POST["email"]
                  );
    }
        
    if ($result == "")
      $msg = "Details set successfully";
    else
      $msg = "Error while setting details: ".$result;
  }
  
  $userId = GetLoggedUserId();  
  $tpl = new HTML_Template_IT("./"); 
  $tpl->loadTemplatefile("edit_my_details.tpl.html", true, true);
  
  SetCommonLoginStatus($tpl);
  SetCommonToolbar($tpl);
  SetCommonFooter($tpl);
  
  $userInfos = GetUserInfosById($userId);
  $ProjectsList = GetProjectsNamesListFromIds($userInfos["ProjectsList"]);
  if(is_string($ProjectsList))
    die($ProjectsList);
  
  $tpl->setVariable("NAME", $userInfos["username"]);
  $tpl->setVariable("FULL_NAME", trim($userInfos["FullName"]));
  $tpl->setVariable("EMAIL", trim($userInfos["email"]));
  $tpl->setVariable("CAN_UPLOAD", $userInfos["CanUpload"]);
  $tpl->setVariable("IS_POWER", $userInfos["IsPower"]);
  $tpl->setVariable("IS_ADMIN", $userInfos["IsAdmin"]);
  $tpl->setVariable("PROJECTS_LIST", implode(", ", $ProjectsList));
  
  $tpl->setVariable("MESSAGE", $msg);

  $tpl->Show(); 
?>