<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "security_utils.php";
  require_once "security.php";
  require_once "page_blocks.php";
  require_once "not_a_power_user.php";
  require_once "no_access_to_project.php";

  if (!IsPower())
    die(GetNotAPowerUserPage());
  
  if (array_key_exists("Id", $_GET))
  {
    $projectId = $_GET["Id"];
    if (!LoggedUserHasAccessToProject($projectId))
      die(GetNoAccessToProjectPage($projectId));
  }
  else
    $projectId = "";
  
  $tpl = new HTML_Template_IT("./"); 
  $tpl->loadTemplatefile("admin_edit_project.tpl.html", true, true);
  
  SetCommonLoginStatus($tpl);
  SetAdminToolbar($tpl);
  SetCommonFooter($tpl);
  
  if (is_numeric($projectId))
  {
    // here we edit an existing project
    $tpl->setVariable("ACTION", "modify");
    
    $project = GetProjectInfos($projectId);
    
    $tpl->setVariable("ID", $project["Id"]);
    $tpl->setVariable("NAME", $project["Name"]);
    $tpl->setVariable("DESCRIPTION", EncodeString(trim($project["Description"])));
    $tpl->setVariable("REVIEWERS_EMAILS", $project["ReviewersEmails"]);
    $tpl->setVariable("ADMIN_EMAIL", $project["AdminEmail"]);
    $tpl->setVariable("SEND_NOTIFICATIONS_CHECKED", 
                         ($project["SendNotifications"]==1)?"checked":"");
  }
  else
  {
    // here we add a project
    $tpl->setVariable("ACTION", "add");

    $tpl->setVariable("ID", "");
    $tpl->setVariable("NAME", "");
    $tpl->setVariable("DESCRIPTION", "");
    $tpl->setVariable("REVIEWERS_EMAILS", "");
    $tpl->setVariable("ADMIN_EMAIL", "");
    $tpl->setVariable("SEND_NOTIFICATIONS_CHECKED", "");
  }
  
  $tpl->Show(); 
?>