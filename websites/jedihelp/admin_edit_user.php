<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "security.php";
  require_once "security_utils.php";
  require_once "not_an_admin.php";
  require_once "page_blocks.php";
  
  if (!IsAdmin())
    die(GetNotAnAdminPage());
  
  if (array_key_exists("Id", $_GET))
    $userId = $_GET["Id"];
  else
    $userId = "";
  
  $tpl = new HTML_Template_IT("./"); 
  $tpl->loadTemplatefile("admin_edit_user.tpl.html", true, true);
  
  SetCommonLoginStatus($tpl);
  SetAdminToolbar($tpl);
  SetCommonFooter($tpl);
  
  if (is_numeric($userId))
  {
    // here we edit an existing user
    $tpl->setVariable("ACTION", "modify");
    
    $userInfos = GetUserInfosById($userId);
    
    $tpl->setVariable("ID", $userInfos["Id"]);
    $tpl->setVariable("NAME", $userInfos["username"]);
    $tpl->setVariable("FULL_NAME", trim($userInfos["FullName"]));
    $tpl->setVariable("EMAIL", trim($userInfos["email"]));
    $tpl->setVariable("CAN_UPLOAD_CHECKED", 
                         ($userInfos["CanUpload"]=="Y")?"checked":"");
    $tpl->setVariable("IS_POWER_CHECKED", 
                         ($userInfos["IsPower"]=="Y")?"checked":"");
    $tpl->setVariable("IS_ADMIN_CHECKED", 
                         ($userInfos["IsAdmin"]=="Y")?"checked":"");
                         
    $userProjectsList = explode(",", $userInfos["ProjectsList"]);
  }
  else
  {
    // here we add a user
    $tpl->setVariable("ACTION", "add");

    $tpl->setVariable("ID", "");
    $tpl->setVariable("NAME", "");
    $tpl->setVariable("FULL_NAME", "");
    $tpl->setVariable("EMAIL", "");
    $tpl->setVariable("CAN_UPLOAD_CHECKED", "");
    $tpl->setVariable("IS_POWER_CHECKED", "");
    $tpl->setVariable("IS_ADMIN_CHECKED", "");
    
    $userProjectsList = array();
  }
  
  // set the projects list
  $projects = GetProjectsInfos();
  
  $projectsCount = count($projects)+1;
  if ($projectsCount > 6)
    $projectsCount = 6;

  $tpl->setVariable("PROJECT_COUNT", $projectsCount);
  foreach ($projects as $project)
  {
    // Assign data to the project list block 
    $tpl->setCurrentBlock("projects_list");
    $tpl->setVariable("PROJECT_NAME", $project["Name"]);
    $tpl->setVariable("PROJECT_ID", $project["Id"]);
    
    if (in_array($project["Id"], $userProjectsList))
      $tpl->setVariable("SELECTED", "selected");
    else
      $tpl->setVariable("SELECTED", "");
      
    $tpl->parseCurrentBlock("projects_list");
  }
  
  
  $tpl->Show(); 
?>