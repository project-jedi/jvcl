<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "security.php";
  require_once "security_utils.php";
  require_once "page_blocks.php";
  require_once "not_a_power_user.php";

  if (!IsPower())
    die(GetNotAPowerUserPage());
  
  $msg = "";
  if (array_key_exists("action", $_POST))
    $action = $_POST["action"];
  else
    $action = "";
    
  if ($action == "add")
  {
    if (!IsAdmin())
    {
      $result = "Only administrators may add projects in the system.";
    }
    else
    {
      $result = AddProject(
                   $_POST["name"], 
                   $_POST["description"],
                   $_POST["reviewers_emails"],
                   $_POST["admin_email"],
                   array_key_exists("send_notifications", $_POST)?$_POST["send_notifications"]:"",
                   GetLoggedUserId()
                  );
    }
    
    if ($result == "")
      $msg = "Add successful";
    else
      $msg = "Error while adding: ".$result;
  }
  elseif ($action == "modify")
  {
    $result = ModifyProject(
                 $_POST["Id"],
                 $_POST["name"], 
                 $_POST["description"],
                 $_POST["reviewers_emails"],
                 $_POST["admin_email"],
                 array_key_exists("send_notifications", $_POST)?$_POST["send_notifications"]:"",
                 GetLoggedUserId()
                );
    if ($result == "")
      $msg = "Modification successful";
    else
      $msg = "Error while modifying: ".$result;
  }
  elseif ($action == "delete")
  {
    if (!IsAdmin())
    {
      $msg = "Only administrators may delete projects from the system.";
    }
    elseif (array_key_exists("projects", $_POST) && is_array($_POST["projects"]))
    {
      StartAccessToDB();
      foreach ($_POST["projects"] as $project)
      {
        $error = DeleteProject($project);
        if ($error != "")
        {
          if ($msg != "")
            $msg .= "<br>";
          $msg .= $error;
        }
      }
      if ($msg == "")
        $msg = "Delete successful";
      else
        $msg = "Error(s) while deleting: ".$msg; 
      EndAccessToDB();
    }
    else
      $msg = "Error: Please select a project to delete";
  }

  $tpl = new HTML_Template_IT("./"); 
  $tpl->loadTemplatefile("admin_projects.tpl.html", true, true);
  
  SetCommonLoginStatus($tpl);
  SetAdminToolbar($tpl);
  SetCommonFooter($tpl); 

  $projects = GetProjectsInfos();

  $tpl->setCurrentBlock("projects_table");
  if(is_array($projects))
  {
    foreach ($projects as $project)
    {
      // Assign data to the delete projects list 
      $tpl->setCurrentBlock("delete_projects");
      $tpl->setVariable("PROJECT_ID", $project["Id"]);
      $tpl->setVariable("PROJECT_NAME", $project["Name"]);
      $tpl->setVariable("PROJECT_DESCRIPTION", EncodeString($project["Description"]));
      $tpl->parseCurrentBlock("delete_projects");
    }
  }
  else
  {
    if ($msg != "")
      $msg .= "<br>";
    $msg .= "Warning: There are no projects in the system, use the link above to add one.";
  }
  $tpl->parseCurrentBlock("projects_table");
  
  $tpl->setVariable("MESSAGE", $msg);
  $tpl->show();
?>