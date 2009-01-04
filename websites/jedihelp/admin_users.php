<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "security.php";
  require_once "security_utils.php";
  require_once "not_an_admin.php";
  require_once "page_blocks.php";
  require_once "utils.php";
  
  if (!IsAdmin())
    die(GetNotAnAdminPage());
  
  $msg = "";
  if (array_key_exists("action", $_POST))
    $action = $_POST["action"];
  else
    $action = "";
    
  if ($action == "add")
  {
    // When adding a user, user name, password and full name must be given
    if ($_POST["username"] == "")
      $result = "You must give a user name when adding a user";
    elseif ($_POST["md5_hash"] == "")
      $result = "You must set the password when adding a user";
    elseif ($_POST["full_name"] == "")
      $result = "You must give a Full Name when adding a user";
    elseif ($_POST["email"] == "")
      $result = "You must give an E-Mail when adding a user";
    elseif (!array_key_exists("is_admin", $_POST) && !array_key_exists("projects", $_POST))
      $result = "Non admin users must have at least one project assigned";
    else
    {
      // We check here that the given username doesn't already exist in the database.
      // We could let AddUser give us an error code in return, but the error message 
      // wouldn't be very human readable.
      if (GetUserInfos($_POST["username"]) != array())
      {
        $result = "The indicated username already exists in the database. Please choose another one.";
      }
      else
      {
        $result = AddUser(
                     $_POST["username"], 
                     $_POST["md5_hash"],
                     $_POST["full_name"],
                     $_POST["email"],
                     array_key_exists("can_upload", $_POST)?"Y":"N",
                     array_key_exists("is_power", $_POST)?"Y":"N",
                     array_key_exists("is_admin", $_POST)?"Y":"N",
                     array_key_exists("projects", $_POST)?$_POST["projects"]:array()
                    );
      }
    }
                
    if ($result == "")
      $msg = "Add successful";
    else
      $msg = "Error while adding: ".$result;
  }
  elseif ($action == "modify")
  {
    // Rules:
    // - There must always be one admin in the system
    // - A user cannot revoke his own admin status
    // - The full name must always be indicated
    
    if ($_POST["full_name"] == "")
      $result = "The Full Name cannot be blank";
    elseif ($_POST["email"] == "")
      $result = "The E-Mail cannot be blank";
    else
    {
      StartAccessToDB();
      $AtLeastOneAdmin = true;
      $adminIds = GetAdminUsersId();
      if (!is_array($adminIds))
        die($adminIds);
        
      if (!array_key_exists("is_admin", $_POST))
      {
         
        $adminCount = 0; 
        foreach ($adminIds as $adminId)
        {
          if ($adminId != $_POST["Id"])
            $adminCount++;
        }  
          
        $AtLeastOneAdmin = ($adminCount > 0); 
      }
      
      if ($AtLeastOneAdmin)
      {
        $userInfos = GetUserInfosById($_POST["Id"]);
        if ($userInfos["username"] == GetLoggedUserName() && 
            $userInfos["IsAdmin"] == "Y" && 
            !array_key_exists("is_admin", $_POST))
          $result = "You cannot revoke your own admin status. Please ask another admin to do so.";
        elseif (!array_key_exists("is_admin", $_POST) && !array_key_exists("projects", $_POST))
          $result = "Non admin users must have at least one project assigned";
        else 
          $result = ModifyUser(
                       $_POST["Id"],
                       $_POST["md5_hash"],
                       $_POST["full_name"], 
                       $_POST["email"],
                       array_key_exists("can_upload", $_POST)?"Y":"N",
                       array_key_exists("is_power", $_POST)?"Y":"N",
                       array_key_exists("is_admin", $_POST)?"Y":"N",
                       array_key_exists("projects", $_POST)?$_POST["projects"]:array()
                      );
      }
      else
        $result = "There must always be at least one admin in the system.";
        
      EndAccessToDB();
    }
        
    if ($result == "")
      $msg = "Modification successful";
    else
      $msg = "Error while modifying: ".$result;
  }
  elseif ($action == "delete")
  {
    if (array_key_exists("users", $_POST) && is_array($_POST["users"]))
    {
      StartAccessToDB();
      
      // Rules:
      // - It is not possible to delete all users
      // - A user cannot delete himself
      // - It is not possible to delete the last admin
      
      if (count($_POST["users"]) == GetUsersCount())
        $msg = "Cannot delete all users in the system.";
      else
      {
        $LoggedUserId = GetLoggedUserId();
        $adminIds = GetAdminUsersId();
        $adminCount = count($adminIds);
        
        foreach ($_POST["users"] as $userId)  // DO NOT CALL THE VARIABLE $user, it clashes with the database settings in conf.php
        {
          if ($LoggedUserId == $userId)
            $error = "You cannot delete yourself. Please ask another admin to do so.";
          else
          {
            if (in_array($userId, $adminIds))
              $adminCount--;
            
            if ($adminCount == 0)
            {
              $error = "Cannot delete the last admin in the system.";
            }
            else
            {  
              $error = DeleteUser($userId);
            }
          }
            
          if ($error != "")
          {
            if ($msg != "")
              $msg .= "<br>";
            $msg .= $error;
          }
        }
      }
      
      if ($msg == "")
        $msg = "Delete successful";
      else
        $msg = "Error(s) while deleting: ".$msg; 
        
      EndAccessToDB();
    }
    else
      $msg = "Error: Please select a user to delete";
  }

  $tpl = new HTML_Template_IT("./"); 
  $tpl->loadTemplatefile("admin_users.tpl.html", true, true);
  
  SetCommonLoginStatus($tpl);
  SetAdminToolbar($tpl);
  SetCommonFooter($tpl); 

  $users = GetUsersInfos();

  $tpl->setCurrentBlock("users_table");
  if(is_array($users))
  {
    foreach ($users as $user)
    {
      // Assign data to the delete projects list 
      $tpl->setCurrentBlock("delete_users");
      $tpl->setVariable("USER_ID", $user["Id"]);
      $tpl->setVariable("USER_USERNAME", $user["username"]);
      $tpl->setVariable("USER_FULLNAME", EncodeString($user["FullName"]));
      $tpl->setVariable("USER_EMAIL", ObfuscateEmail($user["email"]));
      $tpl->setVariable("USER_CAN_UPLOAD", $user["CanUpload"]);
      $tpl->setVariable("USER_IS_POWER", $user["IsPower"]);
      $tpl->setVariable("USER_IS_ADMIN", $user["IsAdmin"]);
      
      $projectsNames = GetProjectsNamesListFromIds($user["ProjectsList"]);
      if (is_string($projectsNames))
        die($projectsNames);
      $tpl->setVariable("USER_PROJECTS_LIST", implode(", ", $projectsNames));
      
      $tpl->parseCurrentBlock("delete_users");
    }
  }
  else
  {
    if ($msg != "")
      $msg .= "<br>";
    $msg .= "Warning: There are no users in the system, this should not happen !!!!";
  }
  $tpl->parseCurrentBlock("users_table");
  
  $tpl->setVariable("MESSAGE", $msg);
  $tpl->show();
?>