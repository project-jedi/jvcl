<?php

  if (realpath(__FILE__) == realpath($_SERVER["SCRIPT_FILENAME"]))
    header("Location: http://".$_SERVER["HTTP_HOST"].dirname($_SERVER["PHP_SELF"])."/index.php");


  require_once "data_access.php";
  require_once "conf.php";
  
  // The possible values for $lastLoginResult
  define("LLR_LOGGED_IN",      0);
  define("LLR_TIMEOUT",        1);
  define("LLR_INVALID_KEY",    2);
  define("LLR_INVALID_LOGIN",  3);
  define("LLR_NOW_LOGGED_OUT", 4);
  define("LLR_NOT_LOGGED_IN",  5);

  // By default, the user is not logged in  
  define("LLR_DEFAULT_VALUE", LLR_NOT_LOGGED_IN);
  
  $lastLoginResult = LLR_DEFAULT_VALUE;
  
  function LastLoginResultMessage()
  {
    global $lastLoginResult;
    global $inactivityDelay;
    
    switch ($lastLoginResult)
    {
      case LLR_LOGGED_IN: 
        return "Logged in successfuly";
      case LLR_TIMEOUT: 
        return "Logged out: No activity for the past ".($inactivityDelay/60)." minutes";
      case LLR_INVALID_KEY: 
        return "The activity key you sent is incorrect. Please check you are not using the same account in two different browsers.";
      case LLR_INVALID_LOGIN: 
        return "Your username/password is incorrect.";
      case LLR_NOW_LOGGED_OUT: 
        return "You are now logged out";
      case LLR_NOT_LOGGED_IN:
        return "You are not logged in"; 
    }
  }
  
  function GetLoggedUserName()
  {
    global $cookieName;
    global $inactivityDelay;
    global $lastLoginResult;

    $result = "";
    // if the last login result is not its default value or not "logged in", then stop 
    // right here so that we don't overwrite the existing value. 
    if ($lastLoginResult != LLR_DEFAULT_VALUE && $lastLoginResult != LLR_LOGGED_IN)    
      return "";
    
    StartAccessToDB();
    
    // two cases: 
    // 1. we have the cookie and we must check the user is validly logged in
    // 2. we have the POST variables, and we must check the user can login
    //
    // Note that if we have both, the POST variables take over
     
    if (array_key_exists($cookieName, $_COOKIE))
    {
      list($userName, $activityKey) = explode(",", $_COOKIE[$cookieName]);
      $userInfos = GetUserInfos($userName);
      if (!is_array($userInfos))
        die($userInfos);
      if ($activityKey == $userInfos["ActivityKey"])
      {
        $result = $userName;
        $lastLoginResult = LLR_LOGGED_IN; 
        
        // The activity key is correct, but the last activity time might not be.
        // This may happen if the cookie is not managed properly by the user's browser
        // so we check it on the server side as well.
        $maxDateTime = date("Y-m-d H:i:s", time()-$inactivityDelay);
        
        if ($userInfos["LastActivity"] < $maxDateTime)
        {
          $result = "";
          $lastLoginResult = LLR_TIMEOUT; 
        }
      }
      else
      {
        $result = "";
        $lastLoginResult = LLR_INVALID_KEY;
      }
    }
    
    if (array_key_exists("user_name", $_POST))
    {
      $userInfos = GetUserInfos($_POST["user_name"]);
      if (!is_array($userInfos))
        die($userInfos);
      
      if (($userInfos == array()) || 
          ($_POST["md5_hash"] != $userInfos["password"]))
      {
        $result = "";
        $lastLoginResult = LLR_INVALID_LOGIN;
        unset($userInfos);
      }
      else
      {
        $result = $userInfos["username"];
        $lastLoginResult = LLR_LOGGED_IN; 
      }
    }
    
    // set the new key and associated cookie if user is logged in 
    if ($result != "")
    {
      $activityKey = md5($userInfos["username"].date("l dS of F Y h:i:s A"));
      $cookieValue = $userInfos["username"].",".$activityKey;
      
      // Set cookie value. We do it in the headers for the next time the script is called
      // but we also set the value in the $_COOKIE array to allow subsequent calls to this
      // function in the same execution to validate the login.
      // This way, one can call IsLogged then GetLoggedUserId to retrieve the Id of the 
      // logged user, both in the same script
      setcookie($cookieName, $cookieValue, time()+$inactivityDelay);//, "./", $_SERVER["SERVER_NAME"]);
      $_COOKIE[$cookieName] = $cookieValue;
      
      // update user
      $updateResult = UpdateUserActivity($userInfos["Id"], $activityKey);
      if ($updateResult != "")
        die($updateResult);
    }
    else
    {
      if (isset($userInfos))
      {
        
        // if we still have the default value here, then we need to check that this is
        // not because of a timeout. We know that by looking at the userInfos: If the
        // activity key is not empty, then this is because the cookie timed out.
        // The only side effect is that if a user closes his browser without first
        // logging out, he will see the timeout message the next time he comes to the
        // web site. 
        if ($lastLoginResult == LLR_DEFAULT_VALUE && $userInfos["ActivityKey"] != "")
          $lastLoginResult = LLR_TIMEOUT;
          
        ResetUserActivity($userInfos["Id"]);
      }    
    }
    
    EndAccessToDB();
    return $result;
  }
  
  function IsLogged()
  {
    $LoggedUserName = GetLoggedUserName();
    $result = ($LoggedUserName != "");
    return $result;
  }
  
  function IsAdmin()
  {
    StartAccessToDB();
    $LoggedUserName = GetLoggedUserName();
    if ($LoggedUserName != "")
    {
      $userInfos = GetUserInfos($LoggedUserName);
      $result = ($userInfos["IsAdmin"] == "Y");
    }
    else
    {
      $result = false;
    }
    EndAccessToDB();
    return $result;
  }
  
  function IsPower()
  {
    StartAccessToDB();
    $LoggedUserName = GetLoggedUserName();
    if ($LoggedUserName != "")
    {
      $userInfos = GetUserInfos($LoggedUserName);
      $result = ($userInfos["IsPower"] == "Y" || $userInfos["IsAdmin"] == "Y");
    }
    else
    {
      $result = false;
    }
    EndAccessToDB();
    return $result;
  }
  
  function IsWriter($projectId)
  {
    if ($projectId == "")
      return false;
      
    StartAccessToDB();
    $LoggedUserName = GetLoggedUserName();
    if ($LoggedUserName != "")
    {
      $userInfos = GetUserInfos($LoggedUserName);
      $validProjects = explode(",", $userInfos["ProjectsList"]);
      $result = (in_array($projectId, $validProjects) ||
                ($userInfos["IsAdmin"] == "Y"));
    }
    else
    {
      $result = false;
    }
    EndAccessToDB();
    return $result;
  }
  
  function CanUpload()
  {
    StartAccessToDB();
    $LoggedUserName = GetLoggedUserName();
    if ($LoggedUserName != "")
    {
      $userInfos = GetUserInfos($LoggedUserName);
      $result = ($userInfos["CanUpload"] == "Y");
    }
    else
    {
      $result = false;
    }
    EndAccessToDB();
    return $result;
  }
  
  function CanUploadZip()
  {
    StartAccessToDB();
    $result = IsPower();
    EndAccessToDB();
    return $result;
  }
  
  function CanManageSI()
  {
    StartAccessToDB();
    $LoggedUserName = GetLoggedUserName();
    if ($LoggedUserName != "")
    {
      $userInfos = GetUserInfos($LoggedUserName);
      $result = ($userInfos["IsAdmin"] == "Y" || $userInfos["IsPower"] == "Y");
    }
    else
    {
      $result = false;
    }
    EndAccessToDB();
    return $result;
  }
  
  function CanManageSeeAlso()
  {
    StartAccessToDB();
    $LoggedUserName = GetLoggedUserName();
    if ($LoggedUserName != "")
    {
      $userInfos = GetUserInfos($LoggedUserName);
      $result = ($userInfos["IsAdmin"] == "Y" || $userInfos["IsPower"] == "Y");
    }
    else
    {
      $result = false;
    }
    EndAccessToDB();
    return $result;
  }
  
  function CanManageProjects()
  {
    StartAccessToDB();
    $LoggedUserName = GetLoggedUserName();
    if ($LoggedUserName != "")
    {
      $userInfos = GetUserInfos($LoggedUserName);
      $result = ($userInfos["IsAdmin"] == "Y" || $userInfos["IsPower"] == "Y");
    }
    else
    {
      $result = false;
    }
    EndAccessToDB();
    return $result;
  }
  
  function CanManageUsers()
  {
    StartAccessToDB();
    $LoggedUserName = GetLoggedUserName();
    if ($LoggedUserName != "")
    {
      $userInfos = GetUserInfos($LoggedUserName);
      $result = ($userInfos["IsAdmin"] == "Y" || $userInfos["IsPower"] == "Y");
    }
    else
    {
      $result = false;
    }
    EndAccessToDB();
    return $result;
  }
  
  function HasNoSpecificRights()
  {
    StartAccessToDB();
    $LoggedUserName = GetLoggedUserName();
    if ($LoggedUserName != "")
    {
      $userInfos = GetUserInfos($LoggedUserName);
      $result = ($userInfos["IsAdmin"] == "N" && $userInfos["IsPower"] == "N" && $userInfos["CanUpload"] == "N");
    }
    else
    {
      $result = true;
    }
    EndAccessToDB();
    return $result;
  }
  
  function LoggedUserHasAccessToProject($projectId)
  {
    return IsWriter($projectId);
  }
  
  function GetLoggedUserId()
  {
    StartAccessToDB();
    $LoggedUserName = GetLoggedUserName();
    if ($LoggedUserName == "")
      $result = -1;
    else
    {
      $userInfos = GetUserInfos($LoggedUserName);
      if (is_array($userInfos))
        $result = $userInfos["Id"];
      else
        $result = -1;
    }
    EndAccessToDB();
    return $result;
  }
  
  function Logout()
  {
    global $cookieName;
    global $lastLoginResult;
    
    $userId = GetLoggedUserId();
    if ($userId != -1)
    {
      ResetUserActivity($userId);
    
      setcookie($cookieName, "");
      unset($_COOKIE[$cookieName]);
      $lastLoginResult = LLR_NOW_LOGGED_OUT;
    }
    else
      $lastLoginResult = LLR_NOT_LOGGED_IN;
  }
  
?>