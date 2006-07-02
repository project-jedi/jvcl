<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "security_utils.php";
  require_once "page_blocks.php";
  require_once "loginfailed.php";
  require_once "loginsuccessful.php";
  require_once "loggedout.php";

  // logout if asked to do so  
  if (array_key_exists("action", $_GET) && $_GET["action"] == "logout")
    Logout();
    
  // get the page to which we should return to, or default to index.php
  if (array_key_exists("returnTo", $_GET))
    $returnTo=$_GET["returnTo"];
  else
    $returnTo="index.php";
    
  // force the check of the currently checked user
  GetLoggedUserName();
  
  if ($lastLoginResult == LLR_LOGGED_IN)
  {
    print GetLoginSuccessfulPage($returnTo);
  }
  elseif ($lastLoginResult == LLR_INVALID_LOGIN)
  {
    print GetLoginFailedPage();
  }
  elseif ($lastLoginResult == LLR_NOW_LOGGED_OUT)
  {
    print GetLoggedOutPage();
  }
  else
  {
    $tpl = new HTML_Template_IT("./"); 
  
    $tpl->loadTemplatefile("login.tpl.html", true, true);
    
    SetCommonLoginStatus($tpl);
    SetCommonToolbar($tpl);
    SetCommonFooter($tpl); 
    
    $tpl->setVariable("RETURN_TO", urlencode($returnTo));
  
    $msg = "";    
    if ($lastLoginResult != LLR_NOT_LOGGED_IN)
      $msg = LastLoginResultMessage();
    
    $tpl->setVariable("MESSAGE", $msg);
    
    // print the output
    $tpl->show();
  }

?>