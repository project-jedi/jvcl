<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "security_utils.php";
  
  function GetAskLoginPage()
  {
    global $lastLoginResult;
    
    $tpl = new HTML_Template_IT("./"); 
  
    $tpl->loadTemplatefile("ask_login.tpl.html", true, true); 

    $msg = "";    
    if ($lastLoginResult != LLR_NOT_LOGGED_IN)
      $msg = LastLoginResultMessage();
    
    $tpl->setVariable("MESSAGE", $msg);
    
    // print the output
    return $tpl->get();
  }
  
  if (realpath(__FILE__) == realpath($_SERVER["SCRIPT_FILENAME"]))
    header("Location: http://".$_SERVER["HTTP_HOST"].dirname($_SERVER["PHP_SELF"])."/admin.php");

?>