<?php
  if (realpath(__FILE__) == realpath($_SERVER["SCRIPT_FILENAME"]))
    header("Location: http://".$_SERVER["HTTP_HOST"].dirname($_SERVER["PHP_SELF"])."/index.php");
  
  require_once "loginfailed.php";
  require_once "security_utils.php";
  
  if (!IsLogged())
  {
    if ($lastLoginResult == LLR_INVALID_LOGIN)
      die(GetLoginFailedPage());
    else
    {
      header("Location: http://". $_SERVER['HTTP_HOST'].
                     dirname($_SERVER['PHP_SELF']).
                     "/login.php?returnTo=".urlencode($_SERVER['PHP_SELF']));
      die();
    }
  }
  
?>
