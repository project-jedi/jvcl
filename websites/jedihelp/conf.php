<?php
  if (realpath(__FILE__) == realpath($_SERVER["SCRIPT_FILENAME"]))
    header("Location: http://".$_SERVER["HTTP_HOST"].dirname($_SERVER["PHP_SELF"])."/index.php");


// The host to connect to
$db_host = "localhost";

// The name of the database
$db_name = "project_jedi";

// Username and password to use to connect
$db_user = "jedihelp";
$db_pwd = "xVXvN5dLru,NrQby";

// The maximum inactivity delay before being logged out in seconds
$inactivityDelay = 20 *60;

// the name of the cookie used to keep track of logins
$cookieName = "jhelp_admin_cookie";


?>
