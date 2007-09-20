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

// The base URL for an item image
$itemImageBaseUrl = "itemimages/";//"http://svn.sourceforge.net/viewcvs.cgi/*checkout*/jvcl/trunk/jvcl/images/";

// Use SMTP for sending emails, it's more reliable.
$SMTP_params = array("host" => '82.235.150.75',//'server.obones.com',
                     "IDHost" => 'project-jedi.org',
                     "port" => "25",
                     "auth" => true,
                     "username" => 'jedi',
                     "password" => 'jedimail');

// Setup the path to the pear library, it makes life much easier for everyone.
$include_path = ini_get("include_path");
ini_set("include_path", $include_path.":pear/");

?>
