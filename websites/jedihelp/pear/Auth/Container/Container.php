<?php
//
// +----------------------------------------------------------------------+
// | PHP Version 4                                                        |
// +----------------------------------------------------------------------+
// | Copyright (c) 1997-2003 The PHP Group                                |
// +----------------------------------------------------------------------+
// | This source file is subject to version 2.02 of the PHP license,      |
// | that is bundled with this package in the file LICENSE, and is        |
// | available at through the world-wide-web at                           |
// | http://www.php.net/license/2_02.txt.                                 |
// | If you did not receive a copy of the PHP license and are unable to   |
// | obtain it through the world-wide-web, please send a note to          |
// | license@php.net so we can mail you a copy immediately.               |
// +----------------------------------------------------------------------+
// | Authors: Martin Jansen <mj@php.net>                                  |
// +----------------------------------------------------------------------+
//
// $Id: Container.php,v 1.15 2003/10/19 14:03:19 yavo Exp $
//

define("AUTH_METHOD_NOT_SUPPORTED", -4);

/**
 * Storage class for fetching login data
 *
 * @author   Martin Jansen <mj@php.net>
 * @package  Auth
 */
class Auth_Container
{

    /**
     * User that is currently selected from the storage container.
     *
     * @access public
     */
    var $activeUser = "";

    // {{{ Constructor

    /**
     * Constructor
     *
     * Has to be overwritten by each storage class
     *
     * @access public
     */
    function Auth_Container()
    {
    }

    // }}}
    // {{{ fetchData()

    /**
     * Fetch data from storage container
     *
     * Has to be overwritten by each storage class
     *
     * @access public
     */
    function fetchData() 
    {
    }

    // }}}
    // {{{ verifyPassword()

    /**
     * Crypt and verfiy the entered password
     *
     * @param  string Entered password
     * @param  string Password from the data container (usually this password
     *                is already encrypted.
     * @param  string Type of algorithm with which the password from
     *                the container has been crypted. (md5, crypt etc.)
     *                Defaults to "md5".
     * @return bool   True, if the passwords match
     */
    function verifyPassword($password1, $password2, $cryptType = "md5")
    {
        switch ($cryptType) {
        case "crypt" :
            return (($password2 == "**" . $password1) ||
                    (crypt($password1, $password2) == $password2)
                    );
            break;

        case "none" :
            return ($password1 == $password2);
            break;

        case "md5" :
            return (md5($password1) == $password2);
            break;

        default :
            if (function_exists($cryptType)) {
                return ($cryptType($password1) == $password2);
            }
            else if (method_exists($this,$cryptType)) { 
                return ($this->$cryptType($password1) == $password2);
            } else {
                return false;
            }
            break;
        }
    }

    // }}}
    // {{{ listUsers()

    /**
     * List all users that are available from the storage container
     */
    function listUsers()
    {
        return AUTH_METHOD_NOT_SUPPORTED;
    }

    /**
     * Returns a user assoc array
     *
     * Containers which want should overide this
     *
     * @param string The username
     */
    function getUser($username)
    {
        $users = $this->listUsers();
        if($users === AUTH_METHOD_NOT_SUPPORTED){
            return(AUTH_METHOD_NOT_SUPPORTED);
        }
        for($i=0;$c = count($users),$i<$c;$i++){
            if($users[$i]['username'] == $username){
                return($users[$i]);
            }
        }
        return(false);
        
    }

    // }}}
    // {{{ addUser()

    /**
     * Add a new user to the storage container
     *
     * @param string Username
     * @param string Password
     * @param array  Additional information
     *
     * @return boolean
     */
    function addUser($username, $password, $additional=null)
    {
        return AUTH_METHOD_NOT_SUPPORTED;
    }

    // }}}
    // {{{ removeUser()

    /**
     * Remove user from the storage container
     *
     * @param string Username
     */
    function removeUser($username)
    {
        return AUTH_METHOD_NOT_SUPPORTED;
    }

    // }}}

}
?>
