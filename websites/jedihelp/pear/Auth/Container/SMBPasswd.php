<?php
/* vim: set expandtab tabstop=4 shiftwidth=4: */
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
// | Authors: Michael Bretterklieber <michael@bretterklieber.com>         |
// +----------------------------------------------------------------------+
//
// $Id: SMBPasswd.php,v 1.1 2003/05/13 19:23:54 mbretter Exp $
//

require_once "File/SMBPasswd.php";
require_once "Auth/Container.php";
require_once "PEAR.php";

/**
 * Storage driver for fetching login data from an SAMBA smbpasswd file.
 *
 * This storage container can handle SAMBA smbpasswd files.
 *
 * Example:
 * $a = new Auth("SMBPasswd", '/usr/local/private/smbpasswd');
 * $a->start();
 * if ($a->getAuth()) {
 *     printf ("AUTH OK<br>\n");
 *     $a->logout();
 * }
 *
 * @author   Michael Bretterklieber <michael@bretterklieber.com>
 * @package  Auth
 * @version  $Revision: 1.1 $
 */
class Auth_Container_SMBPasswd extends Auth_Container
{
    /**
     * File_SMBPasswd object
     * @var object
     */
    var $pwfile;

    // {{{ Constructor

    /**
     * Constructor of the container class
     *
     * @param  $filename   string filename for a passwd type file
     * @return object Returns an error object if something went wrong
     */
    function Auth_Container_SMBPasswd($filename)
    {
        $this->pwfile = new File_SMBPasswd($filename,0);

        if (!$this->pwfile->load()) {
            PEAR::raiseError("Error while reading file contents.", 41, PEAR_ERROR_DIE);
            return;
        }

    }

    // }}}
    // {{{ fetchData()

    /**
     * Get user information from pwfile
     *
     * @param   string Username
     * @param   string Password
     * @return  boolean
     */
    function fetchData($username, $password)
    {
        return $this->pwfile->verifyAccount($username, $password);
    }

    // }}}
    // {{{ listUsers()
    
    function listUsers()
    {
        return $this->pwfile->getAccounts();
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
    function addUser($username, $password, $additional = '')
    {
        $res = $this->pwfile->addUser($user, $additional['userid'], $pass);
        if ($res === true) {
            return $this->pwfile->save();
        }
        return $res;
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
        $res = $this->pwfile->delUser($username);
        if ($res === true) {
            return $this->pwfile->save();
        }
        return $res;
    }

    // }}}

}
?>
