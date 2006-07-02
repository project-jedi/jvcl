<?PHP
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
// | Author: Stanislav Grozev <tacho@orbitel.bg>                          |
// +----------------------------------------------------------------------+
//
// $Id: vpopmail.php,v 1.3 2003/09/08 11:24:05 yavo Exp $
//

require_once "Auth/Container.php";

/**
 * Storage driver for fetching login data from vpopmail
 *
 * @author   Stanislav Grozev <tacho@orbitel.bg>
 * @package  Auth
 * @version  $Revision: 1.3 $
 */
class Auth_Container_vpopmail extends Auth_Container {

    // {{{ Constructor

    /**
     * Constructor of the container class
     *
     * @return integer Always returns 1.
     */
    function Auth_Container_vpopmail()
    {
        return 1;
    }

    // }}}
    // {{{ fetchData()

    /**
     * Get user information from vpopmail
     *
     * @param   string Username - has to be valid email address
     * @param   string Password
     * @return  boolean
     */
    function fetchData($username, $password)
    {
        $userdata = array();
        $userdata = preg_split("/@/", $username, 2);
        $result = @vpopmail_auth_user($userdata[0], $userdata[1], $password);

        return $result;
    }

    // }}}
}
?>
