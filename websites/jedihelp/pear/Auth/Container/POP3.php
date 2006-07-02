<?php
/* vim: set expandtab tabstop=4 shiftwidth=4: */
// +----------------------------------------------------------------------+
// | PHP Version 4                                                        |
// +----------------------------------------------------------------------+
// | Copyright (c) 1997-2002 The PHP Group                                |
// +----------------------------------------------------------------------+
// | This source file is subject to version 2.02 of the PHP license,      |
// | that is bundled with this package in the file LICENSE, and is        |
// | available at through the world-wide-web at                           |
// | http://www.php.net/license/2_02.txt.                                 |
// | If you did not receive a copy of the PHP license and are unable to   |
// | obtain it through the world-wide-web, please send a note to          |
// | license@php.net so we can mail you a copy immediately.               |
// +----------------------------------------------------------------------+
// | Authors: Stefan Ekman <stekman@sedata.org>                           |
// |          Martin Jansen <mj@php.net>                                  |
// |          Mika Tuupola <tuupola@appelsiini.net>                       |
// +----------------------------------------------------------------------+
//
// $Id: POP3.php,v 1.3 2003/07/28 21:39:39 yavo Exp $
//


require_once('Auth/Container.php');
require_once('PEAR.php');
require_once('Net/POP3.php');

/**
 * Storage driver for Authentication on a POP3 server.
 *
 * @author   Yavor Shahpasov <yavo@netsmart.com.cy>
 * @package  Auth
 * @version  $Revision: 1.3 $
 */
class Auth_Container_POP3 extends Auth_Container
{
    /**
     * POP3 Server
     * @var string
     */
    var $server='localhost';

    /**
     * POP3 Server port
     * @var string
     */
    var $port='110';

    // {{{ Constructor

    /**
     * Constructor of the container class
     *
     * @param  $server string server or server:port combination
     * @return object Returns an error object if something went wrong
     */
    function Auth_Container_POP3($server=null)
    {
        if(isset($server)){
            if(is_array($server)){
                if(isset($server['host'])){
                    $this->server = $server['host'];
                }
                if(isset($server['port'])){
                    $this->port = $server['port'];
                }
            }
            else{
                if(strstr($server, ':')){
                    $serverparts = explode(':', trim($server));
                    $this->server = $serverparts[0];
                    $this->port = $serverparts[1];
                }
                else
                {
                    $this->server = $server;
                }
            }
        }
    }

    // }}}
    // {{{ fetchData()

    /**
     * Try to login to the POP3 server
     *
     * @param   string Username
     * @param   string Password
     * @return  boolean
     */
    function fetchData($username, $password)
    {
        $pop3 =& new Net_POP3();
        $res = $pop3->connect($this->server, $this->port);
        if(!$res){
            return($res);
        }
        $result = $pop3->login($username, $password);
        $pop3->disconnect();
        return $result;
    }

    // }}}
}
?>
