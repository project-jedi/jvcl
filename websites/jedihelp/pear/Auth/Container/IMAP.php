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
// | Authors: Jeroen Houben <jeroen@terena.nl>                             |
// +----------------------------------------------------------------------+
//
// $Id: IMAP.php,v 1.7 2003/10/20 09:38:29 yavo Exp $
//

require_once "Auth/Container.php";
require_once "PEAR.php";

/**
 * Storage driver for fetching login data from an IMAP server
 *
 * This class is based on LDAP containers, but it very simple.
 * By default it connects to localhost:143 
 * The constructor will first check if the host:port combination is 
 * actually reachable. This behaviour can be disabled.
 * It then tries to create an IMAP stream (without opening a mailbox)
 * If you wish to pass extended options to the connections, you may
 * do so by specifying protocol options.
 * 
 * To use this storage containers, you have to use the
 * following syntax:
 *
 * <?php
 * ...
 * $params = array(
 * 'host'       => 'mail.example.com',
 * 'port'       => 143,
 * );
 * $myAuth = new Auth('IMAP', $params);
 * ....
 * 
 * By default we connect without any protocol options set. However, some 
 * servers require you to connect with the notls or norsh options set.
 * To do this you need to add the following value to the params array:
 * 'baseDSN'   => '/imap/notls/norsh'
 *
 * To connect to an SSL IMAP server:
 * 'baseDSN'   => '/imap/ssl'
 *
 * To connect to an SSL IMAP server with a self-signed certificate:
 * 'baseDSN'   => '/imap/ssl/novalidate-cert'
 *
 * Further options may be available and can be found on the php site at
 * http://www.php.net/manual/function.imap-open.php
 *
 */

/*
 *
 * @author   Jeroen Houben <jeroen@terena.nl>, Cipriano Groenendal <cipri@campai.nl>
 * @package  Auth
 * @version  $Revision: 1.7 $
 */
class Auth_Container_IMAP extends Auth_Container
{
    /**
     * Options for the class
     * @var array
     */
    var $options = array();

    /**
     * Constructor of the container class
     *
     * @param  $params, associative hash with host,port,basedn and userattr key
     * @param  $params, associative array with host, port, baseDSN, checkServer key.
     * @return object Returns an error object if something went wrong
     */
    function Auth_Container_IMAP($params)
    {
        if (!extension_loaded('imap')) {
            return PEAR::raiseError("Cannot use IMAP authentication, IMAP extension not loaded!",
                                    41, PEAR_ERROR_DIE);
        }
        $this->_setDefaults();
        
        // set parameters (if any)
        if (is_array($params)) {
            $this->_parseOptions($params);
        }
        if ($this->options['checkServer']) {
            $this->_checkServer($this->options['timeout']);
        }
        return true;
    }

    /**
     * Set some default options
     *
     * @access private
     */
    function _setDefaults()
    {
        $this->options['host'] = 'localhost';
        $this->options['port'] = 143;
        $this->options['baseDSN'] = '';
        $this->options['checkServer'] = true;
        $this->options['timeout'] = 20;
    }


    /**
     * Check if the given server and port are reachable
     *
     * @access private
     */
    function _checkServer() {
        $fp = @fsockopen ($this->options['host'], $this->options['port'],
                          $errno, $errstr, $timeout);
        if (is_resource($fp)) {
            @fclose($fp);
        } else {
            $message = "Error connecting to IMAP server "
                . $this->options['host']
                . ":" . $this->options['port'];
            return PEAR::raiseError($message, 41, PEAR_ERROR_DIE);
        }
    }

    /**
     * Parse options passed to the container class
     *
     * @access private
     * @param  array
     */
    function _parseOptions($array)
    {
        foreach ($array as $key => $value) {
            $this->options[$key] = $value;
        }
    }

    /**
     * Try to open a IMAP stream using $username / $password
     *
     * @param  string Username
     * @param  string Password
     * @return boolean
     */
    function fetchData($username, $password)
    {
        $dsn = '{'.$this->options['host'].':'.$this->options['port'].$this->options['baseDSN'].'}';
        $conn = @imap_open ($dsn, $username, $password, OP_HALFOPEN);
        if (is_resource($conn)){
            $this->activeUser = $username;
            @imap_close($conn);
            return true;
        } else {
            $this->activeUser = '';
            return false;
        }
    }
}
?>
