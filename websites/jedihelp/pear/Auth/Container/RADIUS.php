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
// | Authors: Michael Bretterklieber <michael@bretterklieber.com>         |
// +----------------------------------------------------------------------+
//
// $Id: RADIUS.php,v 1.7 2003/05/13 19:27:35 mbretter Exp $
//

require_once "Auth/Container.php";
require_once "Auth/RADIUS.php";

/**
 * Storage driver for authenticating users against RADIUS servers.
 *
 * @author  Michael Bretterklieber <michael@bretterklieber.com>
 * @access  public
 * @version $Revision: 1.7 $
 */
class Auth_Container_RADIUS extends Auth_Container
{

    /**
     * Contains a RADIUS object
     * @var object
     */
    var $radius;
    
    /**
     * Contains the authentication type
     * @var string
     */
    var $authtype;    

    /**
     * Constructor of the container class.
     *
     * $options can have these keys:
     * 'servers'    an array containing an array: servername, port,
     *              sharedsecret, timeout, maxtries
     * 'configfile' The filename of the configuration file
     * 'authtype'   The type of authentication, one of: PAP, CHAP_MD5,
     *              MSCHAPv1, MSCHAPv2, default is PAP
     *
     * @param  $options associative array
     * @return object Returns an error object if something went wrong
     */
    function Auth_Container_RADIUS($options)
    {
        $this->authtype = 'PAP';
        if (isset($options['authtype'])) {
            $this->authtype = $options['authtype'];
        }
        $classname = 'Auth_RADIUS_' . $this->authtype;
        if (!class_exists($classname)) {
            PEAR::raiseError("Unknown Authtype, please use on of: PAP, CHAP_MD5, MSCHAPv1, MSCHAPv2!",
                                    41, PEAR_ERROR_DIE);
        }
        
        $this->radius = new $classname;

        if (isset($options['configfile'])) {
            $this->radius->setConfigfile($options['configfile']);
        }

        $servers = $options['servers'];
        if (is_array($servers)) {
            foreach ($servers as $server) {
                $servername     = $server[0];
                $port           = isset($server[1]) ? $server[1] : 0;
                $sharedsecret   = isset($server[2]) ? $server[2] : 'testing123';
                $timeout        = isset($server[3]) ? $server[3] : 3;
                $maxtries       = isset($server[4]) ? $server[4] : 3;
                $this->radius->addServer($servername, $port, $sharedsecret, $timeout, $maxtries);
            }
        }
        
        if (!$this->radius->start()) {
            PEAR::raiseError($this->radius->getError(), 41, PEAR_ERROR_DIE);
        }
    }

    /**
     * Authenticate
     *
     * @param  string Username
     * @param  string Password
     * @return bool   true on success, false on reject
     */
    function fetchData($username, $password, $challenge = null)
    {
        switch($this->authtype) {
        case 'CHAP_MD5':
        case 'MSCHAPv1':
            if (isset($challenge)) {
                echo $password;
                $this->radius->challenge = $challenge;
                $this->radius->chapid    = 1;
                $this->radius->response  = pack('H*', $password);
            } else {
                require_once 'Crypt_CHAP/CHAP.php';
                $classname = 'Crypt_' . $this->authtype;
                $crpt = new $classname;
                $crpt->password = $password;
                $this->radius->challenge = $crpt->challenge;
                $this->radius->chapid    = $crpt->chapid;
                $this->radius->response  = $crpt->challengeResponse();
                break;
            }

        case 'MSCHAPv2':
            require_once 'Crypt_CHAP/CHAP.php';
            $crpt = new Crypt_MSCHAPv2;
            $crpt->username = $username;
            $crpt->password = $password;
            $this->radius->challenge     = $crpt->authChallenge;
            $this->radius->peerChallenge = $crpt->peerChallenge;
            $this->radius->chapid        = $crpt->chapid;
            $this->radius->response      = $crpt->challengeResponse();
            break;

        default:
            $this->radius->password = $password;
            break;
        }

        $this->radius->username = $username;

        $this->radius->putAuthAttributes();
        $result = $this->radius->send();
        if (PEAR::isError($result)) {
            return false;
        }

        $this->radius->getAttributes();
//      just for debugging
//      $this->radius->dumpAttributes();

        return $result;
    }
}
?>
