<?php
//
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
// | Authors: Bruno Pedro <bpedro@co.sapo.pt>                             |
// +----------------------------------------------------------------------+
//
// $Id: SOAP.php,v 1.6 2003/09/08 11:24:05 yavo Exp $
//

require_once "Auth/Container.php";
require_once "PEAR.php";
require_once 'SOAP/Client.php';

/**
 * Storage driver for fetching login data from SOAP
 *
 * This class takes one parameter (options), where
 * you specify the following fields: endpoint, namespace,
 * method, encoding, usernamefield and passwordfield.
 *
 * You can use specify features of your SOAP service
 * by providing its parameters in an associative manner by
 * using the '_features' array through the options parameter.
 *
 * The 'matchpassword' option should be set to false if your
 * webservice doesn't return (username,password) pairs, but
 * instead returns error when the login is invalid.
 *
 * Example usage:
 *
 * <?php
 *
 * ...
 *
 * $options = array (
 *             'endpoint' => 'http://your.soap.service/endpoint',
 *             'namespace' => 'urn:/Your/Namespace',
 *             'method' => 'get',
 *             'encoding' => 'UTF-8',
 *             'usernamefield' => 'login',
 *             'passwordfield' => 'password',
 *             'matchpasswords' => false,
 *             '_features' => array (
 *                             'example_feature' => 'example_value',
 *                             'another_example'  => ''
 *                             )
 *             );
 * $auth = new Auth('SOAP', $options, 'loginFunction');
 * $auth->start();
 *
 * ...
 *
 * ?>
 *
 * @author   Bruno Pedro <bpedro@co.sapo.pt>
 * @package  Auth
 * @version  $Revision: 1.6 $
 */
class Auth_Container_SOAP extends Auth_Container
{

    /**
     * Required options for the class
     * @var array
     * @access private
     */
    var $_requiredOptions = array('endpoint', 'namespace', 'method', 'encoding', 'usernamefield', 'passwordfield');

    /**
     * Options for the class
     * @var array
     * @access private
     */
    var $_options = array();

    /**
     * Optional SOAP features
     * @var array
     * @access private
     */
    var $_features = array();

    /**
     * The SOAP response
     * @var array
     * @access public
     */
     var $soapResponse = array();

    /**
     * Constructor of the container class
     *
     * @param  $options, associative array with endpoint, namespace, method,
     *                   usernamefield, passwordfield and optional features
     */
    function Auth_Container_SOAP($options)
    {
        $this->_options = $options;
        if (!isset($this->_options['matchpasswords'])) {
            $this->_options['matchpasswords'] = true;
        }
        if (!empty($this->_options['_features'])) {
            $this->_features = $this->_options['_features'];
            unset($this->_options['_features']);
        }
    }

    /**
     * Fetch data from SOAP service
     *
     * Requests the SOAP service for the given username/password
     * combination.
     *
     * @param  string Username
     * @param  string Password
     * @return mixed Returns the SOAP response or false if something went wrong
     */
    function fetchData($username, $password)
    {
        // check if all required options are set
        if (array_intersect($this->_requiredOptions, array_keys($this->_options)) != $this->_requiredOptions) {
            return false;
        } else {
            // create a SOAP client and set encoding
            $soapClient = new SOAP_Client($this->_options['endpoint']);
            $soapClient->setEncoding($this->_options['encoding']);
        }
        // assign username and password fields
        $usernameField = new SOAP_Value($this->_options['usernamefield'],'string', $username);
        $passwordField = new SOAP_Value($this->_options['passwordfield'],'string', $password);
        $SOAPParams = array($usernameField, $passwordField);
        // assign optional features
        foreach ($this->_features as $fieldName => $fieldValue) {
            $SOAPParams[] = new SOAP_Value($fieldName, 'string', $fieldValue);
        }
        // make SOAP call
        $this->soapResponse = $soapClient->call(
                                  $this->_options['method'],
                                  $SOAPParams,
                                  array('namespace' => $this->_options['namespace'])
                                               );
        if (!PEAR::isError($this->soapResponse)) {
            if ($this->_options['matchpasswords']) {
                // check if passwords match
                if ($password == $this->soapResponse->{$this->_options['passwordfield']}) {
                    return true;
                } else {
                    return false;
                }
            } else {
                return true;
            }
        } else {
            return false;
        }
    }
}
?>
