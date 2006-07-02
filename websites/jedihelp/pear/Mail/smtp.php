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
// | Authors: Chuck Hagenbuch <chuck@horde.org>                           |
// |          Jon Parise <jon@php.net>                                    |
// +----------------------------------------------------------------------+

require_once 'Mail.php';

/**
 * SMTP implementation of the PEAR Mail:: interface. Requires the PEAR
 * Net_SMTP:: class.
 * @access public
 * @package Mail
 * @version $Revision: 1.12 $
 */
class Mail_smtp extends Mail {

    /**
     * The SMTP host to connect to.
     * @var string
     */
    var $host = 'localhost';

    /**
     * The port the SMTP server is on.
     * @var integer
     */
    var $port = 25;

    /**
     * Should SMTP authentication be used?
     *
     * This value may be set to true, false or the name of a specific
     * authentication method.
     *
     * If the value is set to true, the Net_SMTP package will attempt to use
     * the best authentication method advertised by the remote SMTP server.
     *
     * @var mixed
     */
    var $auth = false;

    /**
     * The username to use if the SMTP server requires authentication.
     * @var string
     */
    var $username = '';

    /**
     * The password to use if the SMTP server requires authentication.
     * @var string
     */
    var $password = '';

    /**
     * Hostname or domain that will be sent to the remote SMTP server in the
     * HELO / EHLO message.
     *
     * @var string
     */
    var $localhost = 'localhost';

    /**
     * Constructor.
     *
     * Instantiates a new Mail_smtp:: object based on the parameters
     * passed in. It looks for the following parameters:
     *     host        The server to connect to. Defaults to localhost.
     *     port        The port to connect to. Defaults to 25.
     *     auth        SMTP authentication.  Defaults to none.
     *     username    The username to use for SMTP auth. No default.
     *     password    The password to use for SMTP auth. No default.
     *     localhost   The local hostname / domain. Defaults to localhost.
     *
     * If a parameter is present in the $params array, it replaces the
     * default.
     *
     * @param array Hash containing any parameters different from the
     *              defaults.
     * @access public
     */
    function Mail_smtp($params)
    {
        if (isset($params['host'])) $this->host = $params['host'];
        if (isset($params['port'])) $this->port = $params['port'];
        if (isset($params['auth'])) $this->auth = $params['auth'];
        if (isset($params['username'])) $this->username = $params['username'];
        if (isset($params['password'])) $this->password = $params['password'];
        if (isset($params['localhost'])) $this->localhost = $params['localhost'];
    }

    /**
     * Implements Mail::send() function using SMTP.
     *
     * @param mixed $recipients Either a comma-seperated list of recipients
     *              (RFC822 compliant), or an array of recipients,
     *              each RFC822 valid. This may contain recipients not
     *              specified in the headers, for Bcc:, resending
     *              messages, etc.
     *
     * @param array $headers The array of headers to send with the mail, in an
     *              associative array, where the array key is the
     *              header name (e.g., 'Subject'), and the array value
     *              is the header value (e.g., 'test'). The header
     *              produced from those values would be 'Subject:
     *              test'.
     *
     * @param string $body The full text of the message body, including any
     *               Mime parts, etc.
     *
     * @return mixed Returns true on success, or a PEAR_Error
     *               containing a descriptive error message on
     *               failure.
     * @access public
     */
    function send($recipients, $headers, $body)
    {
        include_once 'Net/SMTP.php';

        if (!($smtp = new Net_SMTP($this->host, $this->port, $this->localhost))) {
            return new PEAR_Error('unable to instantiate Net_SMTP object');
        }

        if (PEAR::isError($smtp->connect())) {
            return new PEAR_Error('unable to connect to smtp server ' .
                                  $this->host . ':' . $this->port);
        }

        if ($this->auth) {
            $method = is_string($this->auth) ? $this->auth : '';

            if (PEAR::isError($smtp->auth($this->username, $this->password,
                              $method))) {
                return new PEAR_Error('unable to authenticate to smtp server');
            }
        }

        list($from, $text_headers) = $this->prepareHeaders($headers);

        /*
         * Since few MTAs are going to allow this header to be forged unless
         * it's in the MAIL FROM: exchange, we'll use Return-Path instead of
         * From: if it's set.
         */
        if (!empty($headers['Return-Path'])) {
            $from = $headers['Return-Path'];
        }

        if (!isset($from)) {
            return new PEAR_Error('No from address given');
        }

        if (PEAR::isError($smtp->mailFrom($from))) {
            return new PEAR_Error('unable to set sender to [' . $from . ']');
        }

        $recipients = $this->parseRecipients($recipients);
        foreach($recipients as $recipient) {
            if (PEAR::isError($res = $smtp->rcptTo($recipient))) {
                return new PEAR_Error('unable to add recipient [' .
                                      $recipient . ']: ' . $res->getMessage());
            }
        }

        if (PEAR::isError($smtp->data($text_headers . "\r\n" . $body))) {
            return new PEAR_Error('unable to send data');
        }

        $smtp->disconnect();
        return true;
    }
}

?>
