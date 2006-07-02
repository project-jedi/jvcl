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
// | Author: Colin Viebrock <colin@easyDNS.com>                           |
// +----------------------------------------------------------------------+
//
// $Id: Dig.php,v 1.1 2002/04/25 14:57:23 cmv Exp $
//
// A nice friendly OO interface to dig
//

require_once('PEAR.php');

class Net_Dig extends PEAR
{
	// {{{ Public Properties
	
	/**
	 * The address to dig
	 *
	 * @var string $address
	 * @access public
	 */
	var $address;
	
	/**
	 * The server to use for digging
	 *
	 * @var string $server
	 * @access public
	 */
	var $server;
	
	/**
	 * The type of DNS records to dig for
	 *
	 * @var string $query_type
	 * @access public
	 */
	var $query_type;

	/**
	 * The last system command executed (for debugging)
	 *
	 * @var string $cmd
	 * @access public
	 */
	var $cmd;

	/**
	 * The raw output of the system command (for debugging)
	 *
	 * @var string $raw_data
	 * @access public
	 */
	var $raw_data;

	/**
	 * The location of the system dig program
	 *
	 * @var string $dig_prg
	 * @access public
	 */
	var $dig_prog;

	/**
	 * The parsed result of the last dig
	 *
	 * @var string $result
	 * @access public
	 */
	var $result;
	
	// }}}


	// {{{ Net_Dig()
	
	/**
	 * The Net_Dig constructor
         * Called when a new Net_Dig object is initialized
	 *
	 * @param string     [$address]  The address to dig (can be set 
	 *                               using the $address property as well)
	 *
	 * @param string     [$server]   The server to dig at (can be set 
	 *                               using the $server property as well)
	 *
	 * @return object Net_Dig   $obj   A new Net_Dig object
	 *
	 * @access public
	 * @author Colin Viebrock <colin@easyDNS.com>
	 * @since  PHP 4.0.5
	 */
	function Net_Dig($address = false, $server = false )
	{

		$this->address = $address;
		$this->server = $server;
		$this->query_type = false;

		$this->cmd = '';
		$this->raw_data = '';

		$this->result = false;

		$this->dig_prog = trim(`which dig`);
		if (!$this->dig_prog) {
			$this = new PEAR_Error("Couldn't find system dig program");
		}

	}

	// }}}



	// {{{ dig()
	
	/**
	 * Does a dig of the given address (or $this->address)
	 *
	 * @param string           [$address] The address to dig (can be set 
	 *                                using the $address property as well)
	 *
	 * @return object Net_Dig_result    $obj   A new Net_Dig_result object
	 *
	 * @access public
	 * @author Colin Viebrock <colin@easyDNS.com>
	 * @since  PHP 4.0.5
	 */
	function dig($address=false)
	{

		if ($address) {
			$this->address = $address;
		}

		if (!$this->address) {
			return new PEAR_Error("No address specified");
		}

		if (!$this->_validate_type()) {
			return new PEAR_Error($this->query_type." is an invalid query type");
		}

		$cmd = escapeshellcmd(
			sprintf("%s %s %s %s",
				$this->dig_prog,
				($this->server		? '@'.$this->server : ''),
				$this->address,
				($this->query_type	? $this->query_type : '' )
			)
		);

		$this->cmd = $cmd;


		$this->raw_data = `$cmd`;
		$this->raw_data = trim(	$this->raw_data );

		return $this->_parse_data();

	}
	
	// }}}


	// {{{ _validate_type()
	
	/**
	 * Validates the value of $this->query_type
	 *
	 * @return boolean	$return   True if $this->query_type is a 
	 *                                valid dig query, otherwise false
	 *
	 * @access private
	 * @author Colin Viebrock <colin@easyDNS.com>
	 * @since  PHP 4.0.5
	 */
	function _validate_type()
	{
		$return = true;
		if ($this->query_type) {
			$this->query_type = strtolower($this->query_type);
			switch ($this->query_type) {
			    case 'a':
			    case 'any':
			    case 'mx':
			    case 'ns':
			    case 'soa':
			    case 'hinfo':
			    case 'axfr':
			    case 'txt':
				break;
			    default:
				$return = false;
			}
		}
		return $return;
	}
	
	// }}}



	// {{{ _parse_data()
	
	/**
	 * Parses the raw data in $this->raw_data
	 *
	 * @return obj Net_Dig_result  $return   A Net_Dig_result object
	 *
	 * @access private
	 * @author Colin Viebrock <colin@easyDNS.com>
	 * @since  PHP 4.0.5
	 */
	function _parse_data()
	{

		if (!$this->raw_data) {
			return new PEAR_Error("No raw data to parse");
		}

		$regex = '/' .
			'^;(.*?)' .
			';; QUESTION SECTION\:(.*?)' .
			'(;; ANSWER SECTION\:(.*?))?' .
			'(;; AUTHORITY SECTION\:(.*?))?' .
			'(;; ADDITIONAL SECTION\:(.*?))?' .
			'(;;.*)' .
			'/ims';

		if (preg_match($regex, $this->raw_data, $matches)) {

			$result = new Net_Dig_result;

			/* Start parsing the data */


			/* the header ... */


			$temp = explode("\n", trim($matches[1]));
			if (preg_match('/DiG (.*?) /i', $temp[0], $m)) {
				$result->dig_version 		= trim($m[1]);
			}
			if (preg_match('/status: (.*?), id: (.*?)$/i', $temp[3], $m)) {
				$result->status			= trim($m[1]);
				$result->id			= trim($m[2]);
			}

			if (preg_match('/flags: (.*?); query: (.*?), answer: (.*?), authority: (.*?), additional: (.*?)$/i', $temp[4], $m)) {
				$result->flags			= trim($m[1]);
				$result->query_count		= (int)trim($m[2]);
				$result->answer_count		= (int)trim($m[3]);
				$result->authority_count	= (int)trim($m[4]);
				$result->additional_count	= (int)trim($m[5]);
			}


			/* query section */

			$line = trim(preg_replace('/^(;*)/', '', trim($matches[2])));
			list($host, $class, $type) = preg_split('/[\s]+/', $line, 3);
			$result->query[] = new Net_Dig_resource($host, false, $class, $type, false);


			/* answer section */

			$temp = trim($matches[4]);
			if ($temp) {
				$temp = explode("\n", $temp);
				if (count($temp)) {
					foreach($temp as $line) {
						$result->answer[] = $this->_parse_resource($line);
					}
				}
			}


			/* authority section */

			$temp = trim($matches[6]);
			if ($temp) {
				$temp = explode("\n", $temp);
				if (count($temp)) {
					foreach($temp as $line) {
						$result->authority[] = $this->_parse_resource($line);
					}
				}
			}


			/* additional section */

			$temp = trim($matches[8]);
			if ($temp) {
				$temp = explode("\n", $temp);
				if (count($temp)) {
					foreach($temp as $line) {
						$result->additional[] = $this->_parse_resource($line);
					}
				}
			}

			/* footer */

			$temp = explode("\n", trim($matches[9]));
			if (preg_match('/query time: (.*?)$/i', $temp[0], $m)) {
				$result->query_time	= trim($m[1]);
			}
			if (preg_match('/server: (.*?)#(.*?)\(/i', $temp[1], $m)) {
				$result->dig_server	= trim($m[1]);
				$result->dig_port	= trim($m[2]);
			}

			/* done */

			$result->consistency_check = (
				(count($result->query) == $result->query_count) &&
				(count($result->answer) == $result->answer_count) &&
				(count($result->authority) == $result->authority_count) &&
				(count($result->additional) == $result->additional_count)
			);

			return $result;

		}

		return new PEAR_Error("Can't parse raw data");
	}
	
	// }}}


	// {{{ _parse_resource()
	
	/**
	 * Parses a resource record line
	 *
	 * @param string           $line	The line to parse
	 *
	 * @return obj Net_Dig_resource  $return   A Net_Dig_resource object
	 *
	 * @access private
	 * @author Colin Viebrock <colin@easyDNS.com>
	 * @since  PHP 4.0.5
	 */
	function _parse_resource($line)
	{

		/* trim and remove leading ;, if present */		

		$line = trim(preg_replace('/^(;*)/', '', trim($line)));

		if ($line) {
			list($host, $ttl, $class, $type, $data) = preg_split('/[\s]+/', $line, 5);
			return new Net_Dig_resource($host, $ttl, $class, $type, $data);
		}

		return false;

	}

	// }}}

}



class Net_Dig_result {

	// {{{ Public Properties

	var $status;
	var $id;
	var $flags;
	var $query_count;
	var $answer_count;
	var $authority_count;
	var $additional_count;

	var $dig_version;
	var $dig_server;
	var $dig_port;

	var $query;
	var $answer;
	var $authority;
	var $additional;

	var $consistency_check;

	function Net_Dig_result() {
		$this->status = false;
		$this->id = false;
		$this->flags = false;
		$this->query_count = false;
		$this->answer_count = false;
		$this->authority_count = false;
		$this->additional_count = false;

		$this->dig_version = false;
		$this->dig_server = false;
		$this->dig_port = false;

		$this->query = array();
		$this->answer = array();
		$this->authority = array();
		$this->additional = array();

		$this->consistency_check = false;

	}

}

class Net_Dig_resource {

	var $host;
	var $ttl;
	var $class;
	var $type;
	var $data;

	function Net_Dig_resource($host=false, $ttl=false, $class=false, $type=false, $data=false) {
		$this->host	= $host;
		$this->ttl	= $ttl;
		$this->class	= $class;
		$this->type	= $type;
		$this->data	= $data;
	}

}

?>
