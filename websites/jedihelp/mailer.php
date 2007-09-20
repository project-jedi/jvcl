<?php
/**
 * Provide mail capabilities
 * 
 * Largely inspired by what MediaWiki is doing  
 */
require_once("conf.php");

function wfQuotedPrintable( $string, $charset = '' ) {
	# Probably incomplete; see RFC 2045
	if( empty( $charset ) ) {
		$charset = "ISO-8859-1";
	}
	$charset = strtoupper( $charset );
	$charset = str_replace( 'ISO-8859', 'ISO8859', $charset ); // ?

	$illegal = '\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\xff=';
	$replace = $illegal . '\t ?_';
	if( !preg_match( "/[$illegal]/", $string ) ) return $string;
	$out = "=?$charset?Q?";
	$out .= preg_replace( "/([$replace])/e", 'sprintf("=%02X",ord("$1"))', $string );
	$out .= '?=';
	return $out;
}

/**
 * This function will perform a direct (authenticated) login to
 * a SMTP Server to use for mail relaying if 'SMTP_params' specifies an
 * array of parameters. It requires PEAR:Mail to do that.
 * See Mail::factory() at http://pear.php.net/manual/en/package.mail.mail.factory.php
 * for a full description of these settings. (Scroll down to the smtp parameter.)  
 * Otherwise it just uses the standard PHP 'mail' function.
 *
 * @param string $to recipient's email
 * @param string $from sender's email
 * @param string $subject email's subject
 * @param string $body email's text
 * @param string $encoding code page in which email's text is encoded
 */
function SendEMail( $to, $from, $subject, $body, $encoding = "ISO-8859-1" ) 
{
	global $SMTP_params;
	
	
  	$qto = wfQuotedPrintable( $to );
	
	if (is_array( $SMTP_params ))
	{
//    echo "Using SMTP_params, before requiring Mail.php<br>\r\n";
		require_once( 'pear/Mail.php' );
//    echo "Mail.php required just fine<br>\r\n";
		
		$timestamp = time();
	
		$headers['From'] = $from;
		$headers['To'] = $qto;
		$headers['Subject'] = $subject;
		$headers['MIME-Version'] = '1.0';
		$headers['Content-type'] = 'text/plain; charset='.$encoding;
		$headers['Content-transfer-encoding'] = '8bit';
		$headers['Message-ID'] = "<{$timestamp}" . "jedihelp" . '@' . $SMTP_params['IDHost'] . '>';
		$headers['X-Mailer'] = 'JediHelp e-mailer';
	
		// Create the mail object using the Mail::factory method
//    echo "Creating mail object<br>\r\n";
		$mail_object =& Mail::factory('smtp', $SMTP_params);
	
//    echo "Calling send method<br>\r\n";
		$mailResult =& $mail_object->send($to, $headers, $body);
		
//    echo "before returning from SendEMail<br>\r\n";
		# Based on the result return an error string, 
		if ($mailResult === true)
			return '';
		else if (is_object($mailResult))
			return $mailResult->getMessage();
		else
			return 'Mail object return unknown error.';
	}
	
	else
	{
//    echo "No SMTP_Params, using mail function<br>\r\n";
		$headers =
			"MIME-Version: 1.0\n" .
			"Content-type: text/plain; charset={$encoding}\n" .
			"Content-transfer-encoding: 8bit\n" .
			"From: {$from}\n" .
			"X-Mailer: JediHelp e-mailer";

		$mailresult = mail( $to, $subject, $body, $headers );
		if (!$mailresult) 
		  return "Error sending mail with the mail() function.";
		else
		  return '';
	}
}

?>
