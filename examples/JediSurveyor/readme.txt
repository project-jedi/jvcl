OVERVIEW

JEDI Surveyor is a toolkit for creating, submitting and evaluating surveys. The toolkit contains three applications that together forms a complete solution to handling of surveys.


PROGRAMS

JEDI Survey Builder (jsb.exe): 
	A tool to create surveys. A survey is submitted to clients as a single XML file.

JEDI Surveyor (js.exe): 
	Tool to reply to survey questions. The survey is submitted via e-mail to a configurable recipient when finished.

JEDI Survey Reporter (jsr.exe): 
	Used to collect responses from clients. Displays statistics and results from the surveys. Can also export reports to other formats (XML, HTML etc)


DESIGN NOTES
A survey is viewed as a collection of survey questions. Each question have a Title, a Description, a Type, a Required flag and one or several Choices, Type can be any of "Exclusive", "Multiple Choice" or "Free-Form". The Survey as such also have global settings for Title, Description, Release and Expiry Date, a HREF and a Recipient (the e-amil address where the response should be sent).

The basis of the entire survey complex is a collection of interfaces declared in JvSurveyIntf.pas and an implementation of these interfaces is provided in JvSurveyImpl.pas (and JvSurveyUtils.pas). Each program *uses* JvSurveyIntf and *includes* JvSurveyImpl. This means that within the application, no references are ever made to the actual implementation: all access to the survey is controlled through interface instances. The creation of an instance of the base interface, IJvSurvey, is controlled by a function pointer in JvSurveyIntf - "CreateSurvey": implementors must assign their own "factory function" to this pointer before the application tries to create an IJvSurvey instance and JvSurveyImpl assigns it's own function in its initialization section. This is why the JvSurveyImpl unit must be included in the project: without it, the CreateSurvey function pointer would be nil at run-time.

The implementation in JvSurveyImpl uses an XML file to store the surveys. Other implementations could use other means. As a matter of fact, the interfaces in JvSourceIntf only uses COM/OLE/Automation comaptible data types which means that an implementation would be possible using COM or even SOAP, greatly expanding the usability of these programs.

Since this is partly a JVCL showcase, the programs make heavy use of JVCL components and JCL utilities and you need to have JVCL and JCL installed and working to be able to compile them. The programs should be compatible with D5-D7 with the exception of the UTF8Encode/UTF8Decode functions: these are not available in D5 and are replaced with stubs when compiled with D5. You are welcome to implement the stubs, but as long as the survey file uses standard ASCII characters only, there should not be any problems.

THE XML FILE FORMAT
The format used in the XML file contains few surprises: the only thing special about is in the Description properties and in the CHOICES and RESPONSES values. Since the programs uses TJvLinkLabel to display the descriptions (for both SURVEY and ITEM), this means you can embed some simple HTML codes to format the text. Specifically, you can use <b></b> for bold, <i></i> for italic, <br> for line-breaks and <link>HREF</link> to display links. Note that links must point to a complete, valid URL of some type. This means you must include the protocol in the link (i.e "http://", "mailto:", "news:" etc). When the user clicks such a link, the ShellExecute API function is called to perform the action and any errors are ignored.

The CHOICES and RESPONSES values are encoded as a delimited list of values. Currently, the delimiter is defined as semi-colon (";") but this can be changed by editing the cRecordSeparator constant in JvSurveyUtils.pas. NB! that if you change this value after sending surveys (or the programs) to users, the programs will not work with newer versions of survey files. The use of the delimiter also means that you cannot use the delimiter within an item. Additionally, if you would like to use line-breaks in CHOICES, use a \n instead (this differs from the Description, where you should use <br> for explicit line-breaks).

Each RESPONSE value is filled with a delimited list of responses (if Type is either Exclusive or Multiple). Each delimited value is either a "0" (item is unchecked) or a "1" (item is checked). If the type is Free-Form, RESPONSES is instead filled with the text typed by the user. CRLF's are converted to \n in this case.

Example of CHOICES entry in XML file:

<CHOICES>Yes;No</CHOICES>

Example of RESPONSES in XML file (after user has saved/submitted the survey):
<RESPONSES>0;1</RESPONSES>

In this example, the user checked the "No" item. 

 