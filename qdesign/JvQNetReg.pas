{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvNetReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

Last Modified: 2004-03-22

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQNetReg;

interface

procedure Register;

implementation

uses
  Classes,
  
  DesignEditors, DesignIntf,

  {$IFDEF MSWINDOWS}
  {JvQMail,  JvQMailEditor,} JvQUrlGrabbers,
  JvQUrlListGrabber, JvQUrlListGrabberEditors,
  {$ENDIF MSWINDOWS}
  JvQHtmlParser,  JvQHtmlParserEditor,
  JvQTypes, JvQDsgnConsts,
  JvQStringListToHtml, JvQFormToHtml, JvQRgbToHtml,  JvQStrToHtml;



{$R ../Resources/JvNetReg.dcr}


procedure Register;
begin
  RegisterComponents(RsPaletteInterNetWork, [
    {$IFDEF MSWINDOWS}
    TJvFTPURLGrabber, TJvHTTPURLGrabber,
    TJvLocalFileURLGrabber, {TJvMail,}
    {$ENDIF MSWINDOWS}
    TJvHTMLParser,
    TJvStrToHTML, TJvStringListToHTML, TJvFormToHTML, TJvRGBToHTML
    {$IFDEF MSWINDOWS}
    ,TJvUrlListGrabber
    {$ENDIF MSWINDOWS}
    ]);
  RegisterPropertyEditor(TypeInfo(TJvParserInfoList),
    TJvHTMLParser, 'Parser', TJvHTMLParserEditor);
  {$IFDEF MSWINDOWS}
  RegisterPropertyEditor(TypeInfo(TJvUrlGrabberIndex),
    TJvUrlListGrabber, '', TJvUrlGrabberIndexProperty);
  RegisterPropertyEditor(TypeInfo(TJvUrlGrabberDefaultPropertiesList),
    TJvUrlListGrabber, '', TJvUrlGrabberDefaultPropertiesListEditor);
  RegisterPropertyEditor(TypeInfo(TJvCustomUrlGrabberDefaultProperties),
    TJvUrlGrabberDefPropEdTrick, '', TJvUrlGrabberDefaultPropertiesEditor);
  {$ENDIF MSWINDOWS}
end;

end.
