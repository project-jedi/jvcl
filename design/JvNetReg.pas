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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvNetReg;

interface

{$IFDEF MSWINDOWS}
{$DEFINE USEWINDOWS}
{$ENDIF MSWINDOWS}


procedure Register;

implementation

uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  {$IFDEF VCL}
  JvRichEditToHTML, JvMail, JvMailEditor,
  {$ENDIF VCL}
  {$IFDEF USEWINDOWS}
  JvUrlListGrabber, JvUrlGrabbers, JvUrlListGrabberEditors,
  {$ENDIF USEWINDOWS}
  JvHtmlParser, JvHtmlParserEditor,
  JvTypes, JvDsgnConsts,
  JvStringListToHtml, JvFormToHtml, JvRgbToHtml, JvStrToHtml;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvNetReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvNetReg.dcr}
{$ENDIF LINUX}

procedure Register;
begin
  RegisterComponents(RsPaletteInterNetWork, [
    {$IFDEF USEWINDOWS}
    TJvFTPURLGrabber, TJvHTTPURLGrabber,
    TJvLocalFileURLGrabber,
    {$ENDIF USEWINDOWS}
    {$IFDEF VCL}
    TJvMail,
    {$ENDIF VCL}
    TJvHTMLParser,
    {$IFDEF VCL}
    TJvRichEditToHTML,
    {$ENDIF VCL}
    {$IFDEF USEWINDOWS}
    TJvUrlListGrabber,
    {$ENDIF USEWINDOWS}
    TJvStrToHTML, TJvStringListToHTML, TJvFormToHTML, TJvRGBToHTML]);
  {$IFDEF USEWINDOWS}
  RegisterPropertyEditor(TypeInfo(TStrings),
    TJvHTMLParser, 'Parser', TJvHTMLParserEditor);
  RegisterPropertyEditor(TypeInfo(TJvUrlGrabberIndex),
    TJvUrlListGrabber, '', TJvUrlGrabberIndexProperty);
  RegisterPropertyEditor(TypeInfo(TJvUrlGrabberDefaultPropertiesList),
    TJvUrlListGrabber, '', TJvUrlGrabberDefaultPropertiesListEditor);
  RegisterPropertyEditor(TypeInfo(TJvCustomUrlGrabberDefaultProperties),
    TJvUrlGrabberDefPropEdTrick, '', TJvUrlGrabberDefaultPropertiesEditor);
  {$IFDEF VCL}
  RegisterComponentEditor(TJvMail, TJvMailEditor);
  {$ENDIF VCL}
  {$ENDIF USEWINDOWS}
end;

end.
