{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHtmlParserEditor.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQHtmlParserEditor;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,  
  QForms,  
  DesignEditors, DesignIntf, 
  JvQParserForm;

type
  TJvHtmlParserEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
  end;

implementation

uses
  QControls, QDialogs, JvQDsgnConsts;

function TJvHtmlParserEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paReadOnly];
end;

function TJvHtmlParserEditor.GetValue: string;
begin
  Result := RsJvEditorString; // GetStrValue;
end;

procedure TJvHtmlParserEditor.SetValue(const Value: string);
begin
  SetStrValue(RsJvEditorString);
end;

procedure TJvHtmlParserEditor.GetValues(Proc: TGetStrProc);
begin
  SetStrValue(RsJvEditorString);
end;

procedure TJvHtmlParserEditor.Edit;
var
  Dlg: TJvHTMLParserForm;
  Res: TStrings;
begin
  Res := TStrings(GetOrdValue);
  Dlg := TJvHTMLParserForm.Create(Application);
  try
    Dlg.LoadFromStr(Res);
    if (Dlg.ShowModal = mrOK) or (Dlg.Tag = 0) then
    begin
      Dlg.SaveToStr(Res);
//      SetOrdValue(Integer(Res));
    end;
  finally
    Dlg.Free;
  end;
end;

end.

