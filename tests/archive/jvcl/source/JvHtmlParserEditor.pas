{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHtmlParserEditor.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvHtmlParserEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   {$IFDEF COMPILER5} DsgnIntf, {$ENDIF} {$IFDEF COMPILER6_UP} DesignEditors, DesignIntf, {$ENDIF}
  JvFormParser;

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

{*************************************************}

function TJvHtmlParserEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paSortList];
end;

{*************************************************}

function TJvHtmlParserEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{*************************************************}

procedure TJvHtmlParserEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

{*************************************************}

procedure TJvHtmlParserEditor.GetValues(Proc: TGetStrProc);
begin
  SetStrValue('Click to edit...');
end;

{*************************************************}

procedure TJvHtmlParserEditor.Edit;
var
  Dlg: TFormParsers;
  res: TStringList;
begin
  res := TStringList(GetOrdValue);
  Dlg := TFormParsers.Create(Application);
  Dlg.LoadFromStr(res);
  try
    Dlg.ShowModal;
    if Dlg.Tag = 0 then
    begin
      res.Assign(Dlg.SetFromStr);
      SetOrdValue(Integer(res));
    end;
  finally
    Dlg.Free;
  end;
end;

end.
