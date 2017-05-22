{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBEditors.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBGridExportEditors;

interface

{$I jvcl.inc}

uses
  Classes,
  DesignIntf, DesignEditors, VCLEditors;

type
  TJvDBGridExportWordFormatProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

implementation

uses
  SysUtils,
  JvDBGridExport, JvDsgnConsts;

function TJvDBGridExportWordFormatProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paRevertable];
end;

function TJvDBGridExportWordFormatProperty.GetValue: string;
begin
  if not IntToWordGridFormatIdent(GetOrdValue, Result) then
    Result := IntToStr(GetOrdValue);
end;

procedure TJvDBGridExportWordFormatProperty.GetValues(Proc: TGetStrProc);
begin
  GetWordGridFormatValues(Proc);
end;

procedure TJvDBGridExportWordFormatProperty.SetValue(const Value: string);
var
  N: Integer;
begin
  if WordGridFormatIdentToInt(Value, N) then
    SetOrdValue(N)
  else
    raise Exception.CreateRes(@RsEInvalidPropertyValue);
end;

end.
