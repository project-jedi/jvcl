{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorEditors.pas, released on 2004-09-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFullColorEditors;

{$I jvcl.inc}

interface

uses
  Windows, Classes, DesignIntf, DesignEditors, VCLEditors,
  ComCtrls,
  JvFullColorCtrls;

type
  TJvColorIDEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

procedure Register;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, TypInfo,
  JvFullColorSpaces;

//=== { TJvColorIDEEditor } ==================================================

function TJvColorIDEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paRevertable, paRevertable,
             paNotNestable, paMultiSelect];
end;

function TJvColorIDEditor.GetValue: string;
begin
  Result := ColorSpaceManager.ColorSpace[TJvFullColorSpaceID(GetOrdValue)].ShortName;
end;

procedure TJvColorIDEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  with ColorSpaceManager do
    for I := 0 to Count - 1 do
      Proc(ColorSpaceByIndex[I].ShortName);
end;

procedure TJvColorIDEditor.SetValue(const Value: string);
var
  I: Integer;
  CS: TJvColorSpace;
begin
  with ColorSpaceManager do
    for I := 0 to Count - 1 do
    begin
      CS := ColorSpaceByIndex[I];
      if CompareText(CS.ShortName, Value) = 0 then
      begin
        SetOrdValue(Ord(CS.ID));
        Exit;
      end;
    end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TJvFullColorSpaceID), nil, '', TJvColorIDEditor);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\design'
    );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

