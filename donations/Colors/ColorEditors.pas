{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorEditors.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit ColorEditors;

{$I jvcl.inc}

interface

uses
  Windows, Classes, Forms, DesignIntf, DesignEditors, VCLEditors, Graphics,
  ComCtrls,
  ColorCtrls;

type
  TJvArrowPositionEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

  TJvColorOrientationEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

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
  TypInfo, Math, GraphUtil, SysUtils,
  ColorSpaces;

//=== { TJvArrowPositionEditor } =============================================

function TJvArrowPositionEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paAutoUpdate, paSortList, paRevertable, paNotNestable];
end;

function TJvArrowPositionEditor.GetValue: string;
begin
  Result := '';
  if GetComponent(0) is TJvFullColorTrackBar then
    case (GetComponent(0) as TJvFullColorTrackBar).Orientation of
      trHorizontal:
        case GetOrdValue of
          0:
            Result := 'apTop';
          1:
            Result := 'apBottom';
        end;
      trVertical:
        case GetOrdValue of
          0:
            Result := 'apLeft';
          1:
            Result := 'apRight';
        end;
    end;
end;

procedure TJvArrowPositionEditor.GetValues(Proc: TGetStrProc);
begin
  if GetComponent(0) is TJvFullColorTrackBar then
    case (GetComponent(0) as TJvFullColorTrackBar).Orientation of
      trHorizontal:
        begin
          Proc('apTop');
          Proc('apBottom');
        end;
      trVertical:
        begin
          Proc('apLeft');
          Proc('apRight');
        end;
    end;
end;

procedure TJvArrowPositionEditor.SetValue(const Value: string);
begin
  if Value = 'apLeft' then
    SetOrdValue(0)
  else
  if Value = 'apRight' then
    SetOrdValue(1)
  else
  if Value = 'apTop' then
    SetOrdValue(0)
  else
  if Value = 'apBottom' then
    SetOrdValue(1);
end;

//=== { TJvColorOrientationEditor } ==========================================

function TJvColorOrientationEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paAutoUpdate, paSortList, paRevertable, paNotNestable];
end;

function TJvColorOrientationEditor.GetValue: string;
begin
  Result := '';
  if GetComponent(0) is TJvFullColorTrackBar then
    case (GetComponent(0) as TJvFullColorTrackBar).Orientation of
      trHorizontal:
        case GetOrdValue of
          0:
            Result := 'coLeftToRight';
          1:
            Result := 'coRightToLeft';
        end;
      trVertical:
        case GetOrdValue of
          0:
            Result := 'coTopToBottom';
          1:
            Result := 'coBottomToTop';
        end;
    end;
end;

procedure TJvColorOrientationEditor.GetValues(Proc: TGetStrProc);
begin
  if GetComponent(0) is TJvFullColorTrackBar then
    case (GetComponent(0) as TJvFullColorTrackBar).Orientation of
      trHorizontal:
        begin
          Proc('coLeftToRight');
          Proc('coRightToLeft');
        end;
      trVertical:
        begin
          Proc('coTopToBottom');
          Proc('coBottomToTop');
        end;
    end;
end;

procedure TJvColorOrientationEditor.SetValue(const Value: string);
begin
  if Value = 'coLeftToRight' then
    SetOrdValue(0)
  else
  if Value = 'coRightToLeft' then
    SetOrdValue(1)
  else
  if Value = 'coTopToBottom' then
    SetOrdValue(0)
  else
  if Value = 'coBottomToTop' then
    SetOrdValue(1);
end;

//=== { TJvColorIDEditor } ===================================================

function TJvColorIDEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable, paNotNestable];
end;

function TJvColorIDEditor.GetValue: string;
begin
  Result := ColorSpaceManager.ColorSpace[TJvColorID(GetOrdValue)].ShortName;
end;

procedure TJvColorIDEditor.GetValues(Proc: TGetStrProc);
var
  Index: Integer;
begin
  with ColorSpaceManager do
    for Index := 0 to ColorSpaceCount - 1 do
      Proc(ColorSpaceIndex[Index].ShortName);
end;

procedure TJvColorIDEditor.SetValue(const Value: string);
var
  Index: Integer;
  LColorSpace: TJvColorSpace;
begin
  with ColorSpaceManager do
    for Index := 0 to ColorSpaceCount - 1 do
    begin
      LColorSpace := ColorSpaceIndex[Index];
      if LColorSpace.ShortName = Value then
      begin
        SetOrdValue(LColorSpace.ID);
        Exit;
      end;
    end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TJvColorOrientation), nil, '', TJvColorOrientationEditor);
  RegisterPropertyEditor(TypeInfo(TJvArrowPosition), nil, '', TJvArrowPositionEditor);
  RegisterPropertyEditor(TypeInfo(TJvColorID), nil, '', TJvColorIDEditor);
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

