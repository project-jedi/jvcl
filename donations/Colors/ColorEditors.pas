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
  ColorCtrls;

type
  TArrowPositionEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

  TColorOrientationEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

  TColorIDEditor = class(TPropertyEditor)
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

//=== { TArrowPositionEditor } ===============================================

function TArrowPositionEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paAutoUpdate, paSortList, paRevertable, paNotNestable];
end;

function TArrowPositionEditor.GetValue: string;
begin
  Result := '';
  if GetComponent(0) is TColorTrackBar then
    case (GetComponent(0) as TColorTrackBar).BarOrientation of
      boHorizontal:
        case GetOrdValue of
          0:
            Result := 'apTop';
          1:
            Result := 'apBottom';
        end;
      boVertical:
        case GetOrdValue of
          0:
            Result := 'apLeft';
          1:
            Result := 'apRight';
        end;
    end;
end;

procedure TArrowPositionEditor.GetValues(Proc: TGetStrProc);
begin
  if GetComponent(0) is TColorTrackBar then
    case (GetComponent(0) as TColorTrackBar).BarOrientation of
      boHorizontal:
        begin
          Proc('apTop');
          Proc('apBottom');
        end;
      boVertical:
        begin
          Proc('apLeft');
          Proc('apRight');
        end;
    end;
end;

procedure TArrowPositionEditor.SetValue(const Value: string);
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

//=== { TColorOrientation } ==================================================

function TColorOrientationEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paAutoUpdate, paSortList, paRevertable, paNotNestable];
end;

function TColorOrientationEditor.GetValue: string;
begin
  Result := '';
  if GetComponent(0) is TColorTrackBar then
    case (GetComponent(0) as TColorTrackBar).BarOrientation of
      boHorizontal:
        case GetOrdValue of
          0:
            Result := 'coLeftToRight';
          1:
            Result := 'coRightToLeft';
        end;
      boVertical:
        case GetOrdValue of
          0:
            Result := 'coTopToBottom';
          1:
            Result := 'coBottomToTop';
        end;
    end;
end;

procedure TColorOrientationEditor.GetValues(Proc: TGetStrProc);
begin
  if GetComponent(0) is TColorTrackBar then
    case (GetComponent(0) as TColorTrackBar).BarOrientation of
      boHorizontal:
        begin
          Proc('coLeftToRight');
          Proc('coRightToLeft');
        end;
      boVertical:
        begin
          Proc('coTopToBottom');
          Proc('coBottomToTop');
        end;
    end;
end;

procedure TColorOrientationEditor.SetValue(const Value: string);
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

//=== { TColorIDEditor } =====================================================

function TColorIDEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paRevertable, paNotNestable];
end;

function TColorIDEditor.GetValue: string;
begin
  Result := ColorSpaceManager.ColorSpace[TColorID(GetOrdValue)].ShortName;
end;

procedure TColorIDEditor.GetValues(Proc: TGetStrProc);
var
  Index: Integer;
begin
  with ColorSpaceManager do
    for Index := 0 to ColorSpaceCount - 1 do
      Proc(ColorSpaceIndex[Index].ShortName);
end;

procedure TColorIDEditor.SetValue(const Value: string);
var
  Index: Integer;
  LColorSpace: TColorSpace;
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
  RegisterPropertyEditor(TypeInfo(TColorOrientation), nil, '', TColorOrientationEditor);
  RegisterPropertyEditor(TypeInfo(TArrowPosition), nil, '', TArrowPositionEditor);
  RegisterPropertyEditor(TypeInfo(TColorID), nil, '', TColorIDEditor);
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

