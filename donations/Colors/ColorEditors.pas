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
  Windows, Classes, DesignIntf, DesignEditors, VCLEditors,
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
  TypInfo,
  ColorSpaces;

const
  cPropapTop = 'apTop';
  cPropapBottom = 'apBottom';
  cPropapLeft = 'apLeft';
  cPropapRight = 'apRight';

  cPropcoLeftToRight = 'coLeftToRight';
  cPropcoRightToLeft = 'coRightToLeft';
  cPropcoTopToBottom = 'coTopToBottom';
  cPropcoBottomToTop = 'coBottomToTop';

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
        case TJvArrowPosition(GetOrdValue) of
          apTop:
            Result := cPropapTop;
          apBottom:
            Result := cPropapBottom;
        end;
      trVertical:
        case TJvArrowPosition(GetOrdValue) of
          apLeft:
            Result := cPropapLeft;
          apRight:
            Result := cPropapRight;
        end;
    end;
end;

procedure TJvArrowPositionEditor.GetValues(Proc: TGetStrProc);
begin
  if GetComponent(0) is TJvFullColorTrackBar then
    case (GetComponent(0) as TJvFullColorTrackBar).Orientation of
      trHorizontal:
        begin
          Proc(cPropapTop);
          Proc(cPropapBottom);
        end;
      trVertical:
        begin
          Proc(cPropapLeft);
          Proc(cPropapRight);
        end;
    end;
end;

procedure TJvArrowPositionEditor.SetValue(const Value: string);
begin
  if Value = cPropapLeft then
    SetOrdValue(Ord(apLeft))
  else
  if Value = cPropapRight then
    SetOrdValue(Ord(apRight))
  else
  if Value = cPropapTop then
    SetOrdValue(Ord(apTop))
  else
  if Value = cPropapBottom then
    SetOrdValue(Ord(apBottom));
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
        case TJvColorOrientation(GetOrdValue) of
          coLeftToRight:
            Result := cPropcoLeftToRight;
          coRightToLeft:
            Result := cPropcoRightToLeft;
        end;
      trVertical:
        case TJvColorOrientation(GetOrdValue) of
          coTopToBottom:
            Result := cPropcoTopToBottom;
          coBottomToTop:
            Result := cPropcoBottomToTop;
        end;
    end;
end;

procedure TJvColorOrientationEditor.GetValues(Proc: TGetStrProc);
begin
  if GetComponent(0) is TJvFullColorTrackBar then
    case (GetComponent(0) as TJvFullColorTrackBar).Orientation of
      trHorizontal:
        begin
          Proc(cPropcoLeftToRight);
          Proc(cPropcoRightToLeft);
        end;
      trVertical:
        begin
          Proc(cPropcoTopToBottom);
          Proc(cPropcoBottomToTop);
        end;
    end;
end;

procedure TJvColorOrientationEditor.SetValue(const Value: string);
begin
  if Value = cPropcoLeftToRight then
    SetOrdValue(Ord(coLeftToRight))
  else
  if Value = cPropcoRightToLeft then
    SetOrdValue(Ord(coRightToLeft))
  else
  if Value = cPropcoTopToBottom then
    SetOrdValue(Ord(coTopToBottom))
  else
  if Value = cPropcoBottomToTop then
    SetOrdValue(Ord(coBottomToTop));
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
        SetOrdValue(Ord(LColorSpace.ID));
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

