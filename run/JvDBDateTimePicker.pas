{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBDateTimePicker.PAS, released May 8, 2000

The Initial Developer of the Original Code is Eko Subagio (ekosbg att bigfoot dott com)
Portions created by Eko Subagio are Copyright (C) 2000 Eko Subagio.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s): ______________________________________.

               by Eko Subagio
Current Version: 1.00

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  (rom) comments should be ripped by the help writer
-----------------------------------------------------------------------------}
/////////////////////////////////////////////////////////////////////////
// TJvDBDateTimePicker
// Copyright(c)2000 Eko Subagio
// TJvDBDateTimePicker is derived from TDateTimePicker from Delphi 5
// TDateTimePicker Copyright(c) 2000 Borland/Inprise.
// Extending and add capability to integrate with database
// www.geocities.com/ekosbg
/////////////////////////////////////////////////////////////////////////
// $Id$

unit JvDBDateTimePicker;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Controls, DB, DBCtrls,
  JvDateTimePicker;

type
  TJvDBDateTimePicker = class(TJvDateTimePicker)
  private
    FDataLink: TFieldDataLink;
    FBeepOnError: Boolean;
    FTrimValue: Boolean;
    FIsReadOnly: Boolean;
    FPaintControl: TPaintControl;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    procedure SetDataField(Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure EditingChange(Sender: TObject);
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function IsDateAndTimeField: Boolean;
    // Adding capability to edit
    procedure DoExit; override;
    procedure DataChange(Sender: TObject);
    // Adding capability to edit
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change; override;
    procedure UpdateData(Sender: TObject);
    // On Close Up & Drop Down
    procedure CalendarOnCloseUp(Sender: TObject);
    procedure CalendarOnDropDown(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property TrimValue: Boolean read FTrimValue write FTrimValue default True;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  SysUtils, ComCtrls, CommCtrl, 
  JvConsts;

//=== { TJvDBDateTimePicker } ================================================

///////////////////////////////////////////////////////////////////////////
//constructor TJvDBDateTimePicker.Create
//Parameter   : AOwner as TComponent
//Description : As Constructor the procedure had have responsibility to
//              handle new instance for initial new value.
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

constructor TJvDBDateTimePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsReadOnly := True;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
  OnCloseUp := CalendarOnCloseUp;
  OnDropDown := CalendarOnDropDown;
  FBeepOnError := True;
  FTrimValue := True;
  FPaintControl := TPaintControl.Create(Self, DATETIMEPICK_CLASS);
end;

///////////////////////////////////////////////////////////////////////////
//Destructor TJvDBDateTimePicker.Destroy
//Parameter   : None
//Description : Destructor had have responsibility to destroy all garbage
//              that had been used in Constructor, free anything in here
//              after anything is initialized in Constructor
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

destructor TJvDBDateTimePicker.Destroy;
begin
  OnCloseUp := nil;
  OnDropDown := nil;
  FPaintControl.Free;
  FDataLink.OnDataChange := nil;
  FDataLink.OnUpdateData := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

///////////////////////////////////////////////////////////////////////////
//procedure    : TJvDBDateTimePicker.CalendarOnCloseUp
//Parameter    : Sender as TObject
//Descriptions : To set the dataset into edit mode, when the user
//               closing up the Calendar.
//Revision     : October 18, 2000 ekosbg att bigfoot dott com
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.CalendarOnCloseUp(Sender: TObject);
begin
  FDataLink.Edit;
end;

///////////////////////////////////////////////////////////////////////////
//procedure    : TJvDBDateTimePicker.CalendarOnDropDown
//Parameter    : Sender as TObject
//Descriptions : To set the dataset into edit mode, when the user
//               dropping down the Calendar.
//Revision     : October 18, 2000 ekosbg att bigfoot dott com
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.CalendarOnDropDown(Sender: TObject);
begin
  FDataLink.Edit;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.Change;
//Description : We should maintain the changes in TJvDBDateTimePicker to
//              datalink, in order to notify datalink that it was changed.
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.Change;
begin
  // call method modified
  FDataLink.Edit;
//  FDataLink.Modified;
  // we still need parent code
  inherited Change;
  UpdateData(Self);
end;

procedure TJvDBDateTimePicker.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.DataChange
//Parameter   : Sender as TObject
//Description : DataChange had have responsibility to make data in control
//              always up to date with the current value in database
//              This is event handler for TFieldDataLink event property
//              OnDataChange
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if Kind = dtkDate then
    begin
      if IsDateAndTimeField then
        DateTime := FDataLink.Field.AsDateTime
      else
        DateTime := Trunc(FDataLink.Field.AsDateTime);
    end
    else
    begin
      if IsDateAndTimeField then
        DateTime := FDataLink.Field.AsDateTime
      else
        DateTime := Frac(FDataLink.Field.AsDateTime);
    end;
  end
  else
  if csDesigning in ComponentState then
  begin
    DateTime := Now;
  end;
  CheckNullValue;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.DoExit
//Description : User action , She/He leave the control.......
//              We should tell to database that is leave and database
//             should be updated using datalink value
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.DoExit;
begin
  // trapping in exception
  try
    // Changes should Reflect database
    FDataLink.UpdateRecord;
  except
    // Only got an error the focus will not leave the control
    SetFocus;
  end;
  // We needs the method behavior from parents of DoExit;
  inherited DoExit;
end;

procedure TJvDBDateTimePicker.EditingChange(Sender: TObject);
begin
  FIsReadOnly := not FDataLink.Editing;
end;

//function TJvDBDateTimePicker.GetDataField
//Return Value : String
//Description  : The function retrieve for fieldname from specified
//               datasource
//Revision     : August 30, 2000
//Author       : -ekosbg-
///////////////////////////////////////////////////////////////////////////

function TJvDBDateTimePicker.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

///////////////////////////////////////////////////////////////////////////
//function TJvDBDateTimePicker.GetDataSource
//Return Value : TDataSource
//Description  : The function retrieve DataSource from specified Table
//               To make connection with database
//Revision     : August 30, 2000
//Author       : -ekosbg-
///////////////////////////////////////////////////////////////////////////

function TJvDBDateTimePicker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TJvDBDateTimePicker.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

function TJvDBDateTimePicker.IsDateAndTimeField: Boolean;
begin
  with FDataLink do
    Result := (Field <> nil) and
      (Field.DataType in [ftDateTime {$IFDEF COMPILER6_UP}, ftTimeStamp {$ENDIF}]) and
      not TrimValue;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.KeyDown
//Parameter   : Key as Word by references,
//              ShiftState as TShiftState, this is enumeration type
//Description : Handling user action what should to do ? The control should
//              tell to datalink that they should change mode to edit doing
//              an action such as delete, insert or...you guess it
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.KeyDown(var Key: Word; Shift: TShiftState);
const
  cAllowedKeysWhenReadOnly = [VK_LEFT, VK_RIGHT];
begin
  { Only allow left and right arrow when read-only, don't care about Shift }
  if not (Key in cAllowedKeysWhenReadOnly) and FIsReadOnly and not FDataLink.CanModify then
  begin
    if BeepOnError then
      Beep;
    Key := 0;
    Exit;
  end;

  // we still parent code
  inherited KeyDown(Key, Shift);
  // Is it Delete key, insert key or shiftstate ...
  case Key of
    VK_DELETE:
      if Shift * KeyboardShiftStates = [] then
      begin
        FDataLink.Edit;
        if Kind = dtkDate then
        begin
          if IsDateAndTimeField then
            DateTime := NullDate
          else
            DateTime := Trunc(NullDate);
        end
        else
        begin
          if IsDateAndTimeField then
            DateTime := NullDate
          else
            DateTime := Frac(NullDate);
        end;
        CheckNullValue;
        UpdateData(Self);
      end;
    VK_INSERT:
      if (Shift * KeyboardShiftStates = [ssShift]) then
        FDataLink.Edit;
    else
      FDataLink.Edit;
  end;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.KeyPress
//Parameter   : Key as Char by references when the key changes it will
//              reflect to the sender parameter variable.
//Description : Handling user action what should to do ?
//              Hmmm... ok, first of all the character that user typed
//              should be checked, if it is invalid ignored the character.
//              Otherwise, tell to datalink that the mode should change
//              to edit.
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.KeyPress(var Key: Char);
begin
  if FIsReadOnly and not FDataLink.CanModify then
  begin
    if BeepOnError then
      Beep;
    Key := #0;
    Exit;
  end;

  inherited KeyPress(Key);
  if (Key in [#32..#255]) and ((FDataLink.Field <> nil) and
    not (FDataLink.Field.IsValidChar(Key))) then
  begin
    if BeepOnError then
      Beep;
    Key := #0;
  end;
  case Key of
    #32..#255:
      FDataLink.Edit;
    Esc:
      begin
        FDataLink.Reset;
        SetFocus;
      end;
  end;
end;

procedure TJvDBDateTimePicker.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.SetDataField
//Parameter    : Value as String
//Description  : The procedure is handling the capability to set the
//               DataField property
//Revision     : August 30, 2000
//Author       : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.SetDataField(Value: string);
begin
  FDataLink.FieldName := Value;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.SetDataSource
//Parameter    : Value as TDataSource
//Description  : The procedure is handling the capability to set the
//               DataSource property
//Revision     : August 30, 2000
//Author       : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

procedure TJvDBDateTimePicker.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

///////////////////////////////////////////////////////////////////////////
//procedure TJvDBDateTimePicker.UpdateDate
//Parameter   :
//Description : We should change the value in datalink, and this is the
//              procedure to handle that event. It will assign with
//              event property Datalink, that is OnUpdateData
//Revision    : August 30, 2000
//Author      : -ekosbg-
///////////////////////////////////////////////////////////////////////////

procedure TJvDBDateTimePicker.UpdateData(Sender: TObject);
begin
  // update value in datalink with date value in control, not from system
  if not FDataLink.Editing then
    Exit;
  if Kind = dtkDate then
  begin
    if Trunc(NullDate) = Trunc(DateTime) then
      FDataLink.Field.Value := Null
    else
    if IsDateAndTimeField then
      FDataLink.Field.AsDateTime := DateTime
    else
      FDataLink.Field.AsDateTime := Trunc(DateTime);
  end
  else
  begin
    if Frac(NullDate) = Frac(DateTime) then
      FDataLink.Field.Value := Null
    else
    if IsDateAndTimeField then
      FDataLink.Field.AsDateTime := DateTime
    else
      FDataLink.Field.AsDateTime := Frac(DateTime);
  end;
end;

procedure TJvDBDateTimePicker.WMPaint(var Msg: TWMPaint);
var
  D: TDateTime;
  ST: TSystemTime;
begin
  if not (csPaintCopy in ControlState) then inherited else
  begin
    if Kind = dtkDate then
    begin
      if IsDateAndTimeField then
        D := FDataLink.Field.AsDateTime
      else
        D := Trunc(FDataLink.Field.AsDateTime);
    end
    else
    begin
      if IsDateAndTimeField then
        D := FDataLink.Field.AsDateTime
      else
        D := Frac(FDataLink.Field.AsDateTime);
    end;

    DateTimeToSystemTime(D, ST);
    DateTime_SetSystemTime(FPaintControl.Handle, GDT_VALID, ST);
    SendMessage(FPaintControl.Handle, WM_ERASEBKGND, Msg.DC, 0);
    SendMessage(FPaintControl.Handle, WM_PAINT, Msg.DC, 0);
  end;
end;

procedure TJvDBDateTimePicker.WMLButtonDown(var Msg: TWMLButtonDown);
begin
  if FIsReadOnly and not FDataLink.CanModify then
  begin
    SendCancelMode(Self);
    SetFocus;
    Exit;
  end;

  inherited;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

