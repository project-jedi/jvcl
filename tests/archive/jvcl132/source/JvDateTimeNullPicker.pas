{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDateTimeNullPicker.pas, released 26 July 2000.

The Initial Developer of the Original Code is Russell Fox.
Portions created by Russell Fox are Copyright (C) 1999-2001 Russell Fox.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvDateTimeNullPicker;

{ Used when date value is optional and we need to have a null/0 value }
{ and dont want to see  1899/12/31 etc }
{ Set the Caption for the Label to be shown if there is 0 date}
{ updated from only having one tdatetimepicker and as such only date or time}

interface

uses
  {delphi} ExtCtrls, ComCtrls, Classes, StdCtrls, Controls, Graphics,
  { local} JvComponentFunctions, JvComponent;

type
  TCustomDateTimeNullPicker = class(TJvCustomPanel)
  private
    fcDatePicker: TDateTimePicker;
    fcTimePicker: TDateTimePicker;
    fcCheckBox: TCheckBox;
    fclblCaption: TLabel;
    fbHasDateTime: boolean;

    FOnUserInput: TDTParseInputEvent;
    FOnChange: TNotifyEvent;
    feKind: TdtKind;
    fbHideCheckbox: Boolean;

    { setters and getters}
    function GetDateTime: TDateTime;
    procedure SetDateTime(const pcValue: TDateTime);
    procedure SetHasDateTime(const pbValue: boolean);
    function GetDate: TDate;
    function GetTime: TTime;
    procedure SetDate(const pcValue: TDate);
    procedure SetTime(const pcValue: TTime);
    function GetDateFormat: TDTDateFormat;
    procedure SetDateFormat(const pcValue: TDTDateFormat);
    function GetMaxDate: TDate;
    function GetMinDate: TDate;
    procedure SetMaxDate(const pcValue: TDate);
    procedure SetMinDate(const pcValue: TDate);

    function GetOnCloseUp: TNotifyEvent;
    procedure SetOnCloseUp(const pcValue: TNotifyEvent);
    function GetOnDropDown: TNotifyEvent;
    procedure SetOnDropDown(const pcValue: TNotifyEvent);
    function GetDroppedDown: boolean;
    function GetCaption: string;
    procedure SetCaption(const psValue: string);

    { encapsulated Events}
    procedure OnEncapsChange(Sender: TObject);
    procedure OnEncapsClick(Sender: TObject);
    procedure OnEncapsDblClick(Sender: TObject);
    procedure OnEncapsDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure OnEncapsDragOver(Sender, Source: TObject; X, Y: integer;
      State: TDragState; var Accept: boolean);
    procedure OnEncapsEndDrag(Sender, Target: TObject; X, Y: integer);
    procedure OnEncapsEnter(Sender: TObject);
    procedure OnEncapsExit(Sender: TObject);
    procedure OnEncapsKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure OnEncapsKeyPress(Sender: TObject; var Key: char);
    procedure OnEncapsKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure OnEncapsStartDrag(Sender: TObject; var DragObject: TDragObject);
    {custom events}
    procedure CheckboxClick(Sender: TObject);
    procedure SetKind(const peValue: TdtKind);
    procedure SetHideCheckbox(const pbValue: Boolean);
    function GetColor: TColor;
    function IsColorStored: Boolean;
    procedure SetColor(const peValue: TColor); 

  protected
    procedure Resize; override;
    procedure Loaded; override;

    procedure SetEnabled(pbValue: Boolean); override;
    function GetEnabled: Boolean; override;

    property Caption: string read GetCaption write SetCaption;
    property Color: TColor read GetColor write SetColor stored IsColorStored default clWindow;
    property Date: TDate read GetDate write SetDate;
    property DateFormat: TDTDateFormat read GetDateFormat write SetDateFormat;
    property Enabled: Boolean Read GetEnabled write SetEnabled;
    property HideCheckbox: Boolean read fbHideCheckbox write SetHideCheckbox;
    property Kind: TdtKind read feKind write SetKind;
    property MaxDate: TDate read GetMaxDate write SetMaxDate;
    property MinDate: TDate read GetMinDate write SetMinDate;
    property Time: TTime read GetTime write SetTime;
    property OnCloseUp: TNotifyEvent read GetOnCloseUp write SetOnCloseUp;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDropDown: TNotifyEvent read GetOnDropDown write SetOnDropDown;
    property OnUserInput: TDTParseInputEvent read FOnUserInput write FOnUserInput;

  public
    {TDatetime Properties}
    constructor Create(AOwner: TComponent); override;

    property DroppedDown: boolean read GetDroppedDown;
    property HasDateTime: boolean read fbHasDateTime write SetHasDateTime;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
  end;

  TJvDateTimeNullPicker = class(TCustomDateTimeNullPicker)
  published
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;

    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property Date;
    property DateFormat;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideCheckbox;
    property Kind;
    property MaxDate;
    property MinDate;

    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Time;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnCloseUp;
    property OnChange;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDropDown;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUserInput;
  end;

implementation

uses
  { delphi }SysUtils;

constructor TCustomDateTimeNullPicker.Create(AOwner: TComponent);
begin
  inherited;
  feKind := dtkDateOnly;
  ControlStyle     := ControlStyle - [csAcceptsControls];
  {create the owned controls}
  fccheckbox       := TCheckbox.Create(self);
  fccheckbox.Parent := self;
  fclblCaption     := TLabel.Create(self);
  fclblCaption.Parent := self;
  fcDatePicker := TDateTimePicker.Create(self);
  fcTimePicker := TDateTimePicker.Create(self);
  fcDatePicker.Parent := self;
  fcTimePicker.Parent := self;

  {set the GUI defaults}
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Height     := 25;
  Width      := 270;

  fbHasDateTime := true;

  fccheckbox.ParentFont  := True;
  fccheckbox.ParentColor := True;
  fccheckbox.Caption     := '';
  fccheckbox.OnClick     := CheckboxClick;
  fccheckbox.Checked     := fbHasDateTime;
  fccheckbox.OnClick     := OnEncapsChange;
  fccheckbox.OnDragDrop  := OnEncapsDragDrop;
  fccheckbox.OnDragOver  := OnEncapsDragOver;
  fccheckbox.OnEndDrag   := OnEncapsEndDrag;
  fccheckbox.OnEnter     := OnEncapsEnter;
  fccheckbox.OnExit      := OnEncapsExit;
  fccheckbox.OnKeyDown   := OnEncapsKeyDown;
  fccheckbox.OnKeyPress  := OnEncapsKeyPress;
  fccheckbox.OnKeyUp     := OnEncapsKeyUp;
  fccheckbox.OnStartDrag := OnEncapsStartDrag;

  fclblCaption.Visible := false;
  fclblCaption.ParentFont  := True;
  fclblCaption.ParentColor := True;
  fclblCaption.Caption     := 'Specify Date';
  fclblCaption.OnClick     := CheckboxClick;
  fclblCaption.OnClick     := OnEncapsChange;
  fclblCaption.OnDragDrop  := OnEncapsDragDrop;
  fclblCaption.OnDragOver  := OnEncapsDragOver;
  fclblCaption.OnEndDrag   := OnEncapsEndDrag;
  fclblCaption.OnStartDrag := OnEncapsStartDrag;

  fcDatePicker.ParentFont  := True;
  fcDatePicker.Anchors     := [akTop, akRight];
  fcDatePicker.DateTime    := now;
  fcDatePicker.OnChange    := FOnChange;
  fcDatePicker.OnUserInput := FOnUserInput;
  fcDatePicker.OnChange    := OnEncapsChange;
  fcDatePicker.OnClick     := OnEncapsClick;
  fcDatePicker.OnDblClick  := OnEncapsDblClick;
  fcDatePicker.OnDragDrop  := OnEncapsDragDrop;
  fcDatePicker.OnDragOver  := OnEncapsDragOver;
  fcDatePicker.OnEndDrag   := OnEncapsEndDrag;
  fcDatePicker.OnEnter     := OnEncapsEnter;
  fcDatePicker.OnExit      := OnEncapsExit;
  fcDatePicker.OnKeyDown   := OnEncapsKeyDown;
  fcDatePicker.OnKeyPress  := OnEncapsKeyPress;
  fcDatePicker.OnKeyUp     := OnEncapsKeyUp;
  fcDatePicker.OnStartDrag := OnEncapsStartDrag;
  fcDatePicker.Visible     := False;

  fcTimePicker.ParentFont  := True;
  fcTimePicker.Anchors     := [akTop, akRight];
  fcTimePicker.DateTime    := fcDatePicker.DateTime;
  fcTimePicker.OnChange    := FOnChange;
  fcTimePicker.DateMode := dmUpDown;
  fcTimePicker.Kind := dtkTime;
  fcTimePicker.OnUserInput := FOnUserInput;
  fcTimePicker.OnChange    := OnEncapsChange;
  fcTimePicker.OnClick     := OnEncapsClick;
  fcTimePicker.OnDblClick  := OnEncapsDblClick;
  fcTimePicker.OnDragDrop  := OnEncapsDragDrop;
  fcTimePicker.OnDragOver  := OnEncapsDragOver;
  fcTimePicker.OnEndDrag   := OnEncapsEndDrag;
  fcTimePicker.OnEnter     := OnEncapsEnter;
  fcTimePicker.OnExit      := OnEncapsExit;
  fcTimePicker.OnKeyDown   := OnEncapsKeyDown;
  fcTimePicker.OnKeyPress  := OnEncapsKeyPress;
  fcTimePicker.OnKeyUp     := OnEncapsKeyUp;
  fcTimePicker.OnStartDrag := OnEncapsStartDrag;
  fcTimePicker.Visible     := False;
end;

{private}


procedure TCustomDateTimeNullPicker.CheckboxClick(Sender: TObject);
begin
  HasDateTime := fccheckbox.Checked;
end;

function TCustomDateTimeNullPicker.GetDateTime: TDateTime;
begin
  Result := NullEquivalentDate;
  if HasDateTime then
  begin
    case feKind of
      dtkDateOnly: Result := DateOnly(fcDatePicker.DateTime);
      dtkTimeOnly: Result := TimeOnly(fcTimePicker.DateTime);
      dtkDateTime: Result := DateOnly(fcDatePicker.DateTime) + TimeOnly(fcTimePicker.DateTime);
    end;
  end;
end;

procedure TCustomDateTimeNullPicker.SetDateTime(const pcValue: TDateTime);
begin
  HasDateTime := not DateIsNull(pcValue, Kind);
  if pcValue = 0 then
  begin
    fcDatePicker.DateTime := pcValue;
    fcTimePicker.DateTime := pcValue;
    HasDateTime := not DateIsNull(pcValue, dtkDateTime);
  end
  else
  begin
    if DateIsNull(pcValue, dtkDateOnly) then
    begin
      fcDatePicker.DateTime := 0;
    end
    else
    begin
      fcDatePicker.Date := DateOnly(pcValue);
      fcDatePicker.Time := 0;
    end;

    if DateIsNull(pcValue, dtkTimeOnly) then
    begin
      fcTimePicker.DateTime := 0;
    end
    else
    begin
      fcTimePicker.Time := TimeOnly(pcValue);
      fcTimePicker.Date := 0;
    end;

  end;

end;

procedure TCustomDateTimeNullPicker.SetHasDateTime(const pbValue: boolean);
begin
  if pbValue <> fbHasDateTime then
  begin
    fbHasDateTime      := pbValue;
    fccheckbox.Checked := HasDateTime;
    if HasDateTime then
    begin
      {dont want to default to '1899/12/31'}
      if DateIsNull(DateTime, Kind) then
        DateTime := now;
    end;
    fcDatePicker.Visible := HasDateTime;
    fcTimePicker.Visible := HasDateTime;
    fclblCaption.Visible := not HasDateTime;
    Resize;
  end;
end;

function TCustomDateTimeNullPicker.GetDate: TDate;
begin
  {always return NullEquivalent as Value for not having date time}
  Result := NullEquivalentDate;
  if (HasDateTime) and (feKind in [dtkDateOnly,dtkDatetime]) then
    Result := DateOnly(fcDatePicker.Date);

end;

function TCustomDateTimeNullPicker.GetTime: TTime;
begin
  Result := NullEquivalentDate;
  {always return 0 as Value for not having date time}
  if (HasDateTime) and (feKind in [dtkTimeOnly,dtkDatetime]) then
    Result := TimeOnly(fcTimePicker.time);


end;

procedure TCustomDateTimeNullPicker.SetDate(const pcValue: TDate);
var lbShowCheckBox: Boolean;
begin
  lbShowCheckBox := false;
  try
    lbShowCheckBox := fcDatePicker.ShowCheckbox;
    if not lbShowCheckBox then
      fcDatePicker.ShowCheckbox := true;
    fcDatePicker.Date := DateOnly(pcValue);
  finally
    fcDatePicker.ShowCheckbox := lbShowCheckBox;
  end;
  { if the datetimevalue is set to 0 make false and vice versa

    NB AFS 11 Dec 2: with pcValue = 0, fcDateTimePicker.DateTime gets the value of 0.4692...
    so the next comparison must be done with pcValue not  fcDateTimePicker.DateTime
    likewise in SetTime
  }
  HasDateTime := not DateIsNull(pcValue, Kind);;
end;

procedure TCustomDateTimeNullPicker.SetTime(const pcValue: TTime);
begin
  fcTimePicker.Time := TimeOnly(pcValue);
  {if the datetimevalue is set to 0 make false and vice versa}
  HasDateTime := not DateIsNull(pcValue, Kind);;
end;

function TCustomDateTimeNullPicker.GetDateFormat: TDTDateFormat;
begin
  Result := fcDatePicker.DateFormat;
end;

procedure TCustomDateTimeNullPicker.SetDateFormat(const pcValue: TDTDateFormat);
begin
  fcDatePicker.DateFormat := pcValue;
  fcTimePicker.DateFormat := pcValue;
end;

function TCustomDateTimeNullPicker.GetMaxDate: TDate;
begin
  Result := fcDatePicker.MaxDate;
end;

function TCustomDateTimeNullPicker.GetMinDate: TDate;
begin
  Result := fcDatePicker.MinDate;
end;

procedure TCustomDateTimeNullPicker.SetMaxDate(const pcValue: TDate);
begin
  fcDatePicker.MaxDate := DateOnly(pcValue);
end;

procedure TCustomDateTimeNullPicker.SetMinDate(const pcValue: TDate);
begin
  fcDatePicker.MinDate := DateOnly(pcValue);
end;


function TCustomDateTimeNullPicker.GetOnCloseUp: TNotifyEvent;
begin
  Result := fcDatePicker.OnCloseUp;
end;

procedure TCustomDateTimeNullPicker.SetOnCloseUp(const pcValue: TNotifyEvent);
begin
  fcDatePicker.OnCloseUp := pcValue;
end;

function TCustomDateTimeNullPicker.GetOnDropDown: TNotifyEvent;
begin
  Result := fcDatePicker.OnDropDown;
end;

procedure TCustomDateTimeNullPicker.SetOnDropDown(const pcValue: TNotifyEvent);
begin
  fcDatePicker.OnDropDown := pcValue;
end;

procedure TCustomDateTimeNullPicker.OnEncapsChange(Sender: TObject);
begin
  if Sender = fccheckbox then
    CheckboxClick(Sender);

  if Assigned(fOnChange) then
    FOnChange(Sender);
end;

procedure TCustomDateTimeNullPicker.OnEncapsClick(Sender: TObject);
begin
  if Assigned(OnClick) then
    onclick(Sender);
end;

procedure TCustomDateTimeNullPicker.OnEncapsDblClick(Sender: TObject);
begin
  if Assigned(OnDblClick) then
    onDblClick(Sender);
end;

procedure TCustomDateTimeNullPicker.OnEncapsDragDrop(Sender, Source: TObject; X, Y: integer);
begin
  if Assigned(OnDragDrop) then
    onDragDrop(Sender, Source, X, Y);
end;

procedure TCustomDateTimeNullPicker.OnEncapsDragOver(Sender, Source: TObject; X,
  Y: integer; State: TDragState; var Accept: boolean);
begin
  if Assigned(OnDragOver) then
    onDragOver(Sender, Source, X, Y, State, Accept);
end;

procedure TCustomDateTimeNullPicker.OnEncapsEndDrag(Sender, Target: TObject; X, Y: integer);
begin
  if Assigned(OnEndDrag) then
    onEndDrag(Sender, Target, X, Y);
end;

procedure TCustomDateTimeNullPicker.OnEncapsEnter(Sender: TObject);
begin
  if Assigned(OnEnter) then
    onEnter(Sender);
end;

procedure TCustomDateTimeNullPicker.OnEncapsExit(Sender: TObject);
begin
  if Assigned(OnExit) then
    onExit(Sender);
end;

procedure TCustomDateTimeNullPicker.OnEncapsKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Assigned(OnKeyDown) then
    onKeyDown(Sender, Key, Shift);
end;

procedure TCustomDateTimeNullPicker.OnEncapsKeyPress(Sender: TObject; var Key: char);
begin
  if Assigned(OnKeyPress) then
    onKeyPress(Sender, Key);
end;

procedure TCustomDateTimeNullPicker.OnEncapsKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Assigned(OnKeyUp) then
    onKeyUp(Sender, Key, Shift);
end;

procedure TCustomDateTimeNullPicker.OnEncapsStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  if Assigned(OnStartDrag) then
    onStartDrag(Sender, DragObject);
end;

function TCustomDateTimeNullPicker.GetDroppedDown: boolean;
begin
  Result := fcDatePicker.DroppedDown;
end;

function TCustomDateTimeNullPicker.GetCaption: string;
begin
  Result := fclblCaption.Caption;
end;

procedure TCustomDateTimeNullPicker.SetCaption(const psValue: string);
begin
  fclblCaption.Caption := psValue;
  Resize;
end;

{ protected }
procedure TCustomDateTimeNullPicker.Resize;
const
  MIN_WIDTH = 100;
  GUI_PAD = 2;
var
  liLeft: Integer;
begin
  inherited;

  if ClientWidth < MIN_WIDTH then
    ClientWidth := MIN_WIDTH;
  fcDatePicker.Visible := (feKind in [dtkDateOnly, dtkDateTime]) and HasDateTime;
  fcTimePicker.Visible := (feKind in [dtkTimeOnly, dtkDateTime]) and HasDateTime;

  liLeft := GUI_PAD;
  fcCheckBox.Left   := GUI_PAD;
  fcCheckBox.Top    := GUI_PAD;
  fcCheckBox.Width  := 20 ;
  fccheckbox.Height := ClientHeight - (2 * GUI_PAD);
  if not fcCheckBox.Visible then
    fcCheckBox.Left := 0-((2 * GUI_PAD) + fcCheckBox.Width);
  if fcCheckBox.Visible then
    liLeft := ToRightOf(fcCheckBox, GUI_PAD);
  fclblCaption.Left := liLeft;
  fclblCaption.Height := Canvas.TextHeight(fclblCaption.Caption);
  fclblCaption.Width := Canvas.TextWidth(fclblCaption.Caption);

  fcDatePicker.Height := ClientHeight - GUI_PAD;
  fcTimePicker.Height := ClientHeight - GUI_PAD;
  if fclblCaption.Visible then
    liLeft := ToRightOf(fclblCaption, GUI_PAD);
  if fclblCaption.Visible then
  begin
    case feKind of
      dtkDateOnly:
      begin
        fcTimePicker.Left := 0 - (fcTimePicker.Width+ 10);
        fcDatePicker.Left := liLeft;
        fcDatePicker.Width := ((ClientWidth - ToRightOf(fclblCaption)))- GUI_PAD;
      end;
      dtkDateTime:
      begin
        fcDatePicker.Left := liLeft;
        fcDatePicker.Width := ((ClientWidth - ToRightOf(fclblCaption)) div 2 )- GUI_PAD;
        fcTimePicker.Left := ToRightOf(fcDatePicker, GUI_PAD);
        fcTimePicker.Width := fcDatePicker.Width;
      end;
      dtkTimeOnly:
      begin
        fcDatePicker.Left := 0 - (fcDatePicker.Width+ 10);
        fcTimePicker.Left := liLeft;
        fcTimePicker.Width := ((ClientWidth - ToRightOf(fclblCaption)))- GUI_PAD;
      end;
    end;
  end
  else
  begin
    case feKind of
      dtkDateOnly:
      begin
        fcTimePicker.Left := 0 - (fcTimePicker.Width+ 10);
        fcDatePicker.Left := liLeft;
        fcDatePicker.Width  := ((ClientWidth - ToRightOf(fcCheckBox)))- GUI_PAD;
      end;
      dtkDateTime:
      begin
        fcDatePicker.Left := liLeft;//ToRightOf(fcCheckBox);
        fcDatePicker.Width  := ((ClientWidth - ToRightOf(fcCheckBox)) div 2 )- GUI_PAD;

        fcTimePicker.Left := ToRightOf(fcDatePicker, GUI_PAD);
        fcTimePicker.Width := fcDatePicker.Width;
      end;
      dtkTimeOnly:
      begin
        fcDatePicker.Left := 0 - (fcDatePicker.Width+ 10);
        fcTimePicker.Left := liLeft;
        fcTimePicker.Width  := ((ClientWidth - ToRightOf(fcCheckBox)))- GUI_PAD;
      end;
    end;
  end;
  fcCheckBox.Color := Color;
  CenterHeight(fclblCaption, fcCheckBox);
  CenterHeight(fcDatePicker, fcCheckBox);
  CenterHeight(fcTimePicker, fcCheckBox);
end;


procedure TCustomDateTimeNullPicker.SetEnabled(pbValue: Boolean);
begin
  inherited;
  //!!EnableControlSetColour(fcDatePicker, pbvalue);
  //!!EnableControlSetColour(fcTimePicker, pbvalue);
  //!!EnableControlSetColour(fccheckbox, pbvalue);

 { fcDatePicker.Enabled := pbValue;
  fcTimePicker.Enabled := pbValue;
  fccheckbox.Enabled := pbValue;}
  fclblCaption.Enabled := pbValue;
end;

procedure TCustomDateTimeNullPicker.SetKind(const peValue: TdtKind);
begin
  feKind := peValue;
  Resize;
end;

procedure TCustomDateTimeNullPicker.Loaded;
begin
  inherited;
  Resize;
end;

procedure TCustomDateTimeNullPicker.SetHideCheckbox(const pbValue: Boolean);
begin
  fbHideCheckbox := pbValue;
  fcCheckBox.Visible := not pbValue;
  Resize;
end;

function TCustomDateTimeNullPicker.GetEnabled: Boolean;
begin
  Result := inherited GetEnabled;
end;

function TCustomDateTimeNullPicker.GetColor: TColor;
begin
  //seems dodgy, but it works
  Result := inherited Color;
end;

function TCustomDateTimeNullPicker.IsColorStored: Boolean;
begin
  //seems dodgy, but it works
  Result := not ParentColor;
end;

procedure TCustomDateTimeNullPicker.SetColor(const peValue: TColor);
begin
  //seems dodgy, but it works
  inherited Color := peValue;
  fcCheckBox.Color := peValue;
end;

end.
