{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTimeEdit.pas, released on 2003-09-24.

The Initial Developer of the Original Code is André Snepvangers [asn@xs4all.nl]
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQTimeEdit;

interface

uses
  SysUtils, Classes, DateUtils,
  Types, QButtons, QControls, QStdCtrls, QExtCtrls, QGraphics, QMask, Qt,
  JvQSpin, JvQJCLUtils, JvQExMask;

type

  TTimeField = ( teHours, teMinutes, teSeconds);


  TJvTimeSpin = class(TJvCustomSpinEdit)
  private
    function getTime : TTime;
    procedure setTime(value :TTime);
    procedure SetSelection(value: TTimeField);
    function GetTimeField : TTimeField;
    function GetDelta(value: TTimeField) : TDateTime;
  protected
    function GetValue: Extended; override;
    procedure SetValue(NewValue: Extended); override;
    procedure UpClick(Sender: TObject); override;
    procedure DownClick(Sender: TObject); override;
    procedure DoEnter; override;
  public
    Constructor Create( AOwner: TComponent); override;
  published
    { Published declarations }
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
//    property FlatButtons;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Time :TTime read getTime write setTime;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TDateFormat = (dfShort, dfLong);

  TJvDateSpin = class(TJvCustomSpinEdit)
  private
    FDateFormat: TDateFormat;
    FDate: TDate;
//  function getDate : TDate;
    procedure setDate(value :TDate);
  protected
    procedure IncrementValue ; //override;
    procedure DecrementValue ; //override;
  public
    Constructor Create( AOwner: TComponent); override;
  published
    { Published declarations }
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
//    property FlatButtons;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Date :TDate read FDate write setDate;
    property DateFormat :TDateFormat read FDateFormat write FDateFormat;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation


Constructor TJvTimeSpin.create(AOwner: TComponent);
begin
  inherited;
  ValueType := vtFloat;
  EditText := FormatDateTime(LongTimeFormat, 0.5);;
  EditMask := '!90:00:00;1;';
  MinValue := 0.0;
  MaxValue := 1.0;
  AutoSelect := false;
  ButtonKind := bkClassic;
end;

procedure TJvTimeSpin.setTime(value :TTime);
begin
  EditText := FormatDateTime(LongTimeFormat, value);
end;

function TJvTimeSpin.getTime: TTime;
begin
  if (parent <> nil) and (Text <> '') then
    result := StrToTime( EditText )
  else
    Result := 0.5 ;
end;

function TJvTimeSpin.GetValue: Extended;
begin
  result := getTime;
end;

procedure TJvTimeSpin.SetValue(NewValue: Extended);
begin
  setTime(NewValue);
end;

procedure TJvTimeSpin.UpClick(Sender: TObject) ;
var
  tf : TTimeField;
  cp : integer;
begin
  tf := GetTimeField;
  Increment := GetDelta(tf);
  cp := CursorPos;
  inherited;
  CursorPos := cp;
  SetSelection(tf);
end;

procedure TJvTimeSpin.DownClick(Sender: TObject);
var
  tf : TTimeField;
  cp : integer;
begin
  tf := GetTimeField;
  Increment := GetDelta(tf);
  cp := CursorPos;
  inherited;
  CursorPos := cp;
  SetSelection(tf);
end;

function TJvTimeSpin.GetTimeField : TTimeField;
begin
  if CursorPos > 5
  then
    Result := teSeconds    // 1 second
  else if CursorPos > 2
  then
    Result := teMinutes   // 1 minute
  else
    Result := teHours   // 1 hour
end;

function TJvTimeSpin.GetDelta(value: TTimeField) : TDateTime;
begin
  case value of
  teSeconds : Result := EncodeTime(0,0,1,0);
  teMinutes : Result := EncodeTime(0,1,0,0);
  teHours : Result := EncodeTime(1,0,0,0)
  else
    Result := EncodeTime(1,0,0,0)
  end;
  Increment := Result;
end;

procedure TJvTimeSpin.DoEnter;
begin
//  inherited DoEnter;
  SetSelection(GetTimeField);
end;


procedure TJvTimeSpin.SetSelection(value: TTimeField);
begin
//  SetFocus;
  case Value of
  teHours :
  begin
    SetSel(0,2);
    CursorPos := 2;
  end;
  teMinutes :
  begin
    SetSel(3,5);
    CursorPos := 5;
  end;
  teSeconds :
  begin
    SetSel(6,8);
    CursorPos := 8;
  end;
  end;
end;

Constructor TJvDateSpin.create( AOwner: TComponent);
begin
  inherited;
  EditMask := '';
  SetDate(now);
  ReadOnly := true;
end;

procedure TJvDateSpin.setDate(value :TDate);
var
  tf: string;
begin
  if DateFormat = dfShort
  then
    tf := ShortDateFormat
  else
    tf := LongDateFormat;
  EditText := FormatDateTime(tf, value);
  FDate := value;
end;

procedure TJvDateSpin.IncrementValue ;
begin
  Date := IncDay(Date, 1);
end;

procedure TJvDateSpin.DecrementValue ;
begin
  Date := IncDay(Date, -1 );
end;


end.
