{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvProgressDialog.PAS, released on 2003-03-31.

The Initial Developer of the Original Code is Peter Thörnqvist.
Portions created by Peter Thörnqvist are Copyright (c) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-03-31

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
- OnCancel is executed before the progress dialog is closed.

Description:
A configurable progress dialog with optional image, optional cancel button,
label and progressbar. Driven by a timer and an event on modal mode and by any
user loop in non-modal mode.

How it works:
This component is driven by the Interval property and the OnProgress event. You use Interval to set
the time interval between calls to OnProgress and you use OnProgress to update the
display of the dialog. You can set the properties of the component within OnProgress to
change the display except for Transparent. Since this component relies on the fact that you have
a class that can handle the OnProgress event, there are times when you can't use it easily
(f ex displaying a progress dialog on program startup). Try TJvProgressForm instead since it
has methods and properties you can call directly (ProgressStepIt is the most important) to update the dialog.

Methods:
Execute:boolean - shows the dialog modally and returns true if the user did not cancel it. The user
                  can cancel the dialog only when the Cancel button is visible by clicking it or hitting Esc key
ShowModal:integer - same as Execute but returns mrCancel on cancel and mrOK otherwise
Show              - shows the dialog non-modally
Hide              - hides (closes) the dialog if it is visible

Properties:
Cancelled  - returns true if the user has cancelled the dialog. Only used in non-modal mode
Min - min value of progressbar at startup
Max - max value of progressbar at startup
Position - position of progressbar at startup
Interval - the number of miliseconds between calls to OnProgress. If Interval <= 0,
           OnProgress is called once. If Interval is still <= 0, the dialog is closed.
Caption - caption of dialog. If caption is empty, the entire caption area is removed
Text   - text of label above progressbar
ShowCancel - shows or hides the Cancel button. Note that the Cancel button is always visible when previewing in design mode:
             you wouldn't be able to close the dialog otherwise...
Image - (optional) image to display in dialog. The image can be any size as the dialog
        auto-adjusts to it's size but you should keep them fairly small (say, less than 160x100 something)
        as large images doesn't look too good (IMO)
Transparent - set to true if Image should be rendered transparently (this value cannot be changed in OnProgress)


Events:
OnProgress:TJvProgressDialogEvent = procedure(Sender: TObject; var AContinue: boolean) of object;
  - called every Interval so you can update the values of the component.
    To change the values in the dialog, assign new values to the properties of the component.
    You can change the values Min, Max, Position, Interval, Image, Caption and Text.
    Set AContinue to false to close the dialog.
OnShow:TNotifyEvent = procedure(Sender:TObject) of object;
  - called just before the dialog is shown on screen
OnClose:TNotifyEvent = procedure(Sender:TObject) of object;
  - called just after the dialog is closed
OnCancel:TNotifyEvent = procedure(Sender:TObject) of object;
  - called if the user clicks the Cancel button. NB that this event is called *before* the dialog is closed

NB!
 During execution of the dialog, the component properties reflects the
 *current* values in the dialog (as changed in OnProgress). After execution, the
 properties are reset to their original ("start") values.

-----------------------------------------------------------------------------}

{$I JVCL.INC}
unit JvProgressDialog;

interface
uses
  Classes, SysUtils, Graphics, Forms, JvBaseDlg;

type
  TJvProgressDialogEvent = procedure(Sender: TObject; var AContinue: boolean) of object;
  TJvProgressDialog = class(TJvCommonDialogF)
  private
    FIMin, FIMax, FIPosition, FIInterval: integer;
    FICaption, FIText: string;
    FIImage: TPicture;

    FMin, FMax, FPosition, FInterval: integer;
    FCaption, FText: string;
    FShowCancel: boolean;
    FTransparent: boolean;
    FForm: TForm;
    FOnProgress: TJvProgressDialogEvent;
    FOnCancel: TNotifyEvent;
    FImage: TPicture;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FCancelled: boolean;
    procedure SetPicture(const Value: TPicture);
    procedure SetCaption(const Value: string);
    procedure SetInterval(const Value: integer);
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure SetShowCancel(const Value: boolean);
    procedure SetText(const Value: string);
  protected
    procedure InternalDoClose(Sender: TObject; var Action: TCloseAction);
    procedure InternalDoProgress(Sender: TObject; var AMin, AMax, APosition,
      AInterval: integer; var ACaption, ALabel: string; AnImage: TPicture;
      var AContinue: boolean);
    procedure InternalDoCancel(Sender: TObject);
    procedure DoShow;
    procedure DoClose;
    procedure StoreValues;
    procedure RestoreValues;
  public
    function Execute: Boolean; override;
    function ShowModal: integer;
    // (p3) Show, Hide and Cancelled are used in non-modal mode)
    procedure Show;
    procedure Hide;
    property Cancelled:boolean read FCancelled;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Caption: string read FCaption write SetCaption;
    property Image: TPicture read FImage write SetPicture;
    property Interval: integer read FInterval write SetInterval default 200;
    property Min: integer read FMin write SetMin default 0;
    property Max: integer read FMax write SetMax default 100;
    property Position: integer read FPosition write SetPosition default 0;
    property ShowCancel: boolean read FShowCancel write SetShowCancel default true;
    property Text: string read FText write SetText;
    property Transparent: boolean read FTransparent write FTransparent default false;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnProgress: TJvProgressDialogEvent read FOnProgress write FOnProgress;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

implementation
uses
  Controls, JvProgressFrm;

{ TJvProgressDialog }

constructor TJvProgressDialog.Create(AOwner: TComponent);
begin
  inherited;
  FImage := TPicture.Create;
  FMin := 0;
  FMax := 100;
  FPosition := 0;
  FInterval := 200;
  FTransparent := false;
end;

destructor TJvProgressDialog.Destroy;
begin
  FreeAndNil(FImage);
  FreeAndNil(FIImage);
  inherited;
end;

procedure TJvProgressDialog.InternalDoCancel(Sender: TObject);
begin
  if Assigned(FOnCancel) then
    FOnCancel(self);
  FCancelled := true;
end;

procedure TJvProgressDialog.InternalDoProgress(Sender: TObject; var AMin, AMax,
  APosition, AInterval: integer; var ACaption, ALabel: string;
  AnImage: TPicture; var AContinue: boolean);
begin
  if Assigned(FOnProgress) then
  begin
    // set new values
    Image := AnImage;
    Min := AMin;
    Max := AMax;
    Interval := AInterval;
    Position := APosition;
    Caption := ACaption;
    Text := ALabel;
    // this is were the user gets a chance to alter any property values
    FOnProgress(self, AContinue);
    // send back new values
    if AnImage <> nil then
      AnImage.Assign(Image);
    AMin := Min;
    AMax := Max;
    AInterval := Interval;
    APosition := Position;
    ACaption := Caption;
    ALabel := Text;
  end;
end;

function TJvProgressDialog.Execute: Boolean;
begin
  Result := ShowModal <> mrCancel;
end;

procedure TJvProgressDialog.SetPicture(const Value: TPicture);
begin
  FImage.Assign(Value);
end;

procedure TJvProgressDialog.DoClose;
begin
  if Assigned(FOnClose) then FOnClose(self);
end;

procedure TJvProgressDialog.DoShow;
begin
  if Assigned(FOnShow) then FOnShow(self);
end;

function TJvProgressDialog.ShowModal: integer;
begin
  Result := mrCancel;
  FCancelled := false;
  FreeAndNil(FForm);
  DoShow;
  StoreValues;
  try
    if TfrmProgress.Execute(TfrmProgress(FForm), Caption, Text, Image, Transparent, Min, Max, Position, Interval,
      ShowCancel or (csDesigning in ComponentState), InternalDoProgress, InternalDoCancel) then
        Result := mrOK;
  finally
    RestoreValues;
    DoClose;
  end;
end;

procedure TJvProgressDialog.Hide;
begin
  if FForm <> nil then
    FForm.Close;
end;

procedure TJvProgressDialog.Show;
begin
  FreeAndNil(FForm);
  FForm := TfrmProgress.Create(Application);
  FForm.OnClose := InternalDoClose;
  FCancelled := false;
  DoShow;
  StoreValues;
  TfrmProgress.Execute(TfrmProgress(FForm), Caption, Text, Image, Transparent, Min, Max, Position, Interval, ShowCancel,
    InternalDoProgress, InternalDoCancel);
end;

procedure TJvProgressDialog.InternalDoClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  FForm := nil;
  RestoreValues;
  DoClose;
end;

procedure TJvProgressDialog.SetCaption(const Value: string);
begin
  if FForm <> nil then
  begin
    TfrmProgress(FForm).Caption := Value;
    TfrmProgress(FForm).Update;
  end;
  FCaption := Value;
end;

procedure TJvProgressDialog.SetInterval(const Value: integer);
begin
  if FForm <> nil then
  begin
    TfrmProgress(FForm).tmProgress.Interval := Value;
    TfrmProgress(FForm).Update;
  end;
  FInterval := Value;
end;

procedure TJvProgressDialog.SetMax(const Value: integer);
begin
  if FForm <> nil then
  begin
    TfrmProgress(FForm).pbProgress.Max := Value;
    TfrmProgress(FForm).Update;
  end;
  FMax := Value;
end;

procedure TJvProgressDialog.SetMin(const Value: integer);
begin
  if FForm <> nil then
  begin
    TfrmProgress(FForm).pbProgress.Min := Value;
    TfrmProgress(FForm).Update;
  end;
  FMin := Value;
end;

procedure TJvProgressDialog.SetPosition(const Value: integer);
begin
  if FForm <> nil then
  begin
    TfrmProgress(FForm).pbProgress.Position := Value;
    TfrmProgress(FForm).Update;
  end;
  FPosition := Value;
end;

procedure TJvProgressDialog.SetShowCancel(const Value: boolean);
begin
  if FForm <> nil then
  begin
    TfrmProgress(FForm).btnCancel.Visible := Value;
    TfrmProgress(FForm).Update;
  end;
  FShowCancel := Value;
end;

procedure TJvProgressDialog.SetText(const Value: string);
begin
  if FForm <> nil then
  begin
    TfrmProgress(FForm).Label1.Caption := Value;
    TfrmProgress(FForm).Update;
  end;
  FText := Value;
end;

procedure TJvProgressDialog.RestoreValues;
begin
  // reset values to original values
  Image := FIImage;
  Min := FIMin;
  Max := FIMax;
  Interval := FIInterval;
  Position := FIPosition;
  Caption := FICaption;
  Text := FIText;
  FreeAndNil(FIImage);
end;

procedure TJvProgressDialog.StoreValues;
begin
  // store original values
  FreeAndNil(FIImage);
  FIImage := TPicture.Create;
  FIImage.Assign(Image);
  FIMin := Min;
  FIMax := Max;
  FIInterval := Interval;
  FIPosition := Position;
  FICaption := Caption;
  FIText := Text;
end;

end.

