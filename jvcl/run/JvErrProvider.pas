{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvErrProvider.pas, released on 2002-11-16.

The Initial Developer of the Original Code is Peter Thörnqvist <peter3@peter3.com>.
Portions created by Joe Doe are Copyright (C) 2002 Peter Thörnqvist . All Rights Reserved.

Contributor(s):

Last Modified: 2002-11-16

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
* Setting AutoScroll to true for a form and displaying error icons beyond the form's right
edge can make the form's scrollbars "jump up and down"
* Resizing components while displaying error images, doesn't move the error image smoothly
(this is caused by the image being moved only when the BlinkThread triggers)

Description:
A component patterned on the errorProvider in .NET:
"Provides a user interface for indicating that a control
on a form has an error associated with it."
To set the error, use the Error property: an empty error string, removes the error image

-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvErrProvider;

interface
uses
  Windows, SysUtils, {$IFDEF COMPILER5_UP}ImgList, {$ENDIF}
  Messages, Classes, Controls, Graphics, JvComponent;

type
  IJvErrorProviderClient = interface;

  // IJvErrorProvider is implemented by the TJvErrorProvider
  IJvErrorProvider = interface
    ['{5BCB5404-9C17-4CC6-96EC-46567CA19A12}']
    procedure SetError(AControl:TControl;const AErrorMessage:WideString);
    procedure SetClientError(const AClient: IJvErrorProviderClient);
  end;

  // IJvErrorProviderClient should be implemented by controls that wants to be able
  // to update the error provider through it's own properties
  IJvErrorProviderClient = interface
    ['{9871F250-631E-4119-B073-71B28711C9B8}']
    procedure setErrorProvider(const Value: IJvErrorProvider);
    function getErrorProvider: IJvErrorProvider;
    function getControl: TControl;
    procedure setErrorMessage(const Value: WideString);
    function getErrorMessage: WideString;

    property ErrorProvider: IJvErrorProvider read getErrorProvider write setErrorProvider;
    property ErrorMessage: WideString read getErrorMessage write setErrorMessage;
  end;

  TJvErrorBlinkStyle = (ebsAlwaysBlink, ebsBlinkIfDifferentError, ebsNeverBlink);
  TJvErrorImageAlignment = (eiaBottomLeft, eiaBottomRight, eiaMiddleLeft, eiaMiddleRight,
    eiaTopLeft, eiaTopRight);

  TJvErrorControl = class(TGraphicControl)
  private
    FImageList: TCustomImageList;
    FImageIndex: integer;
    FImagePadding: integer;
    FControl: TControl;
    FImageAlignment: TJvErrorImageAlignment;
    FBlinkCount: integer;
    procedure SetError(const Value: string);
    function GetError: string;
    procedure SetImageIndex(const Value: integer);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetControl(const Value: TControl);
  protected
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function CalcBoundsRect: TRect;
    property Imagelist: TCustomImageList read FImageList write SetImageList;
    property ImageIndex: integer read FImageIndex write SetImageIndex;
    property Control: TControl read FControl write SetControl;
    property Error: string read GetError write SetError;
    property BlinkCount: integer read FBlinkCount write FBlinkCount;
    property ImageAlignment: TJvErrorImageAlignment read FImageAlignment write FImageAlignment;
    property ImagePadding: integer read FImagePadding write FImagePadding;

    procedure DrawImage(Erase: boolean);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ShowHint default true;
    property Width default 16;
    property Height default 16;
  end;

  TJvErrorProvider = class(TJvComponent, IUnknown, IJvErrorProvider)
  private
    FUpdateCount: integer;
    FControls: TList;
    FBlinkRate: integer;
    FImageList: TCustomImageList;
    FBlinkThread: TThread;
    FBlinkStyle: TJvErrorBlinkStyle;
    FChangeLink: TChangeLink;
    FImageIndex: integer;
    FDefaultImage:TImageList;
    function GetError(AControl: TControl): string;
    function GetImageAlignment(AControl: TControl): TJvErrorImageAlignment;
    function GetImagePadding(AControl: TControl): integer;
    procedure SetBlinkRate(const Value: integer);
    procedure SetBlinkStyle(const Value: TJvErrorBlinkStyle);
    procedure SetError(AControl: TControl; const Value: string);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetImageAlignment(AControl: TControl; const Value: TJvErrorImageAlignment);
    procedure SetImagePadding(AControl: TControl; const Value: integer);
    procedure SetImageIndex(const Value: integer);
    procedure DoChangeLinkChange(Sender: TObject);
    procedure DoBlink(Sender: TObject; Erase: boolean);
    procedure StopThread;
    procedure StartThread;
    function GetControl(Index: integer): TJvErrorControl;
    function GetCount: integer;
  protected
    { IJvErrorProvider }
    procedure IJvErrorProvider.SetError = ProviderSetError;
    procedure ProviderSetError(AControl:TControl;const ErrorMessage:WideString);
    procedure SetClientError(const AClient: IJvErrorProviderClient);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function IndexOf(AControl: TControl): integer;
    function Add(AControl: TControl): integer;
    procedure UpdateControls;
    procedure Delete(Index: integer);
    property Controls[Index: integer]: TJvErrorControl read GetControl;
    property Count: integer read GetCount;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;

    // Call ClearErrors to remove all error images with one call
    // After a call to ClearErrors, the internal error image list is emptied
    // Calling ClearErrors is the same as setting Error[nil] := '' but is slightly faster
    procedure ClearErrors;
    // The BeginUpdate method suspends the blinking thread until the EndUpdate method is called.
    procedure BeginUpdate;
    // EndUpdate re-enables the blinking thread that was turned off with the BeginUpdate method.
    procedure EndUpdate;
    // Gets or sets the error message associated with a control
    // Setting the error message to an empty string removes the error image
    // (this is the only way to remove an error image for a single control)
    // Use Error[nil] := 'SomeValue'; to assign the error message 'SomeValue' to all controls
    // Using Error[nil] := ''; is equivalent to calling ClearErrors but ClearErrors is faster
    property Error[AControl: TControl]: string read GetError write SetError;
    // Gets or sets a value indicating where the error image should be placed in relation to the control.
    // The location can be further modified by assigning a non-zero value to ImagePadding
    // Possible values:
    //   eiaBottomLeft - display the error image on the controls left side aligned to the bottom edge of the control
    //   eiaBottomRight - display the error image on the controls right side aligned to the bottom edge of the control
    //   eiaMiddleLeft - display the error image on the controls left side aligned to the middle of the control
    //   eiaMiddleRight - display the error image on the controls right side aligned to the middle of the control
    //   eiaTopLeft - display the error image on the controlsleft side aligned to the top edge of the control
    //   eiaTopRight - display the error image on the controls right side aligned to the top edge of the control
    // Use AControl = nil to set the same Alignment for all controls
    property ImageAlignment[AControl: TControl]: TJvErrorImageAlignment read GetImageAlignment write SetImageAlignment;
    // Gets or sets the amount of extra space to leave between the specified control and the error image.
    // Use AControl = nil to set the same padding for all controls.
    property ImagePadding[AControl: TControl]: integer read GetImagePadding write SetImagePadding;
  published
    // The rate at which the error image should flash. The rate is expressed in milliseconds. The default is 250 milliseconds.
    // A value of zero sets BlinkStyle to ebsNeverBlink.
    property BlinkRate: integer read FBlinkRate write SetBlinkRate default 250;
    // The error Image flashes in the manner specified by the assigned BlinkStyle when an error occurs.
    // Possible values:
    //   ebsBlinkIfDifferentError - blink if the new error message differs from the previous
    //   ebsAlwaysBlink - always blink when the error message changes, even if it's the same message
    //   ebsNeverBlink - never bink, just display the error image and the description
    // Setting the BlinkRate to zero sets the BlinkStyle to ebsNeverBlink.
    // The default is ebsBlinkIfDifferentError
    property BlinkStyle: TJvErrorBlinkStyle read FBlinkStyle write SetBlinkStyle default ebsBlinkIfDifferentError;
    // Gets or sets the ImageList where to retrieve an image to display next to a control when an error description
    // string has been set for the control.
    // This property is used in conjunction with ImageIndex to select the image to display
    // If either is nil, invalid or out of range, no error image is displayed
    property ImageList: TCustomImageList read FImagelist write SetImageList;
    // Gets or sets the ImageIndex in ImageList to use when displaying an image next to a control
    property ImageIndex: integer read FImageIndex write SetImageIndex;
  end;

implementation
uses
  CommCtrl;

const
  cDefBlinkCount = 5;
{$R JvErrProvider.res}

type
  TJvBlinkThreadEvent = procedure(Sender: TObject; Erase: boolean) of object;

  TJvBlinkThread = class(TThread)
  private
    FBlinkRate: integer;
    FErase: boolean;
    FOnBlink: TJvBlinkThreadEvent;
    procedure Blink;
  protected
    procedure Execute; override;
  public
    constructor Create(BlinkRate: integer);
    property OnBlink: TJvBlinkThreadEvent read FOnBlink write FOnBlink;
  end;

  { TJvErrorProvider }

function TJvErrorProvider.Add(AControl: TControl): integer;
var ci: TJvErrorControl;
begin
  Result := IndexOf(AControl);
  if (Result < 0) and (AControl <> nil) then
  begin
    ci := TJvErrorControl.Create(self);
    ci.Control := AControl;
    //    ci.Name := ci.Control.Name + '_ErrorControl';
    Result := FControls.Add(ci);
  end;
end;

constructor TJvErrorProvider.Create(AComponent: TComponent);
begin
  inherited;
  FDefaultImage := TImageList.CreateSize(16,16);
  ImageList_AddIcon(FDefaultImage.Handle,
    LoadImage(hInstance,PChar('JVERRORPROVIDERICON') , IMAGE_ICON, 16, 16, 0));
  FBlinkStyle := ebsBlinkIfDifferentError;
  FBlinkRate := 250;
  FControls := TList.Create;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoChangeLinkChange;
end;

procedure TJvErrorProvider.Delete(Index: integer);
begin
  Controls[Index].Free;
  FControls.Delete(Index);
end;

destructor TJvErrorProvider.Destroy;
begin
  StopThread;
  ClearErrors;
  FControls.Free;
  FChangeLink.Free;
  FDefaultImage.Free;
  inherited;
end;

function TJvErrorProvider.GetError(AControl: TControl): string;
var i: integer;
begin
  i := IndexOf(AControl);
  if (i > -1) then
    Result := Controls[i].Error
  else
    raise Exception.Create('Control not found in GetError');
end;

function TJvErrorProvider.GetImageAlignment(
  AControl: TControl): TJvErrorImageAlignment;
var i: integer;
begin
  i := IndexOf(AControl);
  if (i > -1) then
    Result := Controls[i].ImageAlignment
  else
    raise Exception.Create('Control not found in GetImageAlignment');
end;

function TJvErrorProvider.GetImagePadding(AControl: TControl): integer;
var i: integer;
begin
  i := IndexOf(AControl);
  if (i > -1) then
    Result := Controls[i].ImagePadding
  else
    raise Exception.Create('Control not found in GetImagePadding');
end;

function TJvErrorProvider.IndexOf(AControl: TControl): integer;
begin
  if AControl <> nil then
    for Result := 0 to Count - 1 do
      if Controls[Result].Control = AControl then
        Exit;
  Result := -1;
end;

procedure TJvErrorProvider.Notification(AComponent: TComponent;
  Operation: TOperation);
var i: integer;
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent is TControl) then
      i := IndexOf(TControl(AComponent))
    else
      i := -1;
    if i > -1 then
      Delete(i);
    if AComponent = ImageList then
      ImageList := nil;
  end;
end;

procedure TJvErrorProvider.SetBlinkRate(const Value: integer);
begin
  if FBlinkRate <> Value then
  begin
    StopThread;
    FBlinkRate := Value;
    if FBlinkRate <= 0 then
    begin
      FBlinkRate := 0;
      FBlinkStyle := ebsNeverBlink;
    end;
    UpdateControls;
  end;
end;

procedure TJvErrorProvider.SetBlinkStyle(const Value: TJvErrorBlinkStyle);
begin
  if FBlinkStyle <> Value then
  begin
    StopThread;
    FBlinkStyle := Value;
    UpdateControls;
  end;
end;

procedure TJvErrorProvider.SetError(AControl: TControl;
  const Value: string);
var i: integer; ei: TJvErrorControl;
begin
  StopThread;
  if AControl = nil then
  begin
    if Value = '' then
      ClearErrors
    else
      for i := 0 to Count - 1 do
      begin
        ei := Controls[i];
        if ((ei.Error <> Value) and (BlinkStyle = ebsBlinkIfDifferentError)) or (BlinkStyle = ebsAlwaysBlink) then
          ei.BlinkCount := cDefBlinkCount
        else if (BlinkStyle = ebsNeverBlink) then
          ei.BlinkCount := 0;
        ei.Error := Value;
      end;
  end
  else
  begin
    i := Add(AControl);
    if i > -1 then
    begin
      if Value = '' then
        Delete(i)
      else
      begin
        ei := Controls[i];
        if ((ei.Error <> Value) and (BlinkStyle = ebsBlinkIfDifferentError))
          or (BlinkStyle = ebsAlwaysBlink) then
        begin
          ei.Error := Value;
          ei.BlinkCount := cDefBlinkCount;
          ei.Visible := (csDesigning in ComponentState); 
          if (FUpdateCount = 0) and (FBlinkThread = nil) then
            StartThread;
        end
        else if (BlinkStyle = ebsNeverBlink) then
        begin
          ei.BlinkCount := 0;
          ei.Error := Value;
          ei.Visible := (Value <> '');
        end;
      end;
      UpdateControls;
    end
    else
      raise Exception.Create('Unable to add control in SetError');
  end;
end;

procedure TJvErrorProvider.SetImageAlignment(AControl: TControl;
  const Value: TJvErrorImageAlignment);
var i: integer;
begin
  if AControl = nil then
    for i := 0 to Count - 1 do
      Controls[i].ImageAlignment := Value
  else
  begin
    i := Add(AControl);
    if i > -1 then
      Controls[i].ImageAlignment := Value
    else
      raise Exception.Create('Unable to add control in SetImageAlignment');
  end;
end;

procedure TJvErrorProvider.SetImagePadding(AControl: TControl;
  const Value: integer);
var i: integer;
begin
  if AControl = nil then
    for i := 0 to Count - 1 do
      Controls[i].ImagePadding := Value
  else
  begin
    i := Add(AControl);
    if i > 1 then
      Controls[i].ImagePadding := Value
    else
      raise Exception.Create('Unable to add control in SetImagePadding');
  end;
end;

procedure TJvErrorProvider.UpdateControls;
var i,ii: integer;IL:TCustomImageList;
begin
  if ImageList <> nil then
  begin
    IL := ImageList;
    ii := ImageIndex;
  end
  else
  begin
    IL := FDefaultImage;
    ii := 0;
  end;
  for i := 0 to Count - 1 do
  begin
    Controls[i].ImageList := IL;
    Controls[i].ImageIndex := ii;
  end;
end;

procedure TJvErrorProvider.SetImageList(const Value: TCustomImageList);
begin
  if FImageList <> Value then
  begin
    StopThread;
    if Assigned(FImageList) then
    begin
      FImageList.UnRegisterChanges(FChangeLink);
      FImageList.RemoveFreeNotification(self);
    end;
    FImageList := Value;
    if Assigned(FImageList) then
    begin
      FImageList.RegisterChanges(FChangeLink);
      FImageList.FreeNotification(self);
    end;
    UpdateControls;
  end;
end;

procedure TJvErrorProvider.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then
  begin
    StopThread;
    FImageIndex := Value;
    UpdateControls;
  end;
end;

procedure TJvErrorProvider.DoChangeLinkChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TJvErrorProvider.ClearErrors;
var i: integer;
begin
  StopThread;
  for i := 0 to Count - 1 do
    Controls[i].Free;
  FControls.Clear;
end;

procedure TJvErrorProvider.BeginUpdate;
var i: integer;
begin
  Inc(FUpdateCount);
  StopThread;
  for i := 0 to Count - 1 do
    Controls[i].Visible := false;
end;

procedure TJvErrorProvider.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) then
    begin
      UpdateControls;
      StartThread;
    end;
  end;
end;

procedure TJvErrorProvider.StartThread;
begin
  FBlinkThread := TJvBlinkThread.Create(BlinkRate);
  TJvBlinkThread(FBlinkThread).OnBlink := DoBlink;
  TJvBlinkThread(FBlinkThread).Resume;
end;

procedure TJvErrorProvider.StopThread;
begin
  if FBlinkThread <> nil then
    FBlinkThread.Terminate;
  FreeAndNil(FBlinkThread);
end;

procedure TJvErrorProvider.DoBlink(Sender: TObject; Erase: boolean);
var i: integer;
begin
  for i := 0 to Count - 1 do
    Controls[i].DrawImage(Erase);
end;

function TJvErrorProvider.GetControl(Index: integer): TJvErrorControl;
begin
  Result := TJvErrorControl(FControls[Index]);
end;

function TJvErrorProvider.GetCount: integer;
begin
  Result := FControls.Count;
end;

procedure TJvErrorProvider.SetClientError(const AClient: IJvErrorProviderClient);
begin
  if AClient <> nil then
    SetError(AClient.getControl, AClient.ErrorMessage);
end;

procedure TJvErrorProvider.ProviderSetError(AControl: TControl;
  const ErrorMessage: WideString);
begin
  SetError(AControl,ErrorMessage);
end;

{ TJvErrorControl }

constructor TJvErrorControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageAlignment := eiaMiddleRight;
  ShowHint := true;
  Visible := false;
  Width := 16;
  Height := 16;
end;

destructor TJvErrorControl.Destroy;
begin
  Control := nil;
  inherited;
end;

procedure TJvErrorControl.DrawImage(Erase: boolean);
begin
  if not Assigned(Control) or not Assigned(Control.Parent) or not Assigned(Imagelist) then
    Exit;
  Visible := (Error <> '') and (not Erase or (BlinkCount < 2));
  if not Visible and (BlinkCount > 1) then
    Dec(FBlinkCount);
  if Visible then
    BoundsRect := CalcBoundsRect;
end;

function TJvErrorControl.CalcBoundsRect: TRect;
begin
  if (Control = nil) or (ImageList = nil) then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    case ImageAlignment of
      eiaBottomLeft:
        begin
          // must qualify Result fully since Delphi confuses the TRect with the controls Top/Left properties
          Result.Right := Control.Left - 1;
          Result.Left := Result.Right - ImageList.Width;
          Result.Bottom := Control.Top + Control.Height;
          Result.Top := Result.Bottom - ImageList.Height;
          OffsetRect(Result, -ImagePadding, 0);
        end;
      eiaBottomRight:
        begin
          Result.Left := Control.Left + Control.Width + 1;
          Result.Right := Result.Left + ImageList.Width;
          Result.Bottom := Control.Top + Control.Height;
          Result.Top := Result.Bottom - ImageList.Height;
          OffsetRect(Result, ImagePadding, 0);
        end;
      eiaMiddleLeft:
        begin
          Result.Right := Control.Left - 1;
          Result.Left := Result.Right - ImageList.Width;
          Result.Top := Control.Top + (Control.Height - ImageList.Height) div 2;
          Result.Bottom := Result.Top + ImageList.Height;
          OffsetRect(Result, -ImagePadding, 0);
        end;
      eiaMiddleRight:
        begin
          Result.Left := Control.Left + Control.Width + 1;
          Result.Right := Result.Left + ImageList.Width;
          Result.Top := Control.Top + (Control.Height - ImageList.Height) div 2;
          Result.Bottom := Result.Top + ImageList.Height;
          OffsetRect(Result, ImagePadding, 0);
        end;
      eiaTopLeft:
        begin
          Result.Right := Control.Left - 1;
          Result.Left := Result.Right - ImageList.Width;
          Result.Top := Control.Top;
          Result.Bottom := Result.Top + Control.Height;
          OffsetRect(Result, -ImagePadding, 0);
        end;
      eiaTopRight:
        begin
          Result.Left := Control.Left + Control.Width + 1;
          Result.Right := Result.Left + ImageList.Width;
          Result.Top := Control.Top;
          Result.Bottom := Result.Top + ImageList.Height;
          OffsetRect(Result, ImagePadding, 0);
        end;
    end;
  end;
end;

procedure TJvErrorControl.Paint;
begin
  //  inherited;
  if (Imagelist <> nil) and Visible then
{$IFDEF COMPILER6_UP}
    ImageList.Draw(Canvas, 0, 0, ImageIndex, dsTransparent, itImage);
{$ELSE}
    ImageList.Draw(Canvas, 0, 0, ImageIndex);
{$ENDIF}
end;

procedure TJvErrorControl.SetError(const Value: string);
begin
  Hint := Value;
end;

function TJvErrorControl.GetError: string;
begin
  Result := Hint;
end;

procedure TJvErrorControl.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TJvErrorControl.SetImageList(const Value: TCustomImageList);
begin
  if FImageList <> Value then
  begin
    FImageList := Value;
    if FImageList <> nil then
      BoundsRect := CalcBoundsRect
    else
      SetBounds(Left, Top, 16, 16);
    //    Invalidate;
  end;
end;

procedure TJvErrorControl.SetControl(const Value: TControl);
begin
  if FControl <> Value then
  begin
    if FControl <> nil then
      FControl.RemoveFreeNotification(self);
    FControl := Value;
    if FControl <> nil then
    begin
      FControl.FreeNotification(self);
      Parent := FControl.Parent;
    end
    else
      Parent := nil;
  end;
end;

procedure TJvErrorControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Control) then
    Control := nil;
end;

{ TJvBlinkThread }

constructor TJvBlinkThread.Create(BlinkRate: integer);
begin
  inherited Create(true);
  FBlinkRate := BlinkRate;
  FErase := false;
end;

procedure TJvBlinkThread.Blink;
begin
  if Assigned(FOnBlink) then
    FOnBlink(self, FErase);
end;

procedure TJvBlinkThread.Execute;
begin
  FErase := false;
  while not Terminated and not Suspended do
  begin
    sleep(FBlinkRate);
    Synchronize(Blink);
    if FBlinkRate = 0 then
      Exit;
    FErase := not FErase;
  end;
end;

end.

