{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvErrorIndicator.pas, released on 2002-11-16.

The Initial Developer of the Original Code is Peter Thörnqvist <peter3 at sourceforge dot net>.
Portions created by Joe Doe are Copyright (C) 2002 Peter Thörnqvist . All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
* Setting AutoScroll to True for a form and displaying error icons beyond the form's right
edge can make the form's scrollbars "jump up and down"
* Resizing components while displaying error images, doesn't move the error image smoothly
(this is caused by the image being moved only when the BlinkThread triggers)

Description:
A component patterned on the ErrorProvider in .NET:
"Provides a user interface for indicating that a control
on a form has an error associated with it."
To set the error, use the Error property: an empty error string, removes the error image

-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvErrorIndicator;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, ImgList, Controls, Graphics,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QImgList, QControls, QGraphics, Types,
  {$ENDIF VisualCLX}
  JvComponent;

type
  IJvErrorIndicatorClient = interface;

  // IJvErrorIndicator is implemented by the TJvErrorIndicator
  IJvErrorIndicator = interface
    ['{5BCB5404-9C17-4CC6-96EC-46567CA19A12}']
    procedure SetError(AControl: TControl; const AErrorMessage: WideString);
    procedure SetClientError(const AClient: IJvErrorIndicatorClient);
  end;

  // IJvErrorIndicatorClient should be implemented by controls that wants to be able
  // to update the error indicator through it's own properties
  IJvErrorIndicatorClient = interface
    ['{9871F250-631E-4119-B073-71B28711C9B8}']
    procedure SetErrorIndicator(const Value: IJvErrorIndicator);
    function GetErrorIndicator: IJvErrorIndicator;
    function GetControl: TControl;
    procedure SetErrorMessage(const Value: WideString);
    function GetErrorMessage: WideString;

    property ErrorIndicator: IJvErrorIndicator read GetErrorIndicator write SetErrorIndicator;
    property ErrorMessage: WideString read GetErrorMessage write SetErrorMessage;
  end;

  TJvErrorBlinkStyle = (ebsAlwaysBlink, ebsBlinkIfDifferentError, ebsNeverBlink);
  TJvErrorImageAlignment = (eiaBottomLeft, eiaBottomRight, eiaMiddleLeft, eiaMiddleRight,
    eiaTopLeft, eiaTopRight);

  TJvErrorControl = class(TGraphicControl)
  private
    FImageList: TCustomImageList;
    FImageIndex: Integer;
    FImagePadding: Integer;
    FControl: TControl;
    FImageAlignment: TJvErrorImageAlignment;
    FBlinkCount: Integer;
    procedure SetError(const Value: string);
    function GetError: string;
    procedure SetImageIndex(const Value: Integer);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetControl(const Value: TControl);
  protected
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function CalcBoundsRect: TRect;
    property Images: TCustomImageList read FImageList write SetImageList;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Control: TControl read FControl write SetControl;
    property Error: string read GetError write SetError;
    property BlinkCount: Integer read FBlinkCount write FBlinkCount;
    property ImageAlignment: TJvErrorImageAlignment read FImageAlignment write FImageAlignment;
    property ImagePadding: Integer read FImagePadding write FImagePadding;

    procedure DrawImage(Erase: Boolean);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ShowHint default True;
    property Width default 16;
    property Height default 16;
  end;

  TJvErrorIndicator = class(TJvComponent, IUnknown, IJvErrorIndicator)
  private
    FUpdateCount: Integer;
    FControls: TList;
    FBlinkRate: Integer;
    FImageList: TCustomImageList;
    FBlinkThread: TThread;
    FBlinkStyle: TJvErrorBlinkStyle;
    FChangeLink: TChangeLink;
    FImageIndex: Integer;
    FDefaultImage: TImageList;
    function GetError(AControl: TControl): string;
    function GetImageAlignment(AControl: TControl): TJvErrorImageAlignment;
    function GetImagePadding(AControl: TControl): Integer;
    procedure SetBlinkRate(const Value: Integer);
    procedure SetBlinkStyle(const Value: TJvErrorBlinkStyle);
    procedure SetError(AControl: TControl; const Value: string);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetImageAlignment(AControl: TControl; const Value: TJvErrorImageAlignment);
    procedure SetImagePadding(AControl: TControl; const Value: Integer);
    procedure SetImageIndex(const Value: Integer);
    procedure DoChangeLinkChange(Sender: TObject);
    procedure DoBlink(Sender: TObject; Erase: Boolean);
    procedure StopThread;
    procedure StartThread;
    function GetControl(Index: Integer): TJvErrorControl;
    function GetCount: Integer;
  protected
    { IJvErrorIndicator }
    procedure IJvErrorIndicator.SetError = IndicatorSetError;
    procedure IndicatorSetError(AControl: TControl; const ErrorMessage: WideString);
    procedure SetClientError(const AClient: IJvErrorIndicatorClient);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function IndexOf(AControl: TControl): Integer;
    function Add(AControl: TControl): Integer;
    procedure UpdateControls;
    procedure Delete(Index: Integer);
    property Controls[Index: Integer]: TJvErrorControl read GetControl;
    property Count: Integer read GetCount;
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
    property ImagePadding[AControl: TControl]: Integer read GetImagePadding write SetImagePadding;
  published
    // The rate at which the error image should flash. The rate is expressed in milliseconds. The default is 250 milliseconds.
    // A value of zero sets BlinkStyle to ebsNeverBlink.
    property BlinkRate: Integer read FBlinkRate write SetBlinkRate default 250;
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
    property Images: TCustomImageList read FImagelist write SetImageList;
    // Gets or sets the ImageIndex in ImageList to use when displaying an image next to a control
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
  end;

implementation

uses
  {$IFDEF VCL}
  CommCtrl,
  {$ENDIF VCL}
  JvTypes, JvResources;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvErrorIndicator.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvErrorIndicator.res}
{$ENDIF LINUX}

const
  cDefBlinkCount = 5;

type
  TJvBlinkThreadEvent = procedure(Sender: TObject; Erase: Boolean) of object;

  TJvBlinkThread = class(TThread)
  private
    FBlinkRate: Integer;
    FErase: Boolean;
    FOnBlink: TJvBlinkThreadEvent;
    procedure Blink;
  protected
    procedure Execute; override;
  public
    constructor Create(BlinkRate: Integer);
    property OnBlink: TJvBlinkThreadEvent read FOnBlink write FOnBlink;
  end;

//=== TJvErrorIndicator ======================================================

constructor TJvErrorIndicator.Create(AComponent: TComponent);
{$IFDEF VisualCLX}
var
  Ico: TIcon;
{$ENDIF VisualCLX}
begin
  inherited Create(AComponent);
  FDefaultImage := TImageList.CreateSize(16, 16);
  {$IFDEF VCL}
  ImageList_AddIcon(FDefaultImage.Handle,
    LoadImage(hInstance, PChar('TJVERRORINDICATORICON'), IMAGE_ICON, 16, 16, 0));
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Ico := TIcon.Create;
  Ico.LoadFromResourceName(hInstance, 'TJVERRORINDICATORICON');
  FDefaultImage.Assign(Ico);
  Ico.Free;
  {$ENDIF VisualCLX}
  FBlinkStyle := ebsBlinkIfDifferentError;
  FBlinkRate := 250;
  FControls := TList.Create;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoChangeLinkChange;
end;

destructor TJvErrorIndicator.Destroy;
begin
  StopThread;
  ClearErrors;
  FControls.Free;
  FChangeLink.Free;
  FDefaultImage.Free;
  inherited Destroy;
end;

function TJvErrorIndicator.Add(AControl: TControl): Integer;
var
  Ci: TJvErrorControl;
begin
  Result := IndexOf(AControl);
  if (Result < 0) and (AControl <> nil) then
  begin
    Ci := TJvErrorControl.Create(Self);
    Ci.Control := AControl;
    //    Ci.Name := Ci.Control.Name + '_ErrorControl';
    Result := FControls.Add(Ci);
  end;
end;

procedure TJvErrorIndicator.Delete(Index: Integer);
begin
  Controls[Index].Free;
  FControls.Delete(Index);
end;

function TJvErrorIndicator.GetError(AControl: TControl): string;
var
  I: Integer;
begin
  I := IndexOf(AControl);
  if I > -1 then
    Result := Controls[I].Error
  else
    raise EJVCLException.CreateRes(@RsEControlNotFoundInGetError);
end;

function TJvErrorIndicator.GetImageAlignment(AControl: TControl): TJvErrorImageAlignment;
var
  I: Integer;
begin
  I := IndexOf(AControl);
  if I > -1 then
    Result := Controls[I].ImageAlignment
  else
    raise EJVCLException.CreateRes(@RsEControlNotFoundInGetImageAlignment);
end;

function TJvErrorIndicator.GetImagePadding(AControl: TControl): Integer;
var
  I: Integer;
begin
  I := IndexOf(AControl);
  if I > -1 then
    Result := Controls[I].ImagePadding
  else
    raise EJVCLException.CreateRes(@RsEControlNotFoundInGetImagePadding);
end;

function TJvErrorIndicator.IndexOf(AControl: TControl): Integer;
begin
  if AControl <> nil then
    for Result := 0 to Count - 1 do
      if Controls[Result].Control = AControl then
        Exit;
  Result := -1;
end;

procedure TJvErrorIndicator.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent is TControl then
      I := IndexOf(TControl(AComponent))
    else
      I := -1;
    if I > -1 then
      Delete(I);
    if AComponent = Images then
      Images := nil;
  end;
end;

procedure TJvErrorIndicator.SetBlinkRate(const Value: Integer);
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

procedure TJvErrorIndicator.SetBlinkStyle(const Value: TJvErrorBlinkStyle);
begin
  if FBlinkStyle <> Value then
  begin
    StopThread;
    FBlinkStyle := Value;
    UpdateControls;
  end;
end;

procedure TJvErrorIndicator.SetError(AControl: TControl;
  const Value: string);
var
  I: Integer;
  Ei: TJvErrorControl;
begin
  StopThread;
  if AControl = nil then
  begin
    if Value = '' then
      ClearErrors
    else
      for I := 0 to Count - 1 do
      begin
        Ei := Controls[I];
        if ((Ei.Error <> Value) and (BlinkStyle = ebsBlinkIfDifferentError)) or (BlinkStyle = ebsAlwaysBlink) then
          Ei.BlinkCount := cDefBlinkCount
        else
        if BlinkStyle = ebsNeverBlink then
          Ei.BlinkCount := 0;
        Ei.Error := Value;
      end;
  end
  else
  begin
    I := Add(AControl);
    if I > -1 then
    begin
      if Value = '' then
        Delete(I)
      else
      begin
        Ei := Controls[I];
        if ((Ei.Error <> Value) and (BlinkStyle = ebsBlinkIfDifferentError))
          or (BlinkStyle = ebsAlwaysBlink) then
        begin
          Ei.Error := Value;
          Ei.BlinkCount := cDefBlinkCount;
          Ei.Visible := (csDesigning in ComponentState);
          if (FUpdateCount = 0) and (FBlinkThread = nil) then
            StartThread;
        end
        else
        if BlinkStyle = ebsNeverBlink then
        begin
          Ei.BlinkCount := 0;
          Ei.Error := Value;
          Ei.Visible := (Value <> '');
        end;
      end;
      UpdateControls;
    end
    else
      raise EJVCLException.CreateRes(@RsEUnableToAddControlInSetError);
  end;
end;

procedure TJvErrorIndicator.SetImageAlignment(AControl: TControl;
  const Value: TJvErrorImageAlignment);
var
  I: Integer;
begin
  if AControl = nil then
    for I := 0 to Count - 1 do
      Controls[I].ImageAlignment := Value
  else
  begin
    I := Add(AControl);
    if I > -1 then
      Controls[I].ImageAlignment := Value
    else
      raise EJVCLException.CreateRes(@RsEUnableToAddControlInSetImageAlignme);
  end;
end;

procedure TJvErrorIndicator.SetImagePadding(AControl: TControl;
  const Value: Integer);
var
  I: Integer;
begin
  if AControl = nil then
    for I := 0 to Count - 1 do
      Controls[I].ImagePadding := Value
  else
  begin
    I := Add(AControl);
    if I > 1 then
      Controls[I].ImagePadding := Value
    else
      raise EJVCLException.CreateRes(@RsEUnableToAddControlInSetImagePadding);
  end;
end;

procedure TJvErrorIndicator.UpdateControls;
var
  I, J: Integer;
  IL: TCustomImageList;
begin
  if Images <> nil then
  begin
    IL := Images;
    J := ImageIndex;
  end
  else
  begin
    IL := FDefaultImage;
    J := 0;
  end;
  for I := 0 to Count - 1 do
  begin
    Controls[I].Images := IL;
    Controls[I].ImageIndex := J;
  end;
end;

procedure TJvErrorIndicator.SetImageList(const Value: TCustomImageList);
begin
  if FImageList <> Value then
  begin
    StopThread;
    if Assigned(FImageList) then
    begin
      FImageList.UnRegisterChanges(FChangeLink);
      FImageList.RemoveFreeNotification(Self);
    end;
    FImageList := Value;
    if Assigned(FImageList) then
    begin
      FImageList.RegisterChanges(FChangeLink);
      FImageList.FreeNotification(Self);
    end;
    UpdateControls;
  end;
end;

procedure TJvErrorIndicator.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    StopThread;
    FImageIndex := Value;
    UpdateControls;
  end;
end;

procedure TJvErrorIndicator.DoChangeLinkChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TJvErrorIndicator.ClearErrors;
var
  I: Integer;
begin
  StopThread;
  for I := 0 to Count - 1 do
    Controls[I].Free;
  FControls.Clear;
end;

procedure TJvErrorIndicator.BeginUpdate;
var
  I: Integer;
begin
  Inc(FUpdateCount);
  StopThread;
  for I := 0 to Count - 1 do
    Controls[I].Visible := False;
end;

procedure TJvErrorIndicator.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      UpdateControls;
      StartThread;
    end;
  end;
end;

procedure TJvErrorIndicator.StartThread;
begin
  FBlinkThread := TJvBlinkThread.Create(BlinkRate);
  TJvBlinkThread(FBlinkThread).OnBlink := DoBlink;
  TJvBlinkThread(FBlinkThread).Resume;
end;

procedure TJvErrorIndicator.StopThread;
begin
  if FBlinkThread <> nil then
    FBlinkThread.Terminate;
  FreeAndNil(FBlinkThread);
end;

procedure TJvErrorIndicator.DoBlink(Sender: TObject; Erase: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Controls[I].DrawImage(Erase);
end;

function TJvErrorIndicator.GetControl(Index: Integer): TJvErrorControl;
begin
  Result := TJvErrorControl(FControls[Index]);
end;

function TJvErrorIndicator.GetCount: Integer;
begin
  Result := FControls.Count;
end;

procedure TJvErrorIndicator.SetClientError(const AClient: IJvErrorIndicatorClient);
begin
  if AClient <> nil then
    SetError(AClient.getControl, AClient.ErrorMessage);
end;

procedure TJvErrorIndicator.IndicatorSetError(AControl: TControl;
  const ErrorMessage: WideString);
begin
  SetError(AControl, ErrorMessage);
end;

//=== TJvErrorControl ========================================================

constructor TJvErrorControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageAlignment := eiaMiddleRight;
  ShowHint := True;
  Visible := False;
  Width := 16;
  Height := 16;
end;

destructor TJvErrorControl.Destroy;
begin
  Control := nil;
  inherited Destroy;
end;

procedure TJvErrorControl.DrawImage(Erase: Boolean);
begin
  if not Assigned(Control) or not Assigned(Control.Parent) or not Assigned(Images) then
    Exit;
  Visible := (Error <> '') and (not Erase or (BlinkCount < 2));
  if not Visible and (BlinkCount > 1) then
    Dec(FBlinkCount);
  if Visible then
    BoundsRect := CalcBoundsRect;
end;

function TJvErrorControl.CalcBoundsRect: TRect;
begin
  if (Control = nil) or (Images = nil) then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    case ImageAlignment of
      eiaBottomLeft:
        begin
          // must qualify Result fully since Delphi confuses the TRect with the controls Top/Left properties
          Result.Right := Control.Left - 1;
          Result.Left := Result.Right - Images.Width;
          Result.Bottom := Control.Top + Control.Height;
          Result.Top := Result.Bottom - Images.Height;
          OffsetRect(Result, -ImagePadding, 0);
        end;
      eiaBottomRight:
        begin
          Result.Left := Control.Left + Control.Width + 1;
          Result.Right := Result.Left + Images.Width;
          Result.Bottom := Control.Top + Control.Height;
          Result.Top := Result.Bottom - Images.Height;
          OffsetRect(Result, ImagePadding, 0);
        end;
      eiaMiddleLeft:
        begin
          Result.Right := Control.Left - 1;
          Result.Left := Result.Right - Images.Width;
          Result.Top := Control.Top + (Control.Height - Images.Height) div 2;
          Result.Bottom := Result.Top + Images.Height;
          OffsetRect(Result, -ImagePadding, 0);
        end;
      eiaMiddleRight:
        begin
          Result.Left := Control.Left + Control.Width + 1;
          Result.Right := Result.Left + Images.Width;
          Result.Top := Control.Top + (Control.Height - Images.Height) div 2;
          Result.Bottom := Result.Top + Images.Height;
          OffsetRect(Result, ImagePadding, 0);
        end;
      eiaTopLeft:
        begin
          Result.Right := Control.Left - 1;
          Result.Left := Result.Right - Images.Width;
          Result.Top := Control.Top;
          Result.Bottom := Result.Top + Control.Height;
          OffsetRect(Result, -ImagePadding, 0);
        end;
      eiaTopRight:
        begin
          Result.Left := Control.Left + Control.Width + 1;
          Result.Right := Result.Left + Images.Width;
          Result.Top := Control.Top;
          Result.Bottom := Result.Top + Images.Height;
          OffsetRect(Result, ImagePadding, 0);
        end;
    end;
  end;
end;

procedure TJvErrorControl.Paint;
begin
  //  inherited Paint;
  if (Images <> nil) and Visible then
    {$IFDEF VCL}
    {$IFDEF COMPILER6_UP}
    Images.Draw(Canvas, 0, 0, ImageIndex, dsTransparent, itImage);
    {$ELSE}
    Images.Draw(Canvas, 0, 0, ImageIndex);
    {$ENDIF COMPILER6_UP}
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    Images.Draw(Canvas, 0, 0, ImageIndex);
    {$ENDIF VisualCLX}
end;

procedure TJvErrorControl.SetError(const Value: string);
begin
  Hint := Value;
end;

function TJvErrorControl.GetError: string;
begin
  Result := Hint;
end;

procedure TJvErrorControl.SetImageIndex(const Value: Integer);
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
      FControl.RemoveFreeNotification(Self);
    FControl := Value;
    if FControl <> nil then
    begin
      FControl.FreeNotification(Self);
      Parent := FControl.Parent;
    end
    else
      Parent := nil;
  end;
end;

procedure TJvErrorControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Control) then
    Control := nil;
end;

//=== TJvBlinkThread =========================================================

constructor TJvBlinkThread.Create(BlinkRate: Integer);
begin
  inherited Create(True);
  FBlinkRate := BlinkRate;
  FErase := False;
end;

procedure TJvBlinkThread.Blink;
begin
  if Assigned(FOnBlink) then
    FOnBlink(Self, FErase);
end;

procedure TJvBlinkThread.Execute;
begin
  FErase := False;
  while not Terminated and not Suspended do
  begin
    Sleep(FBlinkRate);
    Synchronize(Blink);
    if FBlinkRate = 0 then
      Exit;
    FErase := not FErase;
  end;
end;

end.

