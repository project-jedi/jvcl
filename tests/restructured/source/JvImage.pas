{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImage.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvImage;

interface



uses
  Windows, Messages, SysUtils, Classes, Graphics, ExtCtrls, Controls, Forms, JVCLVer;

type
  TPicState = (stDefault, stEntered, stClicked1, stClicked2, stDown);
{$EXTERNALSYM TPicState}

  TJvPictures = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
    FPictureClicked1: TPicture;
    FPictureClicked2: TPicture;
    FPictureDown: TPicture;
    FPictureEnter: TPicture;
    procedure SetPictureCLicked(const Value: TPicture);
    procedure SetPictureCLicked2(const Value: TPicture);
    procedure SetPictureDown(const Value: TPicture);
    procedure SetPictureEnter(const Value: TPicture);
  protected
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property PicEnter: TPicture read FPictureEnter write SetPictureEnter;
    property PicClicked1: TPicture read FPictureClicked1 write SetPictureCLicked;
    property PicClicked2: TPicture read FPictureClicked2 write SetPictureCLicked2;
    property PicDown: TPicture read FPictureDown write SetPictureDown;
  end;

  TJvImage = class(TImage)
  private
    FColor: TColor;
    FSaved: TColor;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnStateChanged: TNotifyEvent;
    FPictures: TJvPictures;
    FState: TPicState;
    FOver: Boolean;
    FPicture: TPicture;
    FClickCount: Integer;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetState(Value: TPicState);
    procedure PicturesChanged(Sender: TObject);
    procedure SetPicture(const Value: TPicture);
    procedure ApplyClick;
  protected
    procedure Click; override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property Pictures: TJvPictures read FPictures write FPictures;
    property Picture: TPicture read FPicture write SetPicture;
    property State: TPicState read FState write SetState default stDefault;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvImage
///////////////////////////////////////////////////////////

procedure TJvImage.ApplyClick;
begin
  case FClickCount of
    1:
      begin
        State := stClicked1;
        if State <> stClicked1 then
        begin
          FClickCount := 0;
          State := stDefault;
        end;
      end;
    2:
      begin
        State := stClicked2;
        if State <> stClicked2 then
        begin
          FClickCount := 0;
          State := stDefault;
        end;
      end;
    0, 3:
      begin
        State := stDefault;
        FClickCount := 0;
      end;
  end;
end;

{**************************************************}

procedure TJvImage.Click;
begin
  inherited;
  Inc(FClickCount);
  ApplyClick;
end;

{**************************************************}

constructor TJvImage.Create(AOwner: TComponent);
begin
  inherited;
  FColor := clInfoBk;
  FState := stDefault;
  FOver := False;
  FPictures := TJvPictures.Create;
  FPictures.OnChanged := PicturesChanged;
  FPicture := TPicture.Create;
end;

{**************************************************}

destructor TJvImage.Destroy;
begin
  FPictures.Free;
  FPicture.Free;
  inherited;
end;

{**************************************************}

procedure TJvImage.Loaded;
begin
  inherited Picture.Assign(FPicture);
end;

{**************************************************}

procedure TJvImage.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  State := stDown;
end;

{**************************************************}

procedure TJvImage.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    State := stEntered;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;
{**************************************************}

procedure TJvImage.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if FOver then
  begin
    Application.HintColor := FSaved;
    ApplyClick;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvImage.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if (State = stClicked1) or (State = stClicked2) then
    Exit;
  if (X > 0) and (X < Width) and (Y > 0) and (Y < Height) then
  begin
    SetState(stEntered);
    if State <> stEntered then
      ApplyClick;
  end
  else
    ApplyClick;
end;

{**************************************************}

procedure TJvImage.PicturesChanged(Sender: TObject);
begin
  SetState(State);
end;

{**************************************************}

procedure TJvImage.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
  inherited Picture.Assign(Value);
end;

{**************************************************}

procedure TJvImage.SetState(Value: TPicState);

  function NotEmpty(Value: TPicture): Boolean;
  begin
    Result := (Value <> nil) and (Value.Width > 0) and (Value.Height > 0);
  end;

begin
  case Value of
    stDefault:
      if NotEmpty(FPicture) then
      begin
        inherited Picture.Assign(FPicture);
        FState := Value;
      end;
    stEntered:
      if NotEmpty(Pictures.PicEnter) then
      begin
        inherited Picture.Assign(Pictures.PicEnter);
        FState := Value;
      end;
    stClicked1:
      if NotEmpty(Pictures.PicClicked1) then
      begin
        inherited Picture.Assign(Pictures.PicClicked1);
        FState := Value;
      end;
    stClicked2:
      if NotEmpty(Pictures.PicClicked2) then
      begin
        inherited Picture.Assign(Pictures.PicClicked2);
        FState := Value;
      end;
    stDown:
      if NotEmpty(Pictures.PicDown) then
      begin
        inherited Picture.Assign(Pictures.PicDown);
        FState := Value;
      end;
  end;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

///////////////////////////////////////////////////////////
// TJvPictures
///////////////////////////////////////////////////////////

procedure TJvPictures.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{*******************************************************}

constructor TJvPictures.Create;
begin
  FPictureClicked1 := TPicture.Create;
  FPictureClicked2 := TPicture.Create;
  FPictureDown := TPicture.Create;
  FPictureEnter := TPicture.Create;
end;

{*******************************************************}

destructor TJvPictures.Destroy;
begin
  FPictureClicked1.Free;
  FPictureClicked2.Free;
  FPictureDown.Free;
  FPictureEnter.Free;
  inherited;
end;

{*******************************************************}

procedure TJvPictures.SetPictureCLicked(const Value: TPicture);
begin
  FPictureClicked1.Assign(Value);
  Changed;
end;

{*******************************************************}

procedure TJvPictures.SetPictureCLicked2(const Value: TPicture);
begin
  FPictureClicked2.Assign(Value);
  Changed;
end;

{*******************************************************}

procedure TJvPictures.SetPictureDown(const Value: TPicture);
begin
  FPictureDown.Assign(Value);
  Changed;
end;

{*******************************************************}

procedure TJvPictures.SetPictureEnter(const Value: TPicture);
begin
  FPictureEnter.Assign(Value);
  Changed;
end;

end.
