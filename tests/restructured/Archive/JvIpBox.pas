{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvIpBox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvIpBox;

{*******************************************************}
{  Modifications:                                       }
{    1/11/2000  Corrected an exception on design time   }
{*******************************************************}

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Mask, Dialogs, JvTypes, JVCLVer;

type
  TJvIpBox = class(TCustomMaskEdit)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnCtl3DChanged: TNotiFyEvent;
    FSaved: TColor;
    FColor: TColor;
    FOver: Boolean;
    FEffect: Boolean;
    FAboutJVCL: TJVCLAboutInfo;
    procedure MouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure MouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SetCtl3d(Value: Boolean);
  protected
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ValidateEdit; override;
    function SaveToInt: Cardinal;
    procedure LoadFromInt(Value: Cardinal);
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property Borderstyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragKind;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property Hint;
    property ImeMode;
    property ImeName;
    property Left;
    property MaxLength;
    property Name;
    property ParentBiDiMode;
    property ParentCtl3d;
    property ParentFont;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    property Visible;
    property Width;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property HotTrack: Boolean read FEffect write SetCtl3d default False;
  end;

implementation

type
  TIpBytes = record
    case Byte of
      0:
      (IpAsInt: Cardinal);
      1:
      (Part1: Byte;
        Part2: Byte;
        Part3: Byte;
        Part4: Byte);
  end;

  {***********************************************}

constructor TJvIpBox.Create(AOwner: TComponent);
begin
  inherited;
  FEffect := False;
  FColor := clInfoBk;
  FOver := False;
  EditMask := '!990.990.990.990;1;0';
end;

{***********************************************}

procedure TJvIpBox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvIpBox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvIpBox.MouseEnter(var Msg: TMessage);
begin
  FOver := True;
  FSaved := Application.HintColor;
  Application.HintColor := FColor;
  if FEffect then
    Ctl3d := True;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvIpBox.MouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  FOver := False;
  if FEffect then
    Ctl3d := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvIpBox.SetCtl3d(Value: Boolean);
begin
  FEffect := Value;
  if Value then
    Ctl3d := False;
end;

{**************************************************}

procedure TJvIpBox.KeyPress(var Key: Char);
begin
  case SelStart of
    0, 4, 8, 12:
      if Key in ['3'..'9'] then
      begin
        Key := #0;
        Beep;
      end
      else
        inherited;
    1, 5, 9, 13:
      if (Text[SelStart] = '2') and (Key in ['6'..'9']) then
      begin
        Key := #0;
        Beep;
      end
      else
        inherited;
    2, 6, 10, 14:
      if (Text[SelStart - 1] = '2') and (Text[SelStart] = '5') and (Key in ['6'..'9']) then
      begin
        Key := #0;
        Beep;
      end
      else
        inherited;
  else
    inherited;
  end;
end;

{**************************************************}

procedure TJvIpBox.ValidateEdit;
begin
  //To avoid some problems !
  //Do not inherit this one !
end;

{**************************************************}

procedure TJvIpBox.LoadFromInt(Value: Cardinal);
var
  IpBytes: TIpBytes;
begin
  IpBytes.IpAsInt := Value;
  Text := Format('%3d.%3d.%3d.%3d', [IpBytes.Part4, IpBytes.Part3, IpBytes.Part2,
    IpBytes.Part1]);
end;

{**************************************************}

function TJvIpBox.SaveToInt: Cardinal;
var
  IpBytes: TIpBytes;
begin
  IpBytes.Part4 := StrToInt(Copy(EditText, 1, 3));
  IpBytes.Part3 := StrToInt(Copy(EditText, 5, 3));
  IpBytes.Part2 := StrToInt(Copy(EditText, 9, 3));
  IpBytes.Part1 := StrToInt(Copy(EditText, 13, 3));

  Result := IpBytes.IpAsInt;
end;

end.
