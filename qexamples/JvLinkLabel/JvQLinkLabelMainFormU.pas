{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Main.pas, released 2002-01-05.

The Initial Developer of the Original Code is David Polberger <dpol@swipnet.se>
Portions created by David Polberger are Copyright (C) 2002 David Polberger.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2002-01-05;
Current Version: 1.00

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
  Please see the accompanying documentation.
Description:
  This is a small demo application, showing off some of the features of the
  TJvLinkLabel. It's not as well-written as I would've liked, and could've been
  written in a more object-oriented fashion. If you improve it, please send it
  to me, and I'll try to include it in the next release.
-----------------------------------------------------------------------------}

unit JvQLinkLabelMainFormU;

interface

uses
  Qt, QWindows, QMessages, SysUtils, Classes, Types, QGraphics, QControls, QForms, QDialogs,
  CategCh, QStdCtrls, QExtCtrls, JvQLinkLabel, JvQComponent, JvQExControls,
  QComCtrlsEx;

type
  TJvLinkLabelMainForm = class(TForm)
    imgLogo: TImage;
    lblHeading: TLabel;
    LinkLabel: TJvLinkLabel;
    lblClose: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure lblCloseClick(Sender: TObject);
    procedure LinkLabelLinkClick(Sender: TObject; LinkNumber: Integer; LinkText: String; LinkParam: string);
  private
    FChooser: TJvCategoryChooser;
    procedure ChooserCatChange(Sender: TObject);
    function IsWithinBounds(Category: Integer): Boolean;
//    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
  public
//    procedure CreateParams(var Params: TCreateParams); override;
  end;

var
  JvLinkLabelMainForm: TJvLinkLabelMainForm;

implementation

{$R *.xfm}

uses
  InfoStrings, JvQLinkLabelTools, Play;

procedure TJvLinkLabelMainForm.ChooserCatChange(Sender: TObject);
begin
  with FChooser do
    if IsWithinBounds(SelectedCat) then
    begin
      lblHeading.Caption := Trim(CatList[SelectedCat]);
      LinkLabel.Caption  := Info[SelectedCat];
    end;
end;
(*
procedure TJvLinkLabelMainForm.CreateParams(var Params: TCreateParams);
begin
  { We should really create a new TCustomForm-descendant, called
    TMovableBorderForm or something like that, that we'd inherit from, instead
    of placing all this nasty Windows-specific code directly in our main form.
    However, that would probably cause trouble with the form designer, and we'd
    wind up with creating everything dynamically. Visual form inheritance could
    probably help us out, but this is just a demo application, and we'd just be
    complicating matters unnecessarily. }
   inherited CreateParams(Params);
   with Params do
     Style := (Style or WS_POPUP or WS_BORDER) and not WS_DLGFRAME;
end;
*)
procedure TJvLinkLabelMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  { We create the TCategoryChooser dynamically, so that the user doesn't need
    to install it. }
  FChooser := TJvCategoryChooser.Create(Self);
  FChooser.SetBounds(0, 109, 145, 324);
  FChooser.OnCatChange := ChooserCatChange;
  for I := Low(Headings) to High(Headings) do
    FChooser.CatList.Add(Headings[I]);
  FChooser.Parent := Self;

  ChooserCatChange(Self);
  Application.Palette.SetColor(cgActive, crHighLightText, clRed);
end;

function TJvLinkLabelMainForm.IsWithinBounds(Category: Integer): Boolean;
begin
  Result := Category in [Low(Info)..High(Info)];
end;

(*
procedure TJvLinkLabelMainForm.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  inherited;
  if (ScreenToClient(Mouse.CursorPos).Y < imgLogo.Top + imgLogo.Height) and
    (ScreenToClient(Mouse.CursorPos).X < lblClose.Left) then
    Msg.Result := HTCAPTION;
end;
*)

procedure TJvLinkLabelMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 { Esc } then
  begin
    Close;
    Key := #0;
  end;
end;

procedure TJvLinkLabelMainForm.lblCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TJvLinkLabelMainForm.LinkLabelLinkClick(Sender: TObject; LinkNumber: Integer;
  LinkText: String; LinkParam: string);
var
  frmPlay: TfrmPlay;

  procedure GoToNextCategory(Number: Integer); overload;
  begin
    // This really belongs in the TJvCategoryChooser itself
    FChooser.SelectedCat := FChooser.SelectedCat + Number;
  end;

  procedure GoToNextCategory; overload;
  begin
    GoToNextCategory(1);
  end;

  procedure DisplayLink;
  begin
    ShowMessage('This link is identified by the number ' +
      IntToStr(LinkNumber) + ', and contains the following text: "' +
      LinkText + '".');
  end;

begin
  case FChooser.SelectedCat of
    0:
      case LinkNumber of
        0: TWebTools.OpenWebPage('http://www.mozilla.org/MPL/MPL-1.1.html');
        1: GoToNextCategory;
      end;
    1:
      case LinkNumber of
        0: DisplayLink;
        1: GoToNextCategory;
      end;
    2:
      case LinkNumber of
        0: GoToNextCategory;
        1: GoToNextCategory(2);
      end;
    5:
      begin
        frmPlay := TfrmPlay.Create(nil);
        try
          frmPlay.ShowModal;
        finally
          frmPlay.Free;
        end;
      end;
    6:
      GoToNextCategory;
  end;
end;

end.
