{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{$I jvcl.inc}

unit OLBarMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvOutlookBar, ComCtrls, StdCtrls, Menus, ExtCtrls, ImgList,
  JvComponent, JvExControls, ActnList, JvGIF, jpeg;

type
  TOLBarMainForm = class(TForm)
    popOL: TPopupMenu;
    Splitter1: TSplitter;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Defaultpopupmenu1: TMenuItem;
    popButton: TPopupMenu;
    popPage: TPopupMenu;
    Editbuttoncaption1: TMenuItem;
    Editpagecaption1: TMenuItem;
    StatusBar1: TStatusBar;
    JvOutlookBar1: TJvOutlookBar;
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    chkSmallImages: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    chkButtonFont: TCheckBox;
    RichEdit1: TRichEdit;
    Smallbuttons1: TMenuItem;
    Smallbuttons2: TMenuItem;
    ActionList1: TActionList;
    acSmallButtons: TAction;
    acEditButtonCaption: TAction;
    acEditPageCaption: TAction;
    chkFlat: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure JvOutlookBar1ButtonClick(Sender: TObject; Index: Integer);
    procedure JvOutlookBar1PageChanging(Sender: TObject; Index: Integer;
      var AllowChange: Boolean);
    procedure JvOutlookBar1PageChange(Sender: TObject; Index: Integer);
    procedure JvOutlookBar1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure acSmallButtonsExecute(Sender: TObject);
    procedure acEditButtonCaptionExecute(Sender: TObject);
    procedure acEditPageCaptionExecute(Sender: TObject);
    procedure chkFlatClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  end;

var
  OLBarMainForm: TOLBarMainForm;

implementation

{$R *.dfm}

procedure TOLBarMainForm.Button1Click(Sender: TObject);
begin
  JvOutlookBar1.LargeImages := ImageList1;
  JvOutlookBar1.SmallImages := ImageList2;
end;

procedure TOLBarMainForm.Button2Click(Sender: TObject);
begin
  JvOutlookBar1.LargeImages := nil;
  JvOutlookBar1.SmallImages := nil;
end;

procedure TOLBarMainForm.Button3Click(Sender: TObject);
var i:integer; FD:TFontDialog;
begin
  FD := TFontDialog.Create(nil);
  try
    if not chkButtonFont.Checked then
      FD.Font := JvOutlookBar1.Font
    else
      FD.Font := JvOutlookBar1.ActivePage.Font;
    if FD.Execute then
    begin
      if not chkButtonFont.Checked then
        JvOutlookBar1.Font := FD.Font // this sets the font of all the pages
      else
        for i := 0 to JvOutlookBar1.Pages.Count - 1 do
          JvOutlookBar1.Pages[i].Font := FD.Font; // this sets the button's fonts!
    end;
  finally
    FD.Free;
  end;
end;

procedure TOLBarMainForm.JvOutlookBar1ButtonClick(Sender: TObject;
  Index: Integer);
var P:TJvOutlookBarPage;
begin
  if (Index > -1) then
  with JvOutlookBar1 do
  begin
    P := Pages[ActivePageIndex];
    Caption := Format('Clicked button "%s" on page "%s"',[P.Buttons[Index].Caption,P.Caption]);
  end;
end;

procedure TOLBarMainForm.JvOutlookBar1PageChanging(Sender: TObject;
  Index: Integer; var AllowChange: Boolean);
begin
  with JvOutlookBar1 do
    if (ActivePageIndex > -1) and (Index > -1) then
      Caption := Format('Page changing from "%s" to "%s"',
        [Pages[ActivePageIndex].Caption, Pages[Index].Caption]);
end;

procedure TOLBarMainForm.JvOutlookBar1PageChange(Sender: TObject;
  Index: Integer);
begin
  if Index > -1 then
    Caption := Format('Page changed to "%s"',[JvOutlookBar1.Pages[Index].Caption]);
end;

procedure TOLBarMainForm.JvOutlookBar1ContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  with Sender as TJvOutlookbar do
  if PopupObject is TJvOutlookBarPage then
    PopUpMenu := popPage
  else if PopupObject is TJvOutlookBarButton then
    PopUpMenu := popButton
  else
    PopUpMenu := popOL;
end;

procedure TOLBarMainForm.acSmallButtonsExecute(Sender: TObject);
const
  cButtonSize: array[boolean] of TJvBarButtonSize = (olbsLarge,olbsSmall);
begin
  acSmallButtons.Checked := not acSmallButtons.Checked;
  JvOutlookBar1.ButtonSize := cButtonSize[acSmallButtons.Checked];

end;

procedure TOLBarMainForm.acEditButtonCaptionExecute(Sender: TObject);
begin
  with JvOutlookBar1.PopUpObject as TJvOutlookBarButton do
    EditCaption;
end;

procedure TOLBarMainForm.acEditPageCaptionExecute(Sender: TObject);
begin
  with JvOutlookBar1.PopUpObject as TJvOutlookBarPage do
    EditCaption;
end;

procedure TOLBarMainForm.chkFlatClick(Sender: TObject);
const
  cBorderStyle:array [boolean] of TBorderStyle = (bsSingle, bsNone);
begin
  JvOutlookbar1.BorderStyle := cBorderStyle[chkFlat.Checked];
end;

procedure TOLBarMainForm.FormCreate(Sender: TObject);
begin
  RichEdit1.Wordwrap := True;
end;

end.
